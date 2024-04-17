source("PubChem_API.R")

remove.cas.numbers <- function(text.field) {
    cas.numbers <- unlist(strsplit(text.field, split=" "))
    if (isCAS(cas.numbers)) {
        cas.numbers <- unique(cas.numbers)
        cas.numbers.prefix <- paste("CAS_", cas.numbers, sep="")
        if (isInKnowledgeBase(cas.numbers.prefix)) {
            save.knowledge.base(knowledge.base, overwrite=FALSE)
            knowledge.base <- knowledge.base[!(names(knowledge.base) %in% cas.numbers.prefix)]
            save.knowledge.base(knowledge.base, overwrite=TRUE)
            message <- paste("Removed ", length(cas.numbers), " CAS registry number(s): ",
                             paste(cas.numbers, collapse=", "),
                             sep=""
                             )
            knowledge.base.commit.logs(message)
            notification("success.n.removal", length(cas.numbers))
        } else notification("input.not.present")
    } else notification("not.cas")
}

add.single.cas.number <- function(text.field) {
    if (isCAS(text.field)) {
        cas.prefix <- paste("CAS_", text.field, sep="")
        if (!isInKnowledgeBase(cas.prefix)) {
            cid <- cas2cid(text.field) # 2 NCBI queries
            if (!is.na(cid) && length(cid) == 1) {
                save.knowledge.base(knowledge.base, overwrite=FALSE)
                knowledge.base[[cas.prefix]] <- get.entry.info(cid, text.field)
                save.knowledge.base(knowledge.base, overwrite=TRUE)
                message <- paste("Added 1 CAS registry number: ", text.field, sep="")
                knowledge.base.commit.logs(message)
                notification("success.one.addition")
            } else {
                if (is.na(cid) || length(cid) == 0) notification("no.cid")
                if (length(cid) > 1) notification("too.many.cid", length(cid))
            }
        } else notification("input.already.present")
    } else notification("single.cas")
}

get.entry.info <- function(cid, cas) {
    properties <- getPropertiesFromCID(cid)
    smiles <- properties$CanonicalSMILES
    
    common_name <- getCommonName(cid)
    Sys.sleep(1)
    LRI_polar <- getLRI(cid, type="polar", canonicalSMILES=smiles)
    LRI_nonpolar <- getLRI(cid, type="non-polar", canonicalSMILES=smiles)
    odor_pubchem <- getPubchemDescriptors(cid, type="Odor")
    taste_pubchem <- getPubchemDescriptors(cid, type="Taste")
    Sys.sleep(1)
    
    entry <- list(
        "CAS" = cas,
        "CID" = cid,
        "IUPAC_name" = properties$IUPACName,
        "canonical_SMILES" = smiles,
        "common_name" = common_name,
        "molecular_weight_g.mol-1" = properties$MolecularWeight,
        "XLogP" = properties$XLogP,
        "LRI_polar" = LRI_polar,
        "LRI_nonpolar" = LRI_nonpolar,
        "odor_pubchem" = odor_pubchem,
        "taste_pubchem" = taste_pubchem,
        "odor_goodscents" = getGoodScentsOdorDescriptors(cas),
        "odor_flavornet" = getFlavornetOdorDescriptors(cid)
    )
    return(entry)
}

generate.prefilled.template <- function(text.field) {
    if (tolower(text.field) == "empty" || text.field == "") return(empty.template())
    if (tolower(text.field) == "all") return(knowledge.base.to.dataframe(knowledge.base))
    
    cas.numbers <- unlist(strsplit(text.field, split=" "))
    if (isCAS(cas.numbers)) {
        cas.numbers <- unique(cas.numbers)
        cas.prefix <- paste("CAS_", cas.numbers, sep="")
        query <- knowledge.base[names(knowledge.base) %in% cas.prefix]
        query <- knowledge.base.to.dataframe(query)
        return(query)
    }
    notification("not.cas")
    return(knowledge.base.to.dataframe(knowledge.base))
}

empty.template <- function() {
    header <- names(knowledge.base[[1]])
    df <- as.data.frame(matrix(nrow=0, ncol=length(header)))
    colnames(df) <- header
    return(df)
}

# TODO what to display on the template file: the previous info or the flag
# TODO type of non-str values
# TODO checking separators for vector-like structures

edit.with.template <- function(df) {
    # Checking colnames
    header = colnames(empty.template())
    missing_columns <- setdiff(header, colnames(df))
    
    # Tracking new or edited entries
    new.entry.list <- list()
    edited.entry.list <- list()
    
    if (length(missing_columns) == 0) {
        for (row in 1:nrow(df)) {
            cas <- df[row, "CAS"]
            if (isCAS(cas)) {
                cas.prefix <- paste("CAS_", cas, sep="")
                if (isInKnowledgeBase(cas.prefix)) {
                    edited.entry <- template.to.entry(df[row, ], "edit.existing")
                    edited.entry.list[[cas.prefix]] <- edited.entry
                } else {
                    new.entry <- template.to.entry(df[row, ], "add")
                    new.entry.list[[cas.prefix]] <- new.entry
                }
            }
        }
        all.entries <- list(new.entry.list, edited.entry.list)
        save.knowledge.base(knowledge.base, overwrite=FALSE)
        for (i in 1:length(all.entries)) {
            knowledge.base[names(all.entries[[i]])] <- all.entries[[i]]
        }
        save.knowledge.base(knowledge.base, overwrite=TRUE)
        message1 <- paste("Edited ", length(edited.entry.list), " existing entry(ies): ",
                         paste(gsub("CAS_", "", names(edited.entry.list)), collapse=", "),
                         sep=""
        )
        message2 <- paste("Added ", length(new.entry.list), " new entry(ies): ",
                          paste(gsub("CAS_", "", names(new.entry.list)), collapse=", "),
                          sep=""
        )
        knowledge.base.commit.logs(message1, message2)
        notification("success.template.edition", length(all.entries))
    } else notification("template.wrong.colnames")
}

template.to.entry <- function(df, type=c("edit.existing", "add")) {
    # Identify each type of column:
    ## CID is integer
    ## LRIs, molecular weight, and XLogP are double
    ## Rest is characters
    header <- colnames(empty.template())
    vector.like.col <- c("LRI_polar", "LRI_nonpolar")
    col <- header[!(header %in% vector.like.col)]
    
    flag <- "keep.previous" # flag to keep the previous value
    entry <- list()
    cas.prefix <- paste("CAS_", df[1, "CAS"], sep="")
    
    for (attribute in header) {
        current.cell <- df[1, attribute]
        if (type == "edit.existing" && (tolower(current.cell) == flag || is.na(current.cell))) {
            entry[[attribute]] <- knowledge.base[[cas.prefix]][[attribute]]
        } else {
            if (attribute %in% col) entry[[attribute]] <- current.cell
            if (attribute %in% vector.like.col) {
                entry[[attribute]] <- as.double(unlist(strsplit(current.cell, split=";")))
            }
        }
    }
    return(entry)
}

################################################################################
# Verification functions: isCAS, isInKnowledgeBase, notification
################################################################################

# Check whether the CAS numbers follow the below regex
isCAS <- function(cas.vector) {
    return(all(grepl(cas.vector, pattern="^\\d{2,7}-\\d{2}-\\d$")))
}

# Check whether the CAS numbers are present in the knowledge base
# Does not append the prefix CAS_
isInKnowledgeBase <- function(cas.vector) {
    queriedCASinKnowledgeBase <- length(knowledge.base[names(knowledge.base) %in% cas.vector])
    if (queriedCASinKnowledgeBase < length(cas.vector)) return(FALSE)
    return(TRUE)
}

notification <- function(type, number=NA) {
    switch (type,
        "input.not.present" = showNotification("Input not present in the knowledge base", type="error"),
        "input.already.present" = showNotification("Input already present in the knowledge base", type="warning"),
        "not.cas" = showNotification("Input must contain CAS registry number(s)", type="error"),
        "single.cas" = showNotification("Input must be a single CAS registry number", type="error"),
        "too.many.cid" = showNotification(paste("Entry cannot be added: ", number,
                                                " PubChem CIDs returned instead of one. Choose another method.",
                                                sep=""),
                                          type="error"),
        "no.cid" = showNotification("Entry cannot be added: impossible to convert the input into PubChem CID", type="error"),
        "success.one.addition" = showNotification("Succesfully added 1 CAS registry number", type="message"),
        "success.n.removal" = showNotification(paste("Succesfully removed ", number, " CAS registry numbers(s)",
                                                     sep=""),
                                               type="message"),
        "template.wrong.colnames" = showNotification("Input file has different column names than expected.", type="error"),
        "success.template.edition" = showNotification(paste("Succesfully edited ", number, " entry(ies)", sep=""), type="message")
    )
}

################################################################################
# Save, reload, convert, and logs functions
################################################################################

save.knowledge.base <- function(kb, overwrite) {
    # Add a max number of previous knowledge bases?
    if (overwrite) {
        knowledge.base <- kb # This allows to save the changes in the kb
        save(knowledge.base, file=rdata.file)
        load(rdata.file, envir=.GlobalEnv)
    } else {
        kb.dir <- "previous_compound_knowledge_bases"
        if (!dir.exists(kb.dir)) dir.create(kb.dir)
        time <- gsub(" ", "_", Sys.time())
        save(knowledge.base, file=paste(kb.dir, "/", time, "_", rdata.file, sep=""))
    }
}

bash.append.from.bottom.to.top <- function(message, file) {
    # append message from bottom to top of a file
    return(paste("echo '", message, "' | cat - ", file, " > temp && mv temp ", file, sep=""))
}

knowledge.base.commit.logs <- function(message1=NA, message2=NA) {
    logfile <- "knowledge_base_commit.log"
    if (!file.exists(logfile)) {
        system(paste("echo ", Sys.time(), " > ", logfile, sep=""))
        system(paste("echo 'Compound knowledge base first version' >> ", logfile, sep=""))
    }
    
    if (!is.na(message1)) {
        system(bash.append.from.bottom.to.top(message="", file=logfile))
        system(bash.append.from.bottom.to.top(message=message1, file=logfile))
        if (!is.na(message2)) system(bash.append.from.bottom.to.top(message=message2, file=logfile))
        system(bash.append.from.bottom.to.top(message=Sys.time(), file=logfile))
    }
}

convert_to_string <- function(list_vec) {
    # Convert each vector to a string and concatenate with ";"
    str <- paste(unlist(list_vec), collapse = ";")
    return(str)
}

convert_list_column <- function(df, column_name) {
    df[[column_name]] <- apply(df, 1, function(row) {
        convert_to_string(row[[column_name]])
    })
    return(df)
}

knowledge.base.to.dataframe <- function(list.of.lists) {
    # Load and convert knowledge base to dataframe
    # Separators are semi-columns for vector-like data structures
    df <- as.data.frame(do.call(rbind, list.of.lists))
    
    # Change types
    col <- colnames(df)
    col <- col[!col %in%  c("CID_all", "LRI_polar", "LRI_nonpolar")]
    df[col] <- lapply(df[col], unlist)
    df <- convert_list_column(df, "CID_all") 
    df <- convert_list_column(df, "LRI_polar")
    df <- convert_list_column(df, "LRI_nonpolar")
    return(df)
}