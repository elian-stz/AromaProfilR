# CAS deletion feature----------------------------------------------------------
remove.cas.numbers <- function(CAS) {
    CASvector <- strsplit(CAS, split=" ")[[1]]
    CASvector <- unique(CASvector)
    if (all(sapply(CASvector, isCAS))) {
        CASprefix <- addPrefix(CASvector) 
        if (all(sapply(CASprefix, isInKnowledgeBase))) {
            save.knowledge.base(knowledge.base, overwrite=FALSE)
            knowledge.base <- knowledge.base[!(names(knowledge.base) %in% CASprefix)]
            save.knowledge.base(knowledge.base, overwrite=TRUE)
            message <- paste("Removed ", length(CASvector), " CAS registry number(s): ",
                             paste(CASvector, collapse=", "),
                             sep=""
                             )
            knowledge.base.commit.logs(message)
            notification("success.n.removal", length(CASvector))
        } else notification("input.not.present")
    } else notification("not.cas")
}

# CAS addition feature----------------------------------------------------------
add.single.cas.number <- function(CAS, chemicalFamily) {
    if (isCAS(CAS)) {
        cas.prefix <- addPrefix(CAS)
        if (!isInKnowledgeBase(cas.prefix)) {
            cid <- cas2cid(CAS) # 2 NCBI queries
            if (length(cid) == 1 && !is.na(cid)) {
                save.knowledge.base(knowledge.base, overwrite=FALSE)
                knowledge.base[[cas.prefix]] <- get.entry.info(cid, CAS, chemicalFamily)
                save.knowledge.base(knowledge.base, overwrite=TRUE)
                message <- paste("Added 1 CAS registry number: ", CAS, sep="")
                knowledge.base.commit.logs(message)
                notification("success.one.addition")
            } else {
                if (is.na(cid) || length(cid) == 0) notification("no.cid")
                if (length(cid) > 1) notification("too.many.cid", length(cid))
            }
        } else notification("input.already.present")
    } else notification("single.cas")
}

get.entry.info <- function(cid, cas, chemicalFamily) {
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
        "odor_flavornet" = getFlavornetOdorDescriptors(cid),
        "family" = chemicalFamily
    )
    return(entry)
}

# Template generation feature---------------------------------------------------
generate.prefilled.template <- function(text.field) {
    if (tolower(text.field) == "empty" || text.field == "") return(empty.template())
    if (tolower(text.field) == "all") return(knowledge.base.to.dataframe(knowledge.base))
    
    cas.numbers <- unlist(strsplit(text.field, split=" "))
    if (all(sapply(cas.numbers, isCAS))) {
        cas.numbers <- unique(cas.numbers)
        cas.prefix <- addPrefix(cas.numbers)
        query <- knowledge.base[names(knowledge.base) %in% cas.prefix]
        if (length(query) < 1) return(empty.template())
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

# Knowledge base edition with template feature----------------------------------
# TODO what to display on the template file: the previous info or the flag
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
                cas.prefix <- addPrefix(cas) 
                if (isInKnowledgeBase(cas.prefix)) {
                    edited.entry <- template.to.entry(df[row, ], "edit.existing")
                    edited.entry.list[[cas.prefix]] <- edited.entry
                } else {
                    new.entry <- template.to.entry(df[row, ], "add")
                    new.entry.list[[cas.prefix]] <- new.entry
                }
            }
        }
        save.knowledge.base(knowledge.base, overwrite=FALSE)
        if (length(new.entry.list) != 0) {
            for (i in 1:length(new.entry.list)) {
                name <- names(new.entry.list[i])
                knowledge.base[[name]] <- new.entry.list[[i]]
            }
        }
        if (length(edit.with.template) != 0) {
            for (i in 1:length(edited.entry.list)) {
                name <- names(edited.entry.list[i])
                knowledge.base[[name]] <- edited.entry.list[[i]]
            }
        }
        save.knowledge.base(knowledge.base, overwrite=TRUE)
        message1 <- paste("Edited ", length(edited.entry.list), " existing entry(ies): ",
                         paste(removePrefix(names(edited.entry.list)), collapse=", "),
                         sep=""
        )
        message2 <- paste("Added ", length(new.entry.list), " new entry(ies): ",
                          paste(removePrefix(names(new.entry.list)), collapse=", "),
                          sep=""
        )
        knowledge.base.commit.logs(message1, message2)
        notification("success.template.edition", length(new.entry.list) + length(edited.entry.list))
    } else notification("template.wrong.colnames")
}

template.to.entry <- function(df, type=c("edit.existing", "add")) {
    # Identify each column column type
    ## CID is integer
    ## LRI_polar and LRI_nonpolar are vector of doubles
    ## molecular_weight_g.mol-1 and XLogP are double
    ## Rest is characters
    header    <- colnames(empty.template())
    LRICol    <- c("LRI_polar", "LRI_nonpolar")
    doubleCol <- c("molecular_weight_g.mol-1", "XLogP")
    intCol    <- c("CID")
    col       <- header[!(header %in% c(LRICol, doubleCol, intCol))]
    
    flag <- "keep.previous" # flag to keep the previous value
    entry <- list()
    
    # Assign each cell of the template to the entry object
    for (attribute in header) {
        cell <- df[1, attribute]
        if ((is.na(cell) || tolower(cell) == flag) && type == "edit.existing") {
            cas.prefix <- addPrefix(df[1, "CAS"])
            entry[[attribute]] <- knowledge.base[[cas.prefix]][[attribute]]
        } else {
            if (attribute %in% col) entry[[attribute]] <- cell
            if (attribute %in% doubleCol) entry[[attribute]] <- convertType(cell, "double")
            if (attribute %in% intCol) entry[[attribute]] <- convertType(cell, "int")
            if (attribute %in% LRICol) entry[[attribute]] <- convertType(cell, "LRI")
        }
    }
    return(entry)
}

convertType <- function(cell, conversion=c("LRI", "double", "int")) {
    if (!is.character(cell)) return(cell)
  
    if (conversion == "int") return(as.integer(cell))
    if (conversion == "double") {
        cell <- gsub(",", ".", cell)
        return(as.double(cell))
    }
    if (conversion == "LRI") {
        cell <- strsplit(cell, ";")[[1]]
        cell <- gsub(",", ".", cell)
        cell <- as.double(cell)
        return(cell)
    }
    return(cell)
}

# Knowledge base saving feature-------------------------------------------------
save.knowledge.base <- function(kb, overwrite=FALSE) {
    # Add a max number of previous knowledge bases?
    if (overwrite) {
        knowledge.base <- kb # This allows to save the changes in the kb
        saveRDS(knowledge.base, file=rds.file)
        knowledge.base <<- readRDS(rds.file)
    } else {
        kb.dir <- "data/previous_compound_knowledge_bases"
        if (!dir.exists(kb.dir)) dir.create(kb.dir)
        time <- gsub(" ", "_", Sys.time())
        kb.file <- strsplit(rds.file, split="/")[[1]][2]
        saveRDS(knowledge.base, file=paste(kb.dir, "/", time, "_", kb.file, sep=""))
    }
}

# Log file management feature---------------------------------------------------
bash.append.from.bottom.to.top <- function(message, file) {
    # append message from bottom to top of a file
    return(paste("echo '", message, "' | cat - ", file, " > temp && mv temp ", file, sep=""))
}

knowledge.base.commit.logs <- function(message1=NA, message2=NA) {
    logfile <- "data/knowledge_base_commit.log"
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

# Knowledge base conversion feature---------------------------------------------
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
    col <- col[!col %in%  c("LRI_polar", "LRI_nonpolar")]
    df[col] <- lapply(df[col], unlist)
    df <- convert_list_column(df, "LRI_polar")
    df <- convert_list_column(df, "LRI_nonpolar")
    return(df)
}