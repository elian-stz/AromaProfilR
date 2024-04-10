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
            showNotification(paste("Succesfully removed ", length(cas.numbers), " CAS registry numbers(s)"), type="message")
        } else showNotification("Input not present in the knowledge base", type="error")
    } else showNotification("Input must contain CAS registry number(s)", type="error")
}

add.single.cas.number <- function(text.field) {
    if (isCAS(text.field)) {
        cas.prefix <- paste("CAS_", text.field, sep="")
        if (!isInKnowledgeBase(cas.prefix)) {
            cid <- cas2cid(text.field)
            if (!is.na(cid) && length(cid) == 1) {
                save.knowledge.base(knowledge.base, overwrite=FALSE)
                knowledge.base[[cas.prefix]] <- getEntryInfo(cid, text.field)
                save.knowledge.base(knowledge.base, overwrite=TRUE)
                message <- paste("Added 1 CAS registry number: ", text.field, sep="")
                knowledge.base.commit.logs(message)
                showNotification(paste("Succesfully added 1 CAS registry number"), type="message")
            } else {
                if (is.na(cid) || length(cid) == 0) showNotification("Entry cannot be added: impossible to convert the input into PubChem CID",
                                                       type="error")
                if (length(cid) > 1) showNotification("Entry cannot be added: more than one CID returned. Choose another method.", type="error")
            }
        } else showNotification("CAS registry number already present in the knowledge base", type="warning")
    } else showNotification("Input must be a single CAS registry number", type="error")
}

getEntryInfo <- function(cid, cas) {
    properties <- getPropertiesFromCID(cid)
    smiles <- properties$CanonicalSMILES
    
    common_name <- getCommonName(cid)
    LRI_polar <- getLRI(cid, type="polar", canonicalSMILES=smiles)
    Sys.sleep(1)
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

################################################################################
# Verification functions: isCAS, isInKnowledgeBase
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

###############################################################################
# Save, reload and logs functions
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

knowledge.base.commit.logs <- function(message=NA) {
    logfile <- "knowledge_base_commit.log"
    if (!file.exists(logfile)) {
        system(paste("echo ", Sys.time(), " > ", logfile, sep=""))
        system(paste("echo 'Compound knowledge base first version' >> ", logfile, sep=""))
    }
    
    if (!is.na(message)) {
        system(bash.append.from.bottom.to.top(message="", file=logfile))
        system(bash.append.from.bottom.to.top(message=message, file=logfile))
        system(bash.append.from.bottom.to.top(message=Sys.time(), file=logfile))
    }
}