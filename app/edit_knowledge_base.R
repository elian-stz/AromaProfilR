remove.cas.numbers <- function(text) {
    cas.numbers <- strsplit(text, split=" ")
    isCAS <- all(unname(sapply(cas.numbers, webchem::is.cas)))
    # Check if CAS existing in knowledge base
    if (isCAS) {
        save.knowledge.base(overwrite=FALSE)
        cas.numbers <- paste("CAS_", cas.numbers, sep="")
        knowledge.base <- knowledge.base[names(knowledge.base) != cas.numbers]
        save.knowledge.base(overwrite=TRUE)
        message <- paste("Removed ", length(cas.numbers), " CAS registry number(s): ",
                         paste(cas.numbers, collapse=", "),
                         sep=""
                         )
        knowledge.base.commit.logs(message)
        showNotification(paste("Succesfully removed ", length(cas.numbers), " CAS registry numbers(s)"), type="message")
    } else {
        showNotification("Input text is not CAS registry number(s)", type="error")
    }
}

###############################################################################
# Save, reload and logs functions
################################################################################

save.knowledge.base <- function(overwrite) {
    # Add a max number of previous knowledge bases?
    if (overwrite) {
        save(knowledge.base, file=rdata.file)
        load(rdata.file, envir=.GlobalEnv)
    } else {
        kb.dir <- "previous_compound_knowledge_bases"
        if (!dir.exists(kb.dir)) system(paste("mkdir ", kb.dir, sep=""))
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
        return()
    }
    
    if (!is.na(message)) {
        system(bash.append.from.bottom.to.top(message="", file=logfile))
        system(bash.append.from.bottom.to.top(message=message, file=logfile))
        system(bash.append.from.bottom.to.top(message=Sys.time(), file=logfile))
    }
}