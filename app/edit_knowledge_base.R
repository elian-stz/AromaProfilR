require(webchem)

remove.cas.numbers <- function(text) {
    cas.numbers <- strsplit(text, sep=" ")
    if (all(unname(sapply(cas.numbers, webchem::is.cas)))) {
        cas.numbers <- paste("CAS_", cas.numbers, sep="")
        knowledge.base <- knowledge.base[names(knowledge.base) != cas.numbers]
        save(knowledge.base, file=rdata.file)
        commit.knowledge.base(paste("Removed ", length(cas.numbers), " CAS registry number(s)", sep=""))
    }
}

knowledge.base.commit.logs <- function(message) {
    if (!file.exists("knowledge_base_commit.log")) {
        system(paste("echo ", Sys.time(), "\n"))
    }
}

commit.knowledge.base <- function(message) {
    if (!dir.exists(".git")) {
        system("git init")
        system(paste("git add ", rdata.file, sep=""))
        system("git commit -m 'Knowledge base first version'")
    }
    
    system(paste("git add ", rdata.file, sep=""))
    system(paste("git commit -m '", message, "'", sep=""))
    load(rdata.file, envir=.GlobalEnv)
}