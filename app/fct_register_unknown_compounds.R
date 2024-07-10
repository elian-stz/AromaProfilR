getEmptyRegister <- function() {
    header <- c("CAS", "common_name", "last_encounter", "encounter_number")
    register <- data.frame(matrix(ncol=length(header), nrow=0))
    colnames(register) <- header
    return(register)
}

isUnwantedCompound <- function(name) {
    unwanted <- c("TMS", "TBDMS", "TBS", "TIPS", # 6-Chloro-2,3-quinoxalinediol, O, O', di-TMS
                  "sil" # silanediol
                  # phthal
    )
    unwanted <- paste(unwanted, collapse="|")
    return(grepl(pattern=unwanted, x=name, ignore.case=TRUE))
}

removeCompoundsInKnowledgeBase <- function(register) {
    knowledgeBaseEntries <- removePrefix(names(knowledge.base))
    entriesToRemove <- intersect(knowledgeBaseEntries, register$CAS)
    if (!identical(entriesToRemove, character(0))) {
        register <- register[!(entriesToRemove %in% register["CAS"])]
    }
    return(register)
}

updateUnknownCompoundsRegister <- function(df) {
    # Create unknown compound register
    file <- "data/unknown_compounds_register.rds"
    if (!file.exists(file)) saveRDS(getEmptyRegister(), file=file)
    register <- readRDS(file)
    date <- as.character(Sys.Date())
    
    for (i in 1:nrow(df)) {
        CAS <- df[i, "CAS."]
        name <- df[i, "Compound.Name"]
        if (CAS %in% register$CAS) {
            register$last_encounter[register$CAS == CAS] <- date 
            count <- as.integer(register$encounter_number[register$CAS == CAS]) + 1
            register$encounter_number[register$CAS == CAS] <- count
        } else if (!isUnwantedCompound(name)) {
            register[nrow(register) + 1, ] <- c(CAS, name, date, 1)
        }
    }
    
    register <- removeCompoundsInKnowledgeBase(register)
    register$encounter_number <- as.integer(register$encounter_number)
    saveRDS(register, file=file)
}
