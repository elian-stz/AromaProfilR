#' Create an empty unknown compound register as dataframe in a RDS file.
#' 
#' Create an empty unknown compound register as a dataframe containing four
#' columns: `CAS` for the CAS registry number, `Compound.Name` for the compound's
#' common name, `Last.Encounter` for the date of the compound's last encounter,
#' and `Encounter.Number` for the compound's number of encounters. This dataframe
#' is stored in a RDS file named `unknown_compounds_register.rds` located in the
#' `data/` directory.
#' @examples
#' createEmptyUnknownCompoundRegister()
createEmptyUnknownCompoundRegister <- function() {
    # Run in global.R
    header <- c("CAS", "Compound.Name", "Last.Encounter", "Encounter.Number")
    register <- data.frame(matrix(ncol=length(header), nrow=0))
    colnames(register) <- header
    file <- "data/unknown_compounds_register.rds"
    if (!file.exists(file)) saveRDS(register, file=file)
}

#' Filter out compounds based on their common name. Here it classifies
#' organosilicon compounds as unwanted.
#' 
#' Filter out organosilicon compounds based on their common name. The filter
#' includes contaminants that are not interesting for aroma identification.
#' The filters are TMS, TBDMS, TBS, TIPS, and sil.
#' @return logical: TRUE for unwanted compounds
#' @examples
#' isUnwantedCompound("Dimethyl ether")
#' isUnwantedCompound("Silane, methyl-")
isUnwantedCompound <- function(name) {
    unwanted <- c("TMS", "TBDMS", "TBS", "TIPS",
                  "sil"
                  # phthal
    )
    unwanted <- paste(unwanted, collapse="|")
    return(grepl(pattern=unwanted, x=name, ignore.case=TRUE))
}

#' Remove entries from the unknown compound dataframe if they are in the
#' knowledge base.
#' 
#' Given a dataframe containing the unknown compound register, remove CAS
#' registry numbers from the unknown compound register if they are present in
#' both the register and the knowledge base.
#' @param register a dataframe containing the unknown compound register
#' @return a new register with deplemented CAS registry numbers
#' @examples
#' removeCompoundsInKnowledgeBase(myRegister)
removeCompoundsFromRegister <- function(register) {
    knowledgeBaseEntries <- getAllCASNumbersInKnowledgeBase()
    entriesToRemove <- intersect(knowledgeBaseEntries, register$CAS)
    if (!identical(entriesToRemove, character(0))) {
        register <- register[!(entriesToRemove %in% register["CAS"])]
    }
    return(register)
}

#' Update the unknown compound register by adding the unknown compounds in the
#' input file from MassHunter and removing the ones present in both the register
#' and the knowledge base.
#' @param unknownGroup a dataframe containing the unknown compound group after classification
#' @examples
#' updateUnknownCompoundsRegister(myUnknownCompoundsDF)
updateUnknownCompoundsRegister <- function(unknownGroup) {
    file <- "data/unknown_compounds_register.rds"
    register <- readRDS(file)
    
    date <- as.character(Sys.Date())
    for (i in 1:nrow(unknownGroup)) {
        CAS <- unknownGroup[i, "CAS."]
        name <- unknownGroup[i, "Compound.Name"]
        if (CAS %in% register$CAS) {
            register$Last.Encounter[register$CAS == CAS] <- date 
            count <- as.integer(register$Encounter.Number[register$CAS == CAS]) + 1
            register$Encounter.Number[register$CAS == CAS] <- count
        } else if (!isUnwantedCompound(name)) {
            register[nrow(register) + 1, ] <- c(CAS, name, date, 1)
        }
    }
    
    register <- removeCompoundsFromRegister(register)
    register$Encounter.Number <- as.integer(register$Encounter.Number)
    saveRDS(register, file=file)
    return(0)
}
