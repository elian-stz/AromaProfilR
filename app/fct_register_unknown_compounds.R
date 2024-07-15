#' Return an empty unknown compound register as dataframe.
#' 
#' Return an empty unknown compound register as a dataframe containing four
#' columns: `CAS` for the CAS registry number, `Compound.Name` for the compound's
#' common name, `Last.Encounter` for the date of the compound's last encounter,
#' and `Encounter.Number` for the compound's number of encounters.
#' 
#' @return an empty dataframe containing the columns of the unknown compound register
#' @examples
#' getEmptyRegister()
getEmptyRegister <- function() {
    header <- c("CAS", "Compound.Name", "Last.Encounter", "Encounter.Number")
    register <- data.frame(matrix(ncol=length(header), nrow=0))
    colnames(register) <- header
    return(register)
}

#' Filter out compounds based on their common name. Here it classifies
#' organosilicon compounds as unwanted.
#' 
#' Filter out organosilicon compounds based on their common name. The filter
#' includes contaminants that are not interesting for aroma identification.
#' The filters are TMS, TBDMS, TBS, TIPS, and sil.
#' 
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

removeCompoundsInKnowledgeBase <- function(register) {
    knowledgeBaseEntries <- getAllCASNumbersInKnowledgeBase()
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
            register$Last.Encounter[register$CAS == CAS] <- date 
            count <- as.integer(register$Encounter.Number[register$CAS == CAS]) + 1
            register$Encounter.Number[register$CAS == CAS] <- count
        } else if (!isUnwantedCompound(name)) {
            register[nrow(register) + 1, ] <- c(CAS, name, date, 1)
        }
    }
    
    register <- removeCompoundsInKnowledgeBase(register)
    register$Encounter.Number <- as.integer(register$Encounter.Number)
    saveRDS(register, file=file)
}
