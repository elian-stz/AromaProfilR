#' Return the minimal header of the input CSV file from MassHunter.
#' 
#' Get the minimal header of the CSV file from MassHunter. It must contain at 
#' least 5 columns: `CAS.` (`CAS#`), `Compound.Name` (`Compound Name`),
#' `Match.Factor` (`Match Factor`), `Component.RI` (`Component RI`),
#' and `File.Name` (`File Name`). The name between parentheses are the names
#' obtained directly from MassHunter, whereas those that are not are the names
#' that get automatically converted by R.
#' @return vector containing the minimal header of the input CSV file
getMinimalHeader <- function() {
    return(c("CAS.",           # chr   : CAS registry number
             "Compound.Name",  # chr   : common name
             "Match.Factor",   # double: match factor calculated by MassHunter
             "Component.RI",   # double: experimental LRI
             "File.Name"       # chr   : sample name
             #"Component.RT", "Component.Area", "Component.Height"
    ))
}

#' Check the integrity of the input CSV file's header.
#' 
#' Check whether the header of the input CSV file from MassHunter contains
#' the minimal header: `CAS.`, `Compound.Name`, `Match.Factor`, `Component.RI`,
#' and `File.Name`. Display a notification if it does not match.
#' @param header header of the input CSV file from MassHunter
#' @return logical
#' @examples
#' checkFileHeader("CAS.", "Compound.Name", "Match.Factor", "Component.RI", "File.Name")
#' checkFileHeader("CAS.", "Compound.Name", "Match.Factor")
checkFileHeader <- function(header) {
    expectedMinimalHeader <- getMinimalHeader()
    differentCol <- setdiff(expectedMinimalHeader, header)
    if (identical(differentCol, character(0))) return(TRUE)
    notification("missing.columns", paste(differentCol, collapse=", "))
    return(FALSE)
}

#' Check the column types of the input dataframe. 
#' 
#' Check whether the content of the input CSV file from MassHunter contains
#' the right types (without converting first). `CAS.` and `File.Name` are 
#' characters, while the rest is double.
#' @param df dataframe as the input from MassHunter 
#' @return logical
#' @examples
#' checkFileColumnType(myDataframe)
checkFileColumnType <- function(df) {
   header <- getMinimalHeader()
   chrCol <- c("CAS.", "File.Name", "Compound.Name")
   doubleCol <- header[!(header %in% chrCol)]
   for (i in header) {
       if (i %in% chrCol && !is.character(df[[i]])) return(FALSE)
       if (i %in% doubleCol && !is.double(df[[i]])) return(FALSE)
   }
   return(TRUE)
}

#' Check the integrity of the input dataframe from MassHunter.
#' 
#' Check the integrity of the dataframe by checking the minimal header and
#' column types.
#' @param df dataframe as the input from MassHunter 
#' @return logical
#' @examples
#' checkFileIntegrity(myDataframe)
checkFileIntegrity <- function(df) {
    if (!checkFileHeader(colnames(df))) return(FALSE) # notif in the function
    if (!checkFileColumnType(df)) {
        notification("wrong.column.type")
        return(FALSE)
    }
    return(TRUE)
}

getUnknownCompounds <- function(CAS, table=FALSE) {
    entries <- sapply(CAS, isInKnowledgeBase)
    entries <- names(entries[entries == FALSE])
    entries <- table(entries)
    if (table) return(entries)
    return(names(entries))
}

getCompoundsWithoutLRI <- function(CAS, type=c("LRI_polar", "LRI_nonpolar")) {
    entries <- sapply(CAS, function(entry) {
        LRI <- knowledge.base[[addPrefix(entry)]][[type]]
        return(length(LRI) == 1 && is.na(LRI))
    })
    entries <- names(entries[entries == TRUE])
    return(unique(entries))
}

splitFileByTag <- function(df, type=c("LRI_polar", "LRI_nonpolar")) {
    compoundsWithoutLRI <- getCompoundsWithoutLRI(df$CAS., type)
    unknownCompounds <- getUnknownCompounds(df$CAS.)
    df$tag <- lapply(df$CAS., function(x) {
        if (x %in% compoundsWithoutLRI) return("noLRI") 
        if (x %in% unknownCompounds) return("unknown")
        return("analysable")
    })
    df$tag <- unlist(df$tag)
    return(split(df, df$tag))
}

addLRIDifferenceColumn <- function(df, type=c("LRI_polar", "LRI_nonpolar"), mode=c("Median", "Mean")) {
    df$LRI.Difference <- lapply(1:nrow(df), function(i) {
        experimentalLRI <- df[i, "Component.RI"]
        CASprefix <- addPrefix(df[i, "CAS."])
        if (mode == "Median") referenceLRI <- median(knowledge.base[[CASprefix]][[type]])
        if (mode == "Mean") referenceLRI <- mean(knowledge.base[[CASprefix]][[type]])
        return(abs(referenceLRI - experimentalLRI))
    })
    df$LRI.Difference <- unlist(df$LRI.Difference)
    browser()
    if (mode == "Median") {
        df$Median.Reference.LRI <- median(knowledge.base[[addPrefix(df$CAS.)]][[type]])
    } else {
        df$Mean.Reference.LRI <- mean(knowledge.base[[addPrefix(df$CAS.)]][[type]])
    }
    return(df)
}

splitAnalysableTag <- function(df, cutoff=30) {
    df$tag <- lapply(df$LRI.Difference, function(value) {
        if (value <= cutoff) return("retained")
        return("not retained")
    })
    df$tag <- unlist(df$tag)
    return(split(df, df$tag))
}

getSplitInputFile <- function(df, column=c("Polar", "Non-polar"), mode=c("Median", "Mean"), cutoff=30) {
    if (column == "Polar") type <- "LRI_polar"
    if (column == "Non-polar") type <- "LRI_nonpolar"
    
    if (checkFileIntegrity(df)) {
        firstSplit <- splitFileByTag(df, type)
        analysee <- firstSplit[["analysable"]]
        analysee <- addLRIDifferenceColumn(analysee, type, mode)
        analysee <- splitAnalysableTag(analysee, cutoff)
        return(list(
            "retained" = analysee[["retained"]],
            "notRetained" = analysee[["not retained"]],
            "noLRI" = firstSplit[["noLRI"]],
            "unknown" = firstSplit[["unknown"]]
            )
        )
    }
}
