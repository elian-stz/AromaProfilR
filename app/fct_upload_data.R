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
    return(c("CAS.",          # chr   : CAS registry number
             "Compound.Name", # chr   : common name
             "Match.Factor",  # double: match factor calculated by MassHunter
             "Component.RI",  # double: experimental LRI
             "File.Name"      # chr   : sample name
    ))
    #"Component.RT", "Component.Area", "Component.Height"
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
#' the right types (without converting first). `CAS.`, `File.Name`, and
#' `Compound.Name` are characters, while the rest is double.
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

#' Identify CAS registry numbers that are absent from the knowledge base.
#' 
#' In a vector of several CAS registry numbers, identify CAS registry numbers
#' that are absent from the knowledge base.
#' @param CAS a vector of CAS registry numbers 
#' @return a vector of CAS registry numbers absent from the knowledge base
#' @examples
#' getUnknownCompounds(c("CAS_100-51-6", "CAS_45-12-7"))
#' getUnknownCompounds(c("100-51-6", "45-12-7"))
getUnknownCompounds <- function(CAS) {
    entries <- sapply(CAS, isInKnowledgeBase)
    entries <- names(entries[entries == FALSE])
    entries <- table(entries)
    return(names(entries))
}

#' Identify CAS registry numbers that are in the knowledge base but have no
#' reference LRI values for one of the column types (polar or non-polar).
#' 
#' In a vector of several CAS registry numbers, identify CAS registry numbers
#' that are in the knowledge base but have no reference LRI values for one of
#' the column types (polar or non-polar).
#' @param CAS a vector of CAS registry numbers
#' @param type type of the column used (polar/non-polar): `LRI_polar` or `LRI_nonpolar`
#' @return a vector of CAS registry numbers in the knowledge base but with no reference LRI values
#' @examples
#' getCompoundsWithoutLRI(c("100-51-6", "45-12-7"), "LRI_polar")
#' getCompoundsWithoutLRI(c("100-51-6", "45-12-7"), "LRI_nonpolar")
getCompoundsWithoutLRI <- function(CAS, type=c("LRI_polar", "LRI_nonpolar")) {
    entries <- sapply(CAS, function(entry) {
        LRI <- knowledge.base[[addPrefix(entry)]][[type]]
        return(length(LRI) == 1 && is.na(LRI))
    })
    entries <- names(entries[entries == TRUE])
    return(unique(entries))
}

#' Split a dataframe into a list of three dataframes. The three dataframes
#' correspond to the classification of each compound on whether their LRI
#' difference can be calculated. The three dataframes are:
#' * noLRI: compounds in the knowledge base but with no reference LRI values
#' * unknown: compounds absent from the knowledge base
#' * analysable: compounds for which the LRI difference can be calculated
#' 
#' @param df a dataframe containing the data from MassHunter
#' @param type type of the column used (polar/non-polar): `LRI_polar` or `LRI_nonpolar`
#' @return a list of three dataframes classifying each compound
#' @examples
#' splitFileByTag(myDataframe, "LRI_polar")
#' splitFileByTag(myDataframe, "LRI_nonpolar")
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

#' Add two columns to the dataframe: `(Median|Mean).Reference.LRI` and `LRI.Difference`.
#' 
#' Add two columns to the dataframe: `(Median|Mean).Reference.LRI` and `LRI.Difference`.
#' `(Median|Mean).Reference.LRI` is the median or average reference LRI value for
#' one of the column types (polar or non-polar).
#' `LRI.Difference` results from the calculation of the difference between the
#' mean or average reference LRI values and the experimental LRI. The difference
#' is taken as the absolute value.
#' @param df a dataframe containing the columns `Component.RI` and `CAS.`
#' @param type type of the column used (polar/non-polar): `LRI_polar` or `LRI_nonpolar`
#' @param mode calculation of the reference LRI value: `Median` or `Mean`
#' @return a dataframe containing the LRI difference and reference LRI value
#' @examples
#' addLRIDifferenceColumn(myDataframe, "LRI_polar", "Median")
#' addLRIDifferenceColumn(myDataframe, "LRI_nonpolar", "Mean")
addLRIDifferenceColumn <- function(df, type=c("LRI_polar", "LRI_nonpolar"), mode=c("Median", "Mean")) {
    newColumns <- c(paste(mode, ".Reference.LRI", sep=""), "LRI.Difference")
    df[newColumns] <- do.call(rbind.data.frame, lapply(1:nrow(df), function(i) {
        experimentalLRI <- df[i, "Component.RI"]
        CASprefix <- addPrefix(df[i, "CAS."])
        if (mode == "Median") referenceLRI <- median(knowledge.base[[CASprefix]][[type]])
        if (mode == "Mean") referenceLRI <- mean(knowledge.base[[CASprefix]][[type]])
        difference <- abs(referenceLRI - experimentalLRI)
        return(c(referenceLRI, difference))
    }))
    return(df)
}

#' Split a dataframe into a list of two dataframes: retained and not retained
#' compounds based on the LRI difference column.
#' 
#' Split a dataframe into a list of two dataframes: retained and not retained
#' compounds based on the LRI difference column. If the LRI difference is higher
#' than a threshold, then the compound is classified as not retained, otherwise
#' as retained.
#' @param df a dataframe containing the column `LRI.Difference`
#' @param cutoff a threshold to filter the LRI difference (default: 30)
#' @return a list of dataframes with retained and not retained compounds
#' @examples
#' splitAnalysableTag(myDataframe, 30)
splitAnalysableTag <- function(df, cutoff=30) {
    df$tag <- lapply(df$LRI.Difference, function(value) {
        if (value <= cutoff) return("retained")
        return("not retained")
    })
    df$tag <- unlist(df$tag)
    return(split(df, df$tag))
}

#' Classify each compound in a dataframe containing data from MassHunter.
#' This classification includes four groups: retained, notRetained, noLRI, and
#' unknown.
#' 
#' Classify each compound in a dataframe containing data from MassHunter.
#' This classification includes four groups: retained, notRetained, noLRI, and
#' unknown. Compounds are retained or not depending on the comparison of their
#' LRI difference. noLRI compounds are the compounds in the knowledge base but
#' with no LRI values. Unknown compounds are compounds absent from the knowledge
#' base.
#' @param df a dataframe containing data from MassHunter
#' @param column the type of column used: `Polar` or `Non-polar`
#' @param mode calculation of the reference LRI: `Median` or `Mean`
#' @param cutoff a threshold to filter the LRI difference (default: 30)
#' @return a list of dataframes with the four groups
#' @examples
#' getSplitInputFile(myDataframe, "Polar", "Median", 30)
#' getSplitInputFile(myDataframe, "Non-polar", "Mean", 40)
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
        ))
    }
}
