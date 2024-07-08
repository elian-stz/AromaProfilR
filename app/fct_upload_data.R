getMinimalHeader <- function() {
    return(c("Component.RT",   # double
             "Match.Factor",   # double
             "CAS.",           # chr
             "Component.RI",   # double 
             "File.Name"      # chr
             #"Component.Area", # double
             #"Component.Height", #double
             )
    )
}

checkFileHeader <- function(header) {
    expectedMinimalHeader <- getMinimalHeader()
    differentCol <- setdiff(expectedMinimalHeader, header)
    if (identical(differentCol, character(0))) return(TRUE)
    notification("missing.columns", paste(differentCol, collapse=", "))
    return(FALSE)
}

checkFileColumnType <- function(df) {
   header <- getMinimalHeader()
   chrCol <- c("CAS.", "File.Name")
   doubleCol <- header[!(header %in% chrCol)]
   for (i in header) {
       if (i %in% chrCol && !is.character(df[[i]])) return(FALSE)
       if (i %in% doubleCol && !is.double(df[[i]])) return(FALSE)
   }
   return(TRUE)
}

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
        if (mode == "Median") theoricalLRI <- median(knowledge.base[[CASprefix]][[type]])
        if (mode == "Mean") theoricalLRI <- mean(knowledge.base[[CASprefix]][[type]])
        return(abs(theoricalLRI - experimentalLRI))
    })
    df$LRI.Difference <- unlist(df$LRI.Difference)
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

identifyConditionsAndReplicates <- function(df) {
    samples <- sort(unique(test$File.Name)) #Info
    split <- strsplit(samples, "_")
    names(split) <- samples
    return(split)
}

attributeConditionAndReplicate <- function(list) {
    condition <- 1
    replicate <- 1
    identified <- list()
    sampleNbr <- length(list)
    for (i in 1:sampleNbr) {
        identified[[names(list[i])]] <- c(condition, replicate)
        replicate <- replicate + 1
        if ((i + 1) < sampleNbr && list[[i]][1] != list[[i + 1]][1]) {
            condition <- condition + 1
            replicate <- 1
        }
    }
    return(identified)
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
