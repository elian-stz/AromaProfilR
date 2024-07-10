getUniqueEntries <- function(dt) {
    CAS <- lapply(dt, function(class) {
        return(class["CAS."])
    })
    CAS <- unname(unlist(CAS))
    unique(CAS)
    return(CAS)
}

createDataframeFromKnowledgeBase <- function(entries) {
    # Create dataframe containing info from compounds in knowledge base
    queried <- knowledge.base[names(knowledge.base) %in% entries]
    dfWithKnowledgeBaseInfo <- knowledge.base.to.dataframe(queried)
    dfWithKnowledgeBaseInfo <- dfWithKnowledgeBaseInfo[, !(names(dfWithKnowledgeBaseInfo) %in% c("LRI_polar", "LRI_nonpolar"))]
    names(dfWithKnowledgeBaseInfo)[names(dfWithKnowledgeBaseInfo) == "CAS"] <- "CAS."
    return(dfWithKnowledgeBaseInfo)
}

addDescriptorColumn <- function(df) {
    df$Descriptor <- apply(df, 1, function(row) {
        descriptor <- c(unname(row["odor_pubchem"]), unname(row["row$taste_pubchem"]))
        descriptor <- append(descriptor, c(unname(row["odor_goodscents"]), unname(row["odor_flavornet"])))
        descriptor[is.na(descriptor)] <- ""
        if (sum(nchar(descriptor)) == 0) {
            return(0)
        } else return(1)
    })
    return(df)
}

addKnowledgeBaseInfo <- function(dataSplit) {
    # Get all entries from classified data
    entries <- getUniqueEntries(dataSplit)
    entries <- addPrefix(entries)
    
    dfWithKnowledgeBaseInfo <- createDataframeFromKnowledgeBase(entries)
    
    # Join split input file
    dataSplit <- lapply(dataSplit, function(class) {
        class <- merge(x = class, y = dfWithKnowledgeBaseInfo, by = "CAS.", all.x = TRUE)
        class <- addDescriptorColumn(class)
        return(class)
    })
    
    return(dataSplit)
}
