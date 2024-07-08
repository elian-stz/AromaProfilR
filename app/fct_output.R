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

addKnowledgeBaseInfo <- function(dataSplit) {
    # Get all 
    entries <- getUniqueEntries(dataSplit)
    entries <- addPrefix(entries)
    
    dfWithKnowledgeBaseInfo <- createDataframeFromKnowledgeBase(entries)
    
    # Join split input file
    dataSplit <- lapply(dataSplit, function(class) {
        merge(x = class, y = dfWithKnowledgeBaseInfo, by = "CAS.", all.x = TRUE)
    })
    
    return(dataSplit)
}
