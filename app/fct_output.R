getUniqueEntries <- function(dt) {
    CAS <- lapply(dt, function(class) class["CAS."])
    CAS <- unname(unlist(CAS))
    CAS <- unique(CAS)
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
    dfWithKnowledgeBaseInfo <- addDescriptorColumn(dfWithKnowledgeBaseInfo)
    
    # Join split input file
    dataSplit <- lapply(dataSplit, function(group) {
        merge(x = group, y = dfWithKnowledgeBaseInfo, by = "CAS.", all.x = TRUE)
    })
    
    return(dataSplit)
}

compute_conc_mean_sd <- function(m, n.rep=3) {
    val <- m$Estimated.Conc.
    diff.rep <- n.rep - length(unique(m$Replicate))
    val <- c(val, rep(0, diff.rep))
    rsd <- (sd(val)/mean(val))*100
    if (is.na(rsd)) rsd <- -5
    meanConc <- mean(val)
    if (is.na(meanConc) || meanConc < 0) meanConc <- 10e-5
    return(c(
        mean = meanConc,
        sd = sd(val),
        rsd = rsd,
        min = min(val),
        max = max(val),
        n.missing = diff.rep
    ))
}

plotConcentration <- function(subsetDf) {
    #subsetDf$Estimated.Conc[is.na(subsetDf$Estimated.Conc.)] <- 0
    test.comp <- split(subsetDf, subsetDf$CAS.)
    n.rep <- length(unique(subsetDf$File.Name))
    
    valid.rep <- sapply(test.comp, function(m) return(length(unique(m$Replicate))))
    
    comp.stat <- t(sapply(test.comp, compute_conc_mean_sd, n.rep=n.rep))
    n.max.missing <- max(comp.stat[,"n.missing"])
    color <- c("green", "red")
    if(n.max.missing>1) {
        color <- c("green", colorRampPalette(c("orange", "red"))(n.max.missing))
    }
    par(mar=c(6.1, 4.1, 2.1, 4.1))
    plot(comp.stat[,"rsd"], type="h", xlab="", ylab="Relative standard deviation",
         axes=FALSE, lwd=5, col=color[comp.stat[,"n.missing"]+1])
    axis(2)
    axis(1, at = 1:nrow(comp.stat), labels = row.names(comp.stat), las=2)
    legend("topleft", paste(0:n.max.missing, "missing"), fill=color)
    
    par(new=TRUE)
    plot(comp.stat[,'mean'], type="p", xlab="", ylab="", axes=F, col=4, log="y")
    axis(4, col=4, col.axis=4)
    mtext("Mean estimated concentration", side=4, line=2, col=4)
    title(subsetDf[1, "Condition"], line=1)
    
    p <- recordPlot()
    return(p)
}

plotAllConditions <- function(dataSplit) {
    dataSplit <- dataSplit[["retained"]]
    conditions <- unique(dataSplit$Condition)
    plots <- list()
    for (condition in conditions) {
        subsetDf <- subset(dataSplit, Condition == condition)
        plots[[condition]] <- plotConcentration(subsetDf)
    }
    return(plots)
}
