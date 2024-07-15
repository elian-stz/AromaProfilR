# Check whether the CAS numbers follow the below regex
isCAS <- function(CAS) {
    CAS <- removePrefix(CAS)
    return(grepl("^\\d{2,7}-\\d{2}-\\d$", CAS))
}

# Check whether the CAS numbers are present in the knowledge base
isInKnowledgeBase <- function(CAS) {
    CAS <- addPrefix(CAS) 
    if (is.null(knowledge.base[[CAS]])) return(FALSE)
    return(TRUE)
}

getChemicalFamilies <- function() {
    families <- sapply(knowledge.base, function(x) x$family)
    families <- unlist(unname(families))
    families <- sort(unique(families))
    return(families)
}

addPrefix <- function(CAS) {
    if (all(!grepl("^CAS_", CAS))) CAS <- paste("CAS_", CAS, sep="")
    return(CAS)
}

removePrefix <- function(CAS) {
    if (all(grepl("^CAS_", CAS))) CAS <- gsub("CAS_", "", CAS)
    return(CAS)
}

getAllCASNumbersInKnowledgeBase <- function(prefix=FALSE) {
    entries <- names(knowledge.base)
    if (prefix) return(entries)
    return(removePrefix(entries))
}

notification <- function(type, info=NA) {
    switch(type,
            "input.not.present" = shiny::showNotification(
                "Input not present in the knowledge base",
                type="error"
                ),
            "input.already.present" = shiny::showNotification(
                "Input already present in the knowledge base",
                type="warning"
                ),
            "not.cas" = shiny::showNotification(
                "Input must contain CAS registry number(s)",
                type="error"
                ),
            "single.cas" = shiny::showNotification(
                "Input must be a single CAS registry number",
                type="error"
                ),
            "too.many.cid" = shiny::showNotification(
                paste("Entry cannot be added: ", info,
                      " PubChem CIDs returned instead of one. Choose the template method.",
                      sep=""),
                type="error",
                duration=NULL
                ),
            "no.cid" = shiny::showNotification(
                "Entry cannot be added: impossible to convert the input into PubChem CID",
                type="error",
                duration=NULL
                ),
            "success.one.addition" = shiny::showNotification(
                "Succesfully added 1 CAS registry number",
                type="message"
                ),
            "success.n.removal" = shiny::showNotification(
                paste("Succesfully removed ", info, " CAS registry numbers(s)",
                      sep=""),
                type="message"
                ),
            "template.wrong.colnames" = shiny::showNotification(
                "Input file has different column names than expected.",
                type="error"
                ),
            "success.template.edition" = shiny::showNotification(
                paste("Succesfully edited ", info, " entry(ies)",
                      sep=""),
                type="message"
                ),
            "invalid.file" = shiny::showNotification(
                "Invalid file. Please upload a .tsv file",
                type="error"
                ),
           "missing.columns" = shiny::showNotification(
               paste("Error while uploading data. Missing columns: ", info,
                     sep=""),
               type="error",
               duration=30
               ),
           "wrong.column.type" = shiny::showNotification(
               "Error while uploading data. Wrong data types",
               type="error",
               duration=30
               )
           )
}