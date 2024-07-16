#' Check whether the CAS registry follows the format of an actual CAS registry
#' number.
#' @param CAS a CAS registry number with or without prefix
#' @return logical: TRUE if it follows the format
#' @examples
#' isCAS("0-0")
#' isCAS("100-51-6")
#' isCAS("CAS_100-51-6")
#' isCAS("CAS_0-0")
isCAS <- function(CAS) {
    CAS <- removePrefix(CAS)
    return(grepl("^\\d{2,7}-\\d{2}-\\d$", CAS))
}

#' Check whether the CAS registry numbers are present in the knowledge base.
#' @param CAS a CAS registry number with or without prefix
#' @return logical: TRUE if it is in the knowledge base
#' @examples
#' isInKnowledgeBase("100-51-6")
#' isInKnowledgeBase("CAS_100-51-6")
isInKnowledgeBase <- function(CAS) {
    CAS <- addPrefix(CAS) 
    if (is.null(knowledge.base[[CAS]])) return(FALSE)
    return(TRUE)
}

#' Retrieve all the chemical families from all compounds in the knowledge base.
#' @return a vector of all chemical families
#' @examples
#' getChemicalFamilies()
getChemicalFamilies <- function() {
    families <- sapply(knowledge.base, function(x) x$family)
    families <- unlist(unname(families))
    families <- sort(unique(families))
    return(families)
}

#' Add the prefix `CAS_` to the CAS registry number to query the knowledge base.
#' @param CAS a CAS registry number
#' @return a CAS registry number with the prefix `CAS_`
#' @examples
#' addPrefix("100-51-6")
addPrefix <- function(CAS) {
    if (all(!grepl("^CAS_", CAS))) CAS <- paste("CAS_", CAS, sep="")
    return(CAS)
}

#' Remove the prefix `CAS_` to the name of an entry. 
#' @param CAS a CAS registry number with the prefix `CAS_`
#' @return a CAS registry number without the prefix `CAS_`
#' @examples
#' addPrefix("CAS_100-51-6")
removePrefix <- function(CAS) {
    if (all(grepl("^CAS_", CAS))) CAS <- gsub("CAS_", "", CAS)
    return(CAS)
}

#' Retrieve all the CAS registry numbers in the knowledge base.
#' @param prefix logical: with or without prefix (default: FALSE, without prefix)
#' @return a vector containing all the CAS registry numbers in the knowledge base
#' @examples
#' getAllCASNumbersInKnowledgeBase()
#' getAllCASNumbersInKnowledgeBase(prefix=TRUE)
getAllCASNumbersInKnowledgeBase <- function(prefix=FALSE) {
    entries <- names(knowledge.base)
    if (prefix) return(entries)
    return(removePrefix(entries))
}

#' Shiny notifications, which rely on switch case.
#' @param type an intern argument e.g. `not.cas`, `input.not.present`
#' @param info data: vectors, characters, integers
#' @examples
#' notification("input.not.present")
#' notification("success.template.edition", 2)
#' notification("missing.columns", c("Match.Factor", "File.Name"))
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
               ),
           "wrong.design.file.header" = shiny::showNotification(
               paste("Error while uploading design file. Missing columns: ", info,
                     sep=""),
               type="error",
               duration=30
               ),
           "wrong.design.file.levels" = shiny::showNotification(
               paste("Error while uploading design file. Levels don't match."),
               type="error",
               duration=30
               )
           )
}