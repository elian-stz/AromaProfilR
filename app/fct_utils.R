# Check whether the CAS numbers follow the below regex
isCAS <- function(cas.vector) {
    return(all(grepl(cas.vector, pattern="^\\d{2,7}-\\d{2}-\\d$")))
}

# Check whether the CAS numbers are present in the knowledge base
# Does not append the prefix CAS_
isInKnowledgeBase <- function(cas.vector) {
    queriedCASinKnowledgeBase <- length(knowledge.base[names(knowledge.base) %in% cas.vector])
    if (queriedCASinKnowledgeBase < length(cas.vector)) return(FALSE)
    return(TRUE)
}

notification <- function(type, number=NA) {
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
                paste("Entry cannot be added: ", number,
                      " PubChem CID(s) returned instead of one. Choose the template method.",
                      sep=""),
                type="error"
                ),
            "no.cid" = shiny::showNotification(
                "Entry cannot be added: impossible to convert the input into PubChem CID",
                type="error"
                ),
            "success.one.addition" = shiny::showNotification(
                "Succesfully added 1 CAS registry number",
                type="message"
                ),
            "success.n.removal" = shiny::showNotification(
                paste("Succesfully removed ", number, " CAS registry numbers(s)",
                      sep=""),
                type="message"
                ),
            "template.wrong.colnames" = shiny::showNotification(
                "Input file has different column names than expected.",
                type="error"
                ),
            "success.template.edition" = shiny::showNotification(
                paste("Succesfully edited ", number, " entry(ies)",
                      sep=""),
                type="message"
                ),
            "invalid.file" = shiny::showNotification(
                "Invalid file. Please upload a .tsv file",
                type="error"
                )
           )
}