displayRegisterUI <- function(id) {
    ns <- NS(id)
    tagList(
        dataTableOutput(
            outputId=ns("dt")
        )
    )
}

displayRegisterServer <- function(id, data=NA) {
    #stopifnot(is.reactive(data))
    moduleServer(id, function(input, output, session) {
        #updateUnknownCompoundsRegister(data()$unknown)
        output$dt <- renderDataTable({
            file <- "data/unknown_compounds_register.rds"
            if (file.exists(file)) readRDS(file)
        })
    })
}