displayRegisterUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$p("The datatable below contains every compound finds in th"),
        downloadLink(
            outputId=ns("download"),
            label="Download unknown compound register"
        ),
        dataTableOutput(
            outputId=ns("dt")
        )
    )
}

displayRegisterServer <- function(id, dataSplit) {
    moduleServer(id, function(input, output, session) {
        output$dt <- renderDataTable({
            #invalidateLater(10000, session)
            updateUnknownCompoundsRegister(dataSplit()$unknown)
            file <- "data/unknown_compounds_register.rds"
            if (file.exists(file)) readRDS(file)
        })
        output$download <- downloadHandler(
            filename = function() paste(Sys.Date(), "_unknown_compound_register.tsv", sep=""),
            content = function(con) {
                file <- "data/unknown_compounds_register.rds"
                if (file.exists(file)) {
                    df <- readRDS(file)
                    write.table(df, file=con, sep="\t", row.names=FALSE, quote=FALSE)
                }
            }
        )
    })
}