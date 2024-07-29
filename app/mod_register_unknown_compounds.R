displayRegisterUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$p("The datatable below contains the unknown compound register. It references all compounds found in the MassHunter file that are absent from the knowledge base."),
        tags$p(HTML("Compounds containing <code>TMS</code>, <code>TBDMS</code>, <code>TBS</code>, <code>TIPS</code>, or <code>sil</code> in their name are excluded.")),
        tags$p("You can download the unknown compound register as XLSX below."),
        downloadLink(
            outputId=ns("download"),
            label="Download unknown compound register"
        ),
        br(),br(),
        dataTableOutput(
            outputId=ns("dt")
        )
    )
}

displayRegisterServer <- function(id, dataSplit) {
    moduleServer(id, function(input, output, session) {
        
        refreshTrigger <- reactiveVal(0)
        
        observeEvent(dataSplit(), {
            updateUnknownCompoundsRegister(dataSplit()$unknown)
            refreshTrigger(refreshTrigger() + 1)
        })
        
        output$dt <- renderDataTable({
            refreshTrigger()
            file <- "data/unknown_compounds_register.rds"
            if (file.exists(file)) readRDS(file) else data.frame()
        })
        
        output$download <- downloadHandler(
            filename = function() paste(Sys.Date(), "_unknown_compound_register.xlsx", sep=""),
            content = function(con) {
                file <- "data/unknown_compounds_register.rds"
                if (file.exists(file)) {
                    df <- readRDS(file)
                    openxlsx::write.xlsx(df, file=con, rowNames=FALSE, keepNA=TRUE, na.string="NA")
                }
            }
        )
    })
}