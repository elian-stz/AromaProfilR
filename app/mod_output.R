importExcelFileUI <- function(id) {
    ns <- NS(id)
    tagList(
        downloadLink(
            outputId=ns("downloadExcel"),
            label="Download Excel file"
        )
    )
}

importExcelFileServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        output$downloadExcel <- downloadHandler(
            filename = function() paste(Sys.Date(), "_classification.xlsx", sep=""),
            content = function(con) {
                fileContent <- addKnowledgeBaseInfo(data())
                openxlsx::write.xlsx(fileContent, file=con, rowNames=FALSE, keepNA=TRUE, na.string="NA")
            }
        )
    })
}

showPlotUI <- function(id) {
    ns <- NS(id)
    tagList(
        plotOutput(
            outputId=ns("concentrationPlot")
        )
    )
}

showPlotServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        
        plots <- observeEvent(data(), {
            plotAllConditions(data())
        })
        
        lapply(plots, function(p) {
            output$concentrationPlot <- renderPlot({
                p
            })
        })
    })
}