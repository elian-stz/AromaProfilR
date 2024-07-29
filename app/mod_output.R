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
        uiOutput(
            outputId=ns("plots")
        )
    )
}

showPlotServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        
        plots <- reactive({
            req(data())
            plotAllConditions(data())
        })
        
        output$plots <- renderUI({
            lapply(1:length(plots()), function(i) {
                id <- paste0(session$ns("subplot"), i)
                plotOutput(outputId=id)
                
            output[[id]] <- renderPlot({
                plots()[[i]]
            })
            })
        })
    })
}