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
        return(plots)
    })
}

importPDFUI <- function(id) { 
    ns <- NS(id)
    tagList(
        br(),
        downloadLink(
            outputId=ns("downloadPDF"),
            label="Download concentration plots as PDF"
        )
    )
}

importPDFServer <- function(id, plots) {
    moduleServer(id, function(input, output, session) {
        output$downloadPDF <- downloadHandler(
            filename = function() paste(Sys.Date(), "_concentration_plots.pdf", sep=""),
            content = function(con) {
                pdf(con, onefile=TRUE, paper="a4r")
                for (i in 1:length(plots())) print(plots()[[i]])
                
                output <- capture.output(sessionInfo(c("webchem", "PubChemR", "openxlsx", "shinymanager", "shinyjs", "shiny")))
                plot.new()
                text(0, 1, paste(output, collapse = "\n"), adj = c(0, 1), cex = 0.7, family = "mono")
                
                dev.off()#dev.off(dev.list())
            }
        )
    })
}