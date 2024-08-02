importExcelFileUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h2("Classification file"),
        tags$p("The classification file contains all compounds individually classified into 4 groups:"),
        tags$ul(
            tags$li(HTML("<code>retained</code>: compounds with &Delta;LRI &le; threshold")),
            tags$li(HTML("<code>notRetained</code>: compounds with &Delta;LRI > threshold")),
            tags$li(HTML("<code>noLRI</code>: compounds with no reference LRI in the knowledge base")),
            tags$li(HTML("<code>unknown</code>: compounds absent from the knowledge base"))
        ),
        tags$p("Each sheet of the Excel file corresponds to one of the 4 groups."),
        downloadLink(
            outputId=ns("downloadExcel"),
            label="Download classification file as XLSX"
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
        tags$h2("Plots"),
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
                p <- req(plots()[[i]]())
                replayPlot(p)
            })
            })
        })
        return(plots)
    })
}

importPDFUI <- function(id) { 
    ns <- NS(id)
    tagList(
        tags$hr(),
        tags$h2("Concentration plots"),
        tags$p(HTML("A concentration plot sums up each condition. It displays relative standard deviation, average estimated concentration, and number of apparitions for every <b>retained</b> compound.")),
        tags$p("If you did not provide any design file, each sample will be considered a single condition."),
        tags$p("You can download the plots as a single PDF file."),
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
                pdf(con, onefile=TRUE, width=12, height=8)#paper="a4r")
                for (i in 1:length(plots())) print(plots()[[i]]())
                output <- capture.output(sessionInfo(c("webchem", "PubChemR", "openxlsx", "shinymanager", "shinyjs", "shiny")))
                output <- c(as.character(Sys.time()), output)
                plot(0,0, type="n", xlim=c(0,2), ylim=c(0,1), xlab="", ylab="", axes=F)
                text(0, 1, paste(output, collapse="\n"), adj=c(0, 1), cex=0.7, family="mono")
                
                dev.off()
            }
        )
    })
}