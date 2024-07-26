# Upload input file-------------------------------------------------------------
uploadInputFileUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h2("Input"),
        radioButtons(
            inputId=ns("columnType"),
            label="Select a column type",
            choices=c("Polar", "Non-polar")
        ),
        radioButtons(
            inputId=ns("method"),
            label="Select a LRI difference method",
            choices=c("Median", "Mean")
        ),
        numericInput(
            inputId=ns("cutoff"),
            label="Enter a LRI difference threshold",
            value=30,
            min=1,
            max=300
        ),
        fileInput(
            inputId=ns("MHfile"),
            label="Upload MassHunter file as CSV with comma (,) as separator",
            accept=".csv"
        ),
        div(style = "margin-top: -20px"),
        tags$p("The MassHunter file must contain at least 6 columns:"),
        tags$ul(
            tags$li("CAS#"),
            tags$li("Compound Name"),
            tags$li("Match Factor"),
            tags$li("Component RI"),
            tags$li("File Name"),
            tags$li("Estimated Conc")
        ),
        fileInput(
            inputId=ns("designFile"),
            label="Upload design file as TSV",
            accept=".tsv"
        ),
        div(style = "margin-top: -20px"),
        tags$p("The design file sums up your experiments. It must contain 3 columns:"),
        tags$ul(
            tags$li("File Name (same levels as in the MassHunter file)"),
            tags$li("Condition"),
            tags$li("Replicate")
        )
    )
}

uploadInputFileServer <- function(id) {
    moduleServer(id, function(input, output, session) {
            data <- reactive({
                req(input$MHfile, input$designFile)
                MHdf <- try(read.csv(input$MHfile$datapath, na.strings=c("", "NA")))
                designdf <- try(read.csv(input$designFile$datapath, na.strings=c("", "NA"), sep="\t"))
                if (inherits(MHdf, "try-error") || inherits(designdf, "try-error")) return(NULL)
                if (input$cutoff > 0 & input$cutoff <= 300) {
                    getSplitInputFile(MHdf, designdf, input$columnType, input$method, input$cutoff)
                }
            })
            return(data)
    })
}

downloadExampleFilesUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$hr(),
        tags$h2("Input example"),
        tags$p("An example of MassHunter and design files is provided below."),
        downloadLink(
            outputId=ns("download"),
            label="Download example files"
        )
    )
}

downloadExampleFilesServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$download <- downloadHandler(
            filename = function() {
                paste("example_input_files.zip", sep="")
            },
            content = function(con) {
                file.copy("data/example_input_files.zip", con)
            }
        )
    })
}

# Main panel--------------------------------------------------------------------
displayInputSummaryUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h2("Summary"),
        textOutput(
            outputId=ns("text1")
        ),
        textOutput(
            outputId=ns("text2")
        ),
        br(),
        dataTableOutput(
            outputId=ns("dt")
        )
    )
}

displayInputSummaryServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data(), {
            summary <- getSummary(data())
            
            output$text1 <- renderText({
                paste("Total number of compounds: ", summary$compoundNumber, sep="")
            })
            output$text2 <- renderText({
                paste("Total number of conditions: ", summary$conditionNumber, sep="")
            })
            
            output$dt <- renderDataTable(options = list(pageLength = 25), {
                summary$table
            })
        })
    })
}