# Upload input file-------------------------------------------------------------
uploadInputFileUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h2("Input"),
        radioButtons(
            inputId=ns("columnType"),
            label="Select a column type",
            choices=c("Polar", "Non-polar"),
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
            max=1000
        ),
        fileInput(
            inputId=ns("MHfile"),
            label="Upload MassHunter file as CSV with comma as column separator",
            accept=".csv"
        ),
        tags$div(style = "margin-top: -20px"),
        tags$p("The MassHunter file must contain at least 6 columns:"),
        tags$ul(
            tags$li("CAS#"),
            tags$li("Compound Name"),
            tags$li("Match Factor"),
            tags$li("Component RI"),
            tags$li("File Name"),
            tags$li("Estimated Conc.")
        ),
        fileInput(
            inputId=ns("designFile"),
            label="OPTIONAL: Upload design file as TSV or XLSX",
            accept=c(".tsv", ".csv", ".xlsx")
        ),
        tags$div(style = "margin-top: -20px"),
        tags$p("The design file sums up your experiments. It must contain 3 columns:"),
        tags$ul(
            tags$li("File Name (same levels as in the MassHunter file)"),
            tags$li("Condition"),
            tags$li("Replicate")
        ),
        br(),
        actionButton(
            inputId=ns("run"),
            label="Run"
        )
    )
}

uploadInputFileServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        data <- reactiveVal(NULL)
        
        observeEvent(input$run, {
            req(input$MHfile)
            
            # Read input
            ## MassHunter file
            MHdf <- try(read.csv(input$MHfile$datapath, na.strings=c("", "NA")))
            if (inherits(MHdf, "try-error")) data(NULL)
            ## Design file
            if (!is.null(input$designFile)) {
                file_ext <- tools::file_ext(input$designFile$name)
                designdf <- switch(file_ext,
                               "csv" = try(read.csv(input$designFile$datapath, na.strings=c("", "NA"), sep="\t")),
                               "tsv" = try(read.csv(input$designFile$datapath, na.strings=c("", "NA"), sep="\t")),
                               "xlsx" = try(openxlsx::read.xlsx(input$designFile$datapath, sheet=1)),
                               stop("Invalid file type")
                )
            } else {
                # No design file provided: create design file where one sample = one condition
                samples <- unique(MHdf$File.Name)
                designdf <- data.frame(
                    File.Name = samples,
                    Condition = samples,
                    Replicate = rep(1, length(samples))
                ) 
            }
            
            # Run analysis
            if (input$cutoff > 0 & input$cutoff <= 1000) {
                classification <- getSplitInputFile(MHdf, designdf, input$columnType, input$method, input$cutoff)
                data(classification)
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
        downloadButton(
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
        textOutput(
            outputId=ns("text3")
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
                paste("Total number of samples: ", summary$sampleNumber, sep="")
            })
            output$text3 <- renderText({
                paste("Total number of conditions: ", summary$conditionNumber, sep="")
            })
            
            output$dt <- renderDataTable(options = list(pageLength = 25), {
                summary$table
            })
        })
    })
}