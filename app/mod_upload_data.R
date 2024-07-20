# Upload input file-------------------------------------------------------------
uploadInputFileUI <- function(id) {
    ns <- NS(id)
    tagList(
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
            label="Upload MassHunter file as CSV with comma as separator",
            accept=".csv"
        ),
        div(style = "margin-top: -20px"),
        tags$p("The MassHunter file must contain at least 5 columns:"),
        tags$ul(
            tags$li("CAS#"),
            tags$li("Compound Name"),
            tags$li("Match Factor"),
            tags$li("Component RI"),
            tags$li("File Name")
        ),
        fileInput(
            inputId=ns("designFile"),
            label="Upload design file as TSV",
            accept=".tsv"
        ),
        div(style = "margin-top: -20px"),
        tags$p("The design file sums up your experiments. It must contain 4 columns:"),
        tags$ul(
            tags$li("File Name"),
            tags$li("Condition"),
            tags$li("Replicate"),
            tags$li("Label")
        ),
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

# Display df--------------------------------------------------------------------
displayPlotUI <- function(id) {
    ns <- NS(id)
    tagList(
        dataTableOutput(
            outputId=ns("dt")
        )
    )
}

displayPlotServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        output$dt <- renderDataTable({
            data()$retained
        })
    })
}