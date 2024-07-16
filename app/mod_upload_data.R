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
            label="Upload MassHunter output file as CSV",
            accept=".csv"
        ),
        fileInput(
            inputId=ns("designFile"),
            label="Upload design file as TSV",
            accept=".tsv"
        )
    )
}

uploadInputFileServer <- function(id) {
    moduleServer(id, function(input, output, session) {
            data <- reactive({
                req(input$MHfile, input$designFile)
                MHdf <- read.csv(input$MHfile$datapath, na.strings=c(""))
                designdf <- read.csv(input$designFile$datapath, na.strings=c(""), sep="\t")
                if (input$cutoff > 0 && input$cutoff <= 300) {
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
            #stopifnot(data() == 1)
            data()$retained
        })
    })
}