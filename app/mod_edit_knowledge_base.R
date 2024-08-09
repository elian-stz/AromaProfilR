# Generate template-------------------------------------------------------------
generateTemplateUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h2("Edit entries"),
        tags$ol(
            tags$li("Generate a pre-filled template TSV file"),
            textInput(
                inputId=ns("textField"),
                label=HTML("Enter CAS registry number(s) separated by spaces, or type <code>all</code> or <code>empty</code>")
            ),
            downloadButton(
                outputId=ns("download"),
                label="Download template"
            ),
            br(),br(),
            tags$li("Edit the template"),
            tags$ul(
                tags$li("Modify existing entries or add new ones"),
                tags$li("Decimal separators must be dots"),
                tags$li("LRI values must be separated by semicolons (;)"),
                tags$li(HTML("Use the <code>keep.previous</code> flag to keep previous information")),
            ),
            br(),
            tags$li("Submit the edited template file")
        )
    )
}

generateTemplateServer <- function(id) {
    moduleServer(id, function(input, output, session) {
            output$download <- downloadHandler(
                filename = function() paste(Sys.Date(), "_template.tsv", sep=""),
                content = function(con) {
                    df <- generate.prefilled.template(input$textField)
                    write.table(df, file=con, sep="\t", row.names=FALSE, quote=FALSE)
                }
            )
    })
}

# Submit template --------------------------------------------------------------
submitTemplateUI <- function(id) {
    ns <- NS(id)
    tagList(
        fileInput(
            inputId=ns("upload"),
            label="Upload the filled template",
            accept=c(".tsv", ".csv") # CSV can be tab-separated
        ),
        tags$hr()
    )
}

submitTemplateServer <- function(id) {
    moduleServer(id, function(input, output, session) {
            observeEvent(input$upload, {
                if (tools::file_ext(input$upload$name) == "tsv") {
                    df <- try(read.csv(input$upload$datapath, sep="\t", na.strings=c("", "NA")))
                    if (inherits(df, "try-error")) return(NULL)
                    colnames(df)[colnames(df) == "molecular_weight_g.mol.1"] <- "molecular_weight_g.mol-1"
                    edit.with.template(df)
                } else notification("invalid.file")
            })
    })
}

# Add entry --------------------------------------------------------------------
addSingleCASNumberUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h2("Add an entry"),
        textInput(
            inputId=ns("textField"),
            label="Enter a single CAS registry number"
        ),
        selectInput(
            inputId=ns("choice"),
            label="Select a chemical family",
            choices=c("", "Unknown", getChemicalFamilies()),
        ),
        actionButton(
            inputId=ns("submit"),
            label="Add"
        ),
        tags$hr(),
    )
}

addSingleCASNumberServer <- function(id) {
    moduleServer(id, function(input, output, session) {
            observeEvent(input$submit, {
                if (input$textField != "" & input$choice != "") {
                    shinyjs::disable("submit")
                    #tags$style(HTML(".container-fluid {cursor: wait;}"))
                    add.single.cas.number(input$textField, input$choice)
                    shinyjs::enable("submit")
                    #tags$style(HTML(".container-fluid {cursor: default;}"))
                }
            })
    })
}

# Remove CAS Numbers -----------------------------------------------------------
removeCASNumbersUI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h2("Delete entries"),
        textInput(
            inputId=ns("textField"),
            label="Enter CAS registry number(s) separated by spaces"
        ),
        actionButton(
            inputId=ns("submit"),
            label="Delete"
        )
    )
}

removeCASNumbersServer <- function(id) {
    moduleServer(id, function(input, output, session) {
            observeEvent(input$submit, {
                if (input$textField != "") remove.cas.numbers(input$textField)
            })
    })
}

# Main Panel -------------------------------------------------------------------
showLogsUI <- function(id) {
    ns <- NS(id)
    tagList(
        textOutput(
            outputId=ns("baseSize")
        ),
        tags$head(
            tags$style(HTML("#scrollableText { height: 800px; overflow: auto; }"))
        ),
        div(id="scrollableText", verbatimTextOutput(outputId=ns("logs")))
    )
}

showLogsServer <- function(id) {
    moduleServer(id, function(input, output, session) {
            output$baseSize <- renderText({
                invalidateLater(2000, session)
                paste("Number of compounds in the knowledge base: ", length(knowledge.base), sep="")
            })
            
            output$logs <- renderText({
                invalidateLater(2000, session)
                file_content <- readLines("data/knowledge_base_commit.log")
                paste(file_content, collapse="\n")
            })
    })
}