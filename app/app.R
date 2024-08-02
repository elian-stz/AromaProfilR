source("global.R")

ui <- fluidPage(
    title="AromaProfilR",
    includeCSS("www/global.css"),
    
    navbarPage(title="AromaProfilR", id="main", windowTitle="AromaProfilR",
               tabPanel(title="Input",
                        sidebarLayout(
                            sidebarPanel(
                                uploadInputFileUI("input"),
                                downloadExampleFilesUI("example")
                            ),
                            mainPanel(
                                displayInputSummaryUI("inputSummary")
                            )
                        )
               ),
               tabPanel(title="Output",
                        sidebarLayout(
                            sidebarPanel(
                                importExcelFileUI("ExcelOuput"),
                                importPDFUI("PdfOutput")
                            ),
                            mainPanel(
                                showPlotUI("concentrationPlots")
                            )
                        )
               ),
               tabPanel(title="Unknown compound register",
                        displayRegisterUI("register")
               ),
               tabPanel(title="Edit knowledge base",
                        sidebarLayout(
                            sidebarPanel(
                                generateTemplateUI("getTemplate"),
                                submitTemplateUI("submitTemplate"),
                                addSingleCASNumberUI("addCAS"),
                                removeCASNumbersUI("removeCAS"),
                            ),
                            mainPanel(
                                showLogsUI("displayLogs")
                            )
                        )
                ),
    )
    
)

# Wrap UI with secure_app
ui <- secure_app(ui, enable_admin = TRUE,
                 tags_top=tags$div(
                     tags$head(tags$link(rel="stylesheet", type="text/css", href="login.css")),
                     tags$img(src="/aromaprofilr.png", width=150)
                 ),
                 tags_bottom = tags$div(
                   tags$p(style="text-align: center;",
                     	  "To create an account or for any questions, please contact the PTV:"
                   ),
		           tags$p(style="text-align: center;",
		                  admin1.mail, br(),
			              admin2.mail, br(),
			              admin3.mail
                   ),
		           tags$footer(style="text-align: center;",
		               tags$img(src="/logo_inrae.jpg", width="40%"),
		               br(),br(),
		               tags$img(src="/logo_spo.png", width=75*1.25),
		               tags$img(src="/logo_pechrouge.png", width=150*1.25)
		           )
                 )
)

server <- function(input, output, session) {
    # Check credentials (authentication)
    res_auth <- secure_server(
        check_credentials = check_credentials(
            db = "./data/login_db.sqlite",
            passphrase = db.passphrase
        )
    )
    
    # Hide the tab "Edit knowledge base" to non-admin
    observe({
        if (!is.null(res_auth$user) && reactiveValuesToList(res_auth)$admin == FALSE) {
            removeTab("main", target="Edit knowledge base")
        }
    })
    
    # "Input" tab---------------------------------------------------------------
    dataSplit <- uploadInputFileServer("input")
    downloadExampleFilesServer("example")
    displayInputSummaryServer("inputSummary", dataSplit)
    
    # "Output" tab--------------------------------------------------------------
    importExcelFileServer("ExcelOuput", dataSplit)
    plots <- showPlotServer("concentrationPlots", dataSplit)
    importPDFServer("PdfOutput", plots)
    
    # "Unknown compound register" tab-------------------------------------------
    displayRegisterServer("register", dataSplit)
    
    # "Edit knowledge base" tab-------------------------------------------------
    generateTemplateServer("getTemplate")
    submitTemplateServer("submitTemplate")
    addSingleCASNumberServer("addCAS")
    removeCASNumbersServer("removeCAS")
    showLogsServer("displayLogs")
}

shinyApp(ui, server)