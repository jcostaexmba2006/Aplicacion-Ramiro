library(shiny)
## which fields are mandatory for submitting the form
fieldsMandatory <- c("note","streaming","name","match","round","drive","backend","driveopp","backendopp")
# add an asterisk to an input label
labelMandatory <- function(label) {
        tagList(
                label,
                span("*", class = "mandatory_star")
        )
}

##which fields are going to be saved
fieldsAll <- c("streaming", "name", "date","match", "round","drive","backend","driveopp","backendopp","att1","att2","note")
responsesDir <- file.path("responses")

##get current epochtime
epochTime <- function() {
        as.integer(Sys.time())
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
        format(Sys.time(), "%Y%m%d-%H%M%OS")
}
## save the data, sprintf() crea un vector caracter con un formato predefinido y digest() crea el codigo hash
saveData <- function(data) {
        fileName <- sprintf("%s_%s.csv",
                            humanTime(),
                            digest::digest(data))
        
        write.csv(x = data, file = file.path(responsesDir, fileName),
                  row.names = FALSE, quote = TRUE)
}
# load all responses into a data.frame
loadData <- function() {
        files <- list.files(file.path(responsesDir), full.names = TRUE)
        data <- lapply(files, read.csv, stringsAsFactors = FALSE)
        #data <- dplyr::rbind_all(data)
        data <- do.call(rbind, data)
        data
}


# CSS to use in the app
appCSS <-
        ".mandatory_star { color: red; }
        #error { color: red; }"

# usernames that are admins
adminUsers <- c("admin", "prof")

shinyApp(
        ui = fluidPage(
         shinyjs::useShinyjs(),
         shinyjs::inlineCSS(appCSS),
         titlePanel("APLICACION ANALISIS INTELIGENTE PARTIDOS"),
         div(id = "form",
            checkboxInput("streaming", labelMandatory("Grabacion de los datos en streaming"), TRUE),
            selectInput("name", labelMandatory("Nombre Grabador"),choices= list("monitor 1"=1,"Ramiro"),selected=1),
            dateInput("date", labelMandatory("Seleccione fecha"), value = Sys.time()),
            selectInput("match", labelMandatory("Selecciona el tipo de partido"), 
                       choices = list("World Padel Tour" = 1, "Entrenamiento" = 2), 
                       selected = 1),
            radioButtons("round", labelMandatory("Selecciona la ronda del torneo"),
                        choices = list("Dieciseisavos" = 5, "Octavos" = 4, "Cuartos" = 3, "Semifinales" = 2, "Final" = 1), 
                        selected = 3),
            labelMandatory("Pareja a analizar"),
            checkboxInput("drive", "Sanyo Gutierrez",TRUE),
            checkboxInput("backend", "Paquito Navarro", TRUE),
            h3("Pareja contraria"),
            selectInput("driveopp", labelMandatory("Jugador derecha"), 
                       choices = list("Lima" = 1, "Mieres" = 2), 
                       selected = 1),
            selectInput("backendopp", labelMandatory("Jugador reves"), 
                       choices = list("Belasteguin" = 1, "Lamperti" = 2), 
                       selected = 1),
            sliderInput("att1", h3("Atributo numerico"), 0, 25, 2, ticks = FALSE),
            selectInput("att2", h3("Atributo 2"),
                       c("",  "Windows", "Mac", "Linux")),
            textInput("note", labelMandatory("Espacio para comentarios")),
            actionButton("submit", "Submit", class = "btn-primary"),
            br(),br()
         ), 
         ## correct process in the middle the form and error message
         shinyjs::hidden(
                 span(id = "submit_msg", "Submitting the message"),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )
         ),        
         
         # hide the form and show the thank you message
         shinyjs::hidden(
                 div(
                         id = "thankyou_msg",
                         h3("Gracias, los datos se han grabado correctamente!"),
                         actionLink("submit_another", "Submit another response")
                 )
                ),
         ##user control of download and datatable
         uiOutput("adminPanelContainer")
         
        ),
        server = function(input, output, session) {
                # Enable the Submit button when all mandatory fields are filled out
                observe({
                        mandatoryFilled <-
                                vapply(fieldsMandatory,
                                       function(x) {
                                               !is.null(input[[x]]) && input[[x]] != ""
                                       },
                                       logical(1))
                        mandatoryFilled <- all(mandatoryFilled)
                        
                        shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
                })
                ## create a vector with de data of the input form, y have to traspone in order to be one row one observation
                formData <- reactive({
                        data <- sapply(fieldsAll, function(x) input[[x]])
                        data <- c(data, timestamp = epochTime())
                        data <- t(data)
                        data
                })
                
                
                # action to take when submit button is pressed, formData goes with parenthesis becouse is reactive
                observeEvent(input$submit, {
                        # User-experience stuff
                        shinyjs::disable("submit")
                        shinyjs::show("submit_msg")
                        shinyjs::hide("error")
                        
                        # Save the data (show an error message in case of error)
                        tryCatch({
                                saveData(formData())
                                shinyjs::reset("form")
                                shinyjs::hide("form")
                                shinyjs::show("thankyou_msg")
                        },
                        error = function(err) {
                                shinyjs::html("error_msg", err$message)
                                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
                        },
                        finally = {
                                shinyjs::enable("submit")
                                shinyjs::hide("submit_msg")
                        })
                })
               # submit another response
                 observeEvent(input$submit_another, {
                        shinyjs::show("form")
                        shinyjs::hide("thankyou_msg")
                }) 
              # render the admin panel
                 output$adminPanelContainer <- renderUI({
                         if (!isAdmin()) return()
                         br()
                         wellPanel(
                                 h2("Previous responses (only visible to admins)"),
                                 downloadButton("downloadBtn", "Download responses"), br(), br(),
                                 DT::dataTableOutput("responsesTable") 
                         )
                 })
                 # determine if current user is admin
                 isAdmin <- reactive({
                         is.null(session$user) || session$user %in% adminUsers
                 })    
                 # Show the responses in the admin table
                 output$responsesTable <- DT::renderDataTable(
                         loadData(),
                         rownames = FALSE,
                         options = list(searching = FALSE, lengthChange = FALSE)
                 )
                 # Allow user to download responses
                 output$downloadBtn <- downloadHandler(
                         filename = function() { 
                                 sprintf("mimic-google-form_%s.csv", humanTime())
                         },
                         content = function(file) {
                                 write.csv(loadData(), file, row.names = FALSE)
                         }
                 )    
                
                 
        }
)