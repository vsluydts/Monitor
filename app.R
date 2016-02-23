##instal necessary packages
#install.packages(c("shiny","shinyjs","dplyr","digest"))

#load necessary packages
library(shiny)
library(shinyjs)
library(digest)
library(dplyr)

#function for formatting of time - don't worry about it
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")


####define mandatory fields and asterix label#####
###and some nice looking stuff####

fieldsMandatory<-c("name", "gewas")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

####Define the fields that need to be saved in the database#####

fieldsAll<-c("name","gewas","Row","plagen","count")
responsesDir<-file.path("response")

##################################################
#############CREATE SHINY APP#####################
##################################################

shinyApp(
  ui = fluidPage(
    #this is just code already for nice looking stuff
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    
    #this is where the user interface is created
    titlePanel("Monitoring van plaagsoorten in een serre"),
 
    div(
      id = "form",
      
      selectInput("name", labelMandatory("Naam van de waarnemer"), c("","Els",  "Rob", "Amber")),
      selectInput("gewas", labelMandatory("Gewas in de serre"), c("Tomaten",  "Paprika's", "Ander")),
      sliderInput("Row", "Aantal rijen in de serre", 0, 25, 2, ticks = FALSE),
      selectInput("plagen", "Plaagsoort die geteld wordt", c("Witte Vlieg",  "Spint", "Ander")),
      numericInput("count", label = h3("Aantal"), value = 1),
      #textInput("name", "Naam van de andere soort", ""),
      actionButton("submit", "Submit", class = "btn-primary")
      )
  ),
  
  server = function(input, output, session) {
    
    observe({
      # check if all mandatory fields have a value
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
    
      # enable/disable the submit button
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      

      formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(data, timestamp = humanTime())
        data <- t(data)
        data
      })
      
      saveData <- function(data) {
        fileName <- sprintf("%s_%s.csv",
                            humanTime(),
                            digest::digest(data))
        
        write.csv(x = data, file = file.path(responsesDir, fileName),
                  row.names = FALSE, quote = TRUE)
      }
      
      # action to take when submit button is pressed
      observeEvent(input$submit, {
        saveData(formData())
      })
    })
  }
)




  




