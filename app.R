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

#CSS to use in app
appCSS <-".mandatory_star { color: red; }"

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
 
    div(id = "form"),
      selectInput("name", labelMandatory("Naam van de waarnemer"), c("","Els",  "Rob", "Amber")),
      selectInput("gewas", labelMandatory("Gewas in de serre"), c("","Tomaten",  "Paprika's", "Ander")),
      sliderInput("Row", "Rijnummer", 0, 100, 2, ticks = T),
      sliderInput("VangNr", "Vangplaat of Paal nummer binnen een rij", 0,30,2,ticks=T),
      selectInput("plagen", "Plaagsoort die geteld wordt", c("Witte Vlieg",  "Spint", "Ander")),
      numericInput("count", label = "Aantal", value = 1),
      #textInput("name", "Naam van de andere soort", ""),
      actionButton("submit", "Submit", class = "btn-primary"),
    shinyjs::hidden(div(
      id="thankyou_msg",
      h2("Bedankt, uw telling is opgeslagen"),
      h3(actionLink("submit_another", "Vul een nieuwe telling in"))
          )
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
        shinyjs::hide("submit")
        shinyjs::show("thankyou_msg")
      })
      
      observeEvent(input$submit_another, {
        shinyjs::reset("name")
        shinyjs::reset("gewas")
        shinyjs::reset("Row")
        shinyjs::reset("VangNr")
        shinyjs::reset("count")
        shinyjs::show("submit")
        shinyjs::hide("thankyou_msg")
      })    
    })
  }
)




  




