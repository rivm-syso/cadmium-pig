library(shiny)
library(rmarkdown)
library(dplyr)
library(deSolve)
library(ggplot2)

####Input staple feed composition####
feed_data<- read.csv(file = "data/feedDatabasePig.csv", header = TRUE) # convert to /data/feedDatabase.csv when making the docker file???
compoundfeed_intakePig<- filter(feed_data,species== "Pig;") %>% filter(.,product== "Compound feed (total)") %>% select("amount_kgdmPerDay") %>% unique(.) %>% as.numeric(.$amount_kgdmPerDay[1])
wetmixesfeed_intakePig<- filter(feed_data,species== "Pig;") %>% filter(.,product== "Wet mixes") %>% select("amount_kgdmPerDay") %>% unique(.) %>% as.numeric(.$amount_kgdmPerDay[1])


####Input compound feed composition####
compoundfeed_data<- read.csv(file = "data/2018_011_016_Composition_Feed_Pigs_EG.csv", header = TRUE) # convert to /data/feedDatabase.csv when making the docker file???
#add two colums in which the composition percentages are converted  
compoundfeed_data<-compoundfeed_data %>% mutate(compoundFeedMeatPigIntake = AnimalSubspeciesCompoundFeedIntake/100*compoundfeed_intakePig)

choices_compound <- filter(compoundfeed_data, AnimalSubSpecies== "Meat pig", Year== 2016) %>% select(ProductSubCategory) %>% rename(.,`Ingredient`   = ProductSubCategory)  

# Server logic ----
server <- function(session,input, output) {
  
  input_received<-reactive({
    if(input$age> 12){
      max
    }else{
      input$age
    }
  })
  textnote<-reactive({
    if(input$age< 12){
      paste0("<font color=\"#e05959\"><b>", "Note: your input is beyond the minimal starting age, 12 weeks will be used as input. ", "</b></font>")
    }
  })
  output$note<-renderText({
    HTML(textnote() )
  })   

# -----------Options for feed product input----------
output$products <- renderUI({
  conditionalPanel(
    condition = "input.selected_species == 'feedIngredients'",
    selectInput("selected_product", "Select:", choices_compound)
    )
})  #end renderUI
  
#absolute feed intake kg dm/day
IfeedValue<<-  reactive({
  if (input$selected_species == "Pig;"){
    IfeedValuec<-compoundfeed_intakePig
  } else if ( input$selected_species == "wetMixes") {
    IfeedValuec<-wetmixesfeed_intakePig
  } else if ( input$selected_species == "other") {
    IfeedValuec<-NULL
  } else {
    IfeedValuec<- filter(compoundfeed_data, AnimalSubSpecies == "Meat pig", Year == 2016, ProductSubCategory== input$selected_product) %>% select("compoundFeedMeatPigIntake") %>% unique(.)%>% as.numeric(.$compoundFeedSummerIntake[1])
  }
  return(IfeedValuec)
})

output$feedIntakeStart <- renderUI({
  numericInput("Fstart", "<15 weeks", value = round(IfeedValue()/1.42857,digits =2), min=0, width = "100%")
})  #end renderUI

output$feedIntake <- renderUI({
  numericInput("Fgrow1", "15-20 weeks", value = IfeedValue(), min =0 )
    })  #end renderUI

output$feedIntakeGrow2 <- renderUI({
  numericInput("Fgrow2", ">20 weeks", value = round(IfeedValue()*1.3,digits =2), min =0 )
  })  #end renderUI
  
  

  #-----------------------------------------------------------------------------------------
  # Output report
  #-----------------------------------------------------------------------------------------

output$plot <- renderPlot({
  source("cadmiumModel.R", local = TRUE)
  f.model(as.numeric(input$selected_organ), input$Tdoseoff, orgnames[as.numeric(input$selected_organ)])
})

  #http://shiny.rstudio.com/gallery/download-knitr-reports.html

  output$saveRes <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.docx",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(age = input$age, 
                     feed = input$selected_product,
                     intake1 = input$Fstart,
                     intake2 = input$Fgrow1,
                     intake3 = input$Fgrow2,
                     concCad = input$Cgrow,
                     expDuration = input$Tdoseoff,
                     deplTime =  input$tSTOP, 
                     selected_organ = input$selected_organ
                     )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  

  
  
}
