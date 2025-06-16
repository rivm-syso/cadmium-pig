library(shiny)
#library(ggplot2)
library(rmarkdown)
#library(dplyr)
library(deSolve)
#library(tidyr)
library(knitr)
library(plotly)
library(rsvg)
#library(stringr)
library(shinyjs)
library(shinyFeedback)
library(tidyverse)



####Input staple feed composition####
feed_data<- read.csv(file = "data/feedDatabasePig.csv", header = TRUE) # convert to /data/feedDatabase.csv when making the docker file???
compoundfeed_intakePig<- filter(feed_data,species== "Pig;") %>% filter(.,product== "Compound feed (total)") %>% select("amount_kgdmPerDay") %>% unique(.) %>% as.numeric(.$amount_kgdmPerDay[1])
wetmixesfeed_intakePig<- filter(feed_data,species== "Pig;") %>% filter(.,product== "Wet mixes") %>% select("amount_kgdmPerDay") %>% unique(.) %>% as.numeric(.$amount_kgdmPerDay[1])

####Input compound feed composition####
compoundfeed_data<- read.csv(file = "data/2018_011_016_Composition_Feed_Pigs_EG.csv", header = TRUE) # convert to /data/feedDatabase.csv when making the docker file???
#add two colums in which the composition percentages are converted  
compoundfeed_data<-compoundfeed_data %>% mutate(compoundFeedMeatPigIntake = AnimalSubspeciesCompoundFeedIntake) #/100*compoundfeed_intakePig

choices_compound <- filter(compoundfeed_data, AnimalSubSpecies== "Meat pig", Year== 2016) %>% select(ProductSubCategory) %>% rename(.,`Compound feed`   = ProductSubCategory)  
productChoicesCompoundFeed<-list(
  `Compound feed` = choices_compound)


## server.R ##
function(input, output,session) {
  
  output$products <- renderUI({
    conditionalPanel(
      condition = "input.selected_species == 'feedIngredients'",
      selectInput("selected_product", "Select:", choices_compound)
    )
  })  #end renderUI
  
  IfeedValue<-  reactive({
    if (input$selected_species == "feedIngredients"){
      IfeedValuec<<- filter(compoundfeed_data, AnimalSubSpecies == "Meat pig", Year == 2016, ProductSubCategory== input$selected_product) %>% 
        select("compoundFeedMeatPigIntake") %>% 
        unique(.) %>% 
        as.numeric(.$compoundFeedSummerIntake[1]) 
    } else {
      IfeedValuec<-1
    }
    return(IfeedValuec)
  })
  output$percIngredient <- renderUI({
    conditionalPanel(
      condition = "input.selected_species == 'feedIngredients'",
      numericInput("percentage_ingredient", "% in compound feed", value = IfeedValue(), min=0, width = "100%") #IfeedValue()*100
    )#
  })  #end renderUI
  
  
 
  output$feed1 <- renderUI({
    conditionalPanel(
      condition = "input.selected_species == 'other'",
      numericInput("feedIntake1", "Day 0-28", value = 0, min=0, width = "100%")
    )
  })  #end renderUI
  output$feed2 <- renderUI({
    conditionalPanel(
      condition = "input.selected_species == 'other'",
      numericInput("feedIntake2", "Day 29-75", value = 1, min=0, width = "100%")
    )
  })  #end renderUI
  
  output$feed3 <- renderUI({
    conditionalPanel(
      condition = "input.selected_species == 'other'",
      numericInput("feedIntake3", "Day 76-125", value = 1.6, min=0, width = "100%")
    )
  })  #end renderUI
  
  output$feed4<- renderUI({
    conditionalPanel(
      condition = "input.selected_species == 'other'",
      numericInput("feedIntake4",  "Day 126-175", value = 2.1, min=0, width = "100%")
    )
  })  #end renderUI
  
  output$feed5<- renderUI({
    conditionalPanel(
      condition = "input.selected_species == 'other'",
      numericInput("feedIntake5",  "Day >175", value = 2.6, min=0, width = "100%")
    )
  })  #end renderUI
  

  
  contaminationLevel<-eventReactive(input$run, {input$Cfeed
  })
  
  
  observeEvent(input$feedIntake1|
                 input$feedIntake2|
                 input$feedIntake3|
                 input$feedIntake4|
                 input$feedIntake5|
                 input$tdoseoff|
                 input$tstop|
                 input$limitFat|
                 input$Cfeed|
                 input$Dpobck,{
                   shinyFeedback::feedbackWarning("feedIntake1", input$feedIntake1 < 0, "Please add a non-negative value")  
                   shinyFeedback::feedbackWarning("feedIntake2", input$feedIntake2 < 0, "Please add a non-negative value")  
                   shinyFeedback::feedbackWarning("feedIntake3", input$feedIntake3 < 0, "Please add a non-negative value")  
                   shinyFeedback::feedbackWarning("feedIntake4", input$feedIntake4 < 0, "Please add a non-negative value") 
                   shinyFeedback::feedbackWarning("feedIntake5", input$feedIntake4 < 0, "Please add a non-negative value") 
                   shinyFeedback::feedbackWarning("tdoseoff", input$tdoseoff < 0, "Please add a non-negative value")  
                   shinyFeedback::feedbackWarning("tstop", input$tstop < 0, "Please add a non-negative value")  
                   shinyFeedback::feedbackWarning("limitFat", input$limitFat < 0, "Please add a non-negative value")  
                   shinyFeedback::feedbackWarning("Cfeed", input$Cfeed < 0, "Please add a non-negative value")  
                   shinyFeedback::feedbackWarning("Dpobck", input$Dpobck < 0, "Please add a non-negative value")  
                   
                 })
  
  
  ######output##########
  result<-eventReactive(input$run, {
    source("cadmiumModel.R", local = TRUE)
    return(solution)#f.model(i=1, Tdoseoff=Tdoseoff)
    })
  
  
  
  hlineKidney<- eventReactive(input$run, {
    if (all(c("liver", "kidney") %in% input$organName)) {
      hlineKidney = input$limitKidney
    } else if (input$organName == "liver") {
      hlineKidney = 0
    } else {
      hlineKidney = input$limitKidney
    }
  })
  
  hlineLiver<- eventReactive(input$run, {
    if (all(c("liver", "kidney") %in% input$organName)) {
      hlineLiver = input$limitLiver
    } else if (input$organName == "kidney") {
      hlineLiver = 0
    } else {
      hlineLiver = input$limitLiver
    }
  })
  
  
  
  output$plot1 <- renderPlotly ({
    prepareResultPlot2<<-result()
    prepareResultPlot1<- result() %>%
      #mutate("conc" = output) %>%
      filter(compartment != 0) %>%
      filter(compartment %in% input$organName)
      #ifelse(is.null(input$organName), filter(compartment == 0), filter(compartment %in% input$organName)) 
      #mutate("time" = timval) %>%
      #mutate(compartment = "kidney")
      #gather(compartment, conc, Ac:Cf.Af) %>%
      #filter(
      #  if (all(c("central", "fat") %in% input$CentralFat)) { 
      #    compartment  %in% c("Cc.Ac", "Cf.Af")
      #  } else if (input$CentralFat ==  "central"){
      #    compartment  %in% c("Cc.Ac")
      #  } else if (input$CentralFat ==  "fat"){
      #    compartment  %in% c("Cf.Af")
      #  } else { 1
      #    compartment  %in% c("Cf.Af", "Cc.Ac") 
      #  }
      #) %>%      
      #  mutate(compartment = ifelse(compartment == "Cc.Ac", "Central", "Body fat"))
    
    
    ylabel<- ({
      if (all(c("kidney", "liver") %in% input$organName)) {
        ylabel = "Cd concentration (mg/kg kidney or liver)" #\n
      } else if (input$organName == "kidney") {
        ylabel = "Cd concentration (mg/kg kidney)"
      } else {
        ylabel = "Cd concentration (mg/kg liver)"
      }
    })
    
    hlineKidney<-hlineKidney()
    hlineLiver<-hlineLiver()
    
    plot1 <- ggplot() + 
      {if(hlineKidney>0 )geom_hline(yintercept = hlineKidney, color = "#fdbb84")}+
      {if(hlineKidney>0 )annotate("text", x = c((input$tdoseoff+input$tstop)*0.2), y = c(hlineKidney+(max(prepareResultPlot1$conc, na.rm = TRUE)-hlineKidney)*0.07), 
                               label = c("limit kidney") , color="#fdbb84")}+
      {if(hlineLiver>0 )geom_hline(yintercept = hlineLiver, color = "#e34a33")}+
      {if(hlineLiver>0 )annotate("text", x = c((input$tdoseoff+input$tstop)*0.6), y = c(hlineLiver+(max(prepareResultPlot1$conc, na.rm = TRUE)-hlineLiver)*0.07), 
                                  label = c("limit liver") , color="#e34a33")}+
           
      labs(x="Time (days)", y= ylabel, fill="")+
      scale_x_continuous(expand=c(0,0))+ #,limits=c(0,255)
      scale_y_continuous(expand=c(0,max(prepareResultPlot1$conc)),
              limits =c(0,hlineKidney+max(prepareResultPlot1$conc, na.rm = TRUE)))+
      {if(!is.null(input$organName))geom_line(data = prepareResultPlot1, aes(time, conc, linetype = compartment))} + 
      #{if(is.null(input$organName))geom_line(data = prepareResultPlot2, aes(time, 0, linetype = compartment))} + 
      
      scale_linetype_manual(breaks = c("liver","kidney"), values = c("solid","dotted"))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.title = element_blank())
    ay <- list(
      tickfont = list(size=11.7),
      titlefont=list(size=14.6),
      overlaying = "y",
      nticks = 5,
      side = "right",
      title = "Feed intake (kg dry matter/day)"
    )
    
    
    plotly1<- if(input$intakeGraph == TRUE){
      ggplotly(plot1) %>% #, tooltip = c("time", "conc")
      layout(legend=list(title=list(text='')))%>%
      style(hoverlabel = list(bgcolor = "white"), hoveron = "text") %>%
      add_lines(x=~time, y=~intake, colors=NULL, yaxis="y2", 
                data=result(), showlegend=FALSE, inherit=FALSE)%>%
      layout(yaxis2 = ay)
    } else {
      ggplotly(plot1) %>% #, tooltip = c("time", "conc")
      layout(legend=list(title=list(text='')))%>%
      style(hoverlabel = list(bgcolor = "white"), hoveron = "text")
    }
    
    #cleanup layout labels
    for (i in 1:length(plotly1$x$data)){
      if (!is.null(plotly1$x$data[[i]]$name)){
        plotly1$x$data[[i]]$name =  gsub("[()]","",plotly1$x$data[[i]]$name)
      }
    }
    
    
  
    
       
       
    plotly1
    
    
  })
  
  
  
  
  observeEvent(input$plotly_svg, {
    shinyjs::runjs("document.getElementById('download').click();")
  })
  
  
  #https://github.com/rstudio/shiny/issues/2152
  #https://stackoverflow.com/questions/50396139/knitr-kable-text-color-not-rendering
  #https://github.com/STAT545-UBC/Discussion/issues/136
  #rmarkdown downloade of the word document
  output$download <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.docx",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      temp_dir <- tempdir()
      tempReport <- file.path(temp_dir, "report.Rmd")
      tempImage1 <- file.path(temp_dir, "outP1.png")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy(rsvg_png(charToRaw(input$plotly_svg), tempImage1), overwrite = TRUE)#, height = 290
      
      
      
      
      # Set up parameters to pass to Rmd document
      params <- list(species = input$selected_species,
                     product = input$selected_product,
                     intake1 = input$feedIntake1,
                     intake2 = input$feedIntake2,
                     intake3 = input$feedIntake3,
                     intake4 = input$feedIntake4,
                     intake5 = input$feedIntake5,
                     expDuration = input$tdoseoff, 
                     tstop = input$tstop,
                     contaminationLevel = input$Cfeed,
                     backgroundLevel = input$Dpobck
      )
      #https://stackoverflow.com/questions/57802225/how-to-pass-table-and-plot-in-shiny-app-as-parameters-to-r-markdown
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )} 
  ) #end downloadhandler
  
  outputOptions(output, "download", suspendWhenHidden = FALSE)
  
  
  
} #end server
