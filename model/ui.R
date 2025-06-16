library(shiny)
library(shinyjs)
library(plotly)

#https://dioxins-congener-hen-acc.apps.feedfoodtransfer.nl/

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  tags$head(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$style(HTML("
                  input[type=number] {
                  -moz-appearance:textfield;
                  }
                  input[type=number]::-webkit-outer-spin-button,
                  input[type=number]::-webkit-inner-spin-button {
                  -webkit-appearance: none;
                  margin: 0;
                  }
                  "))
  ), 
  # numeric input not accepting limits: https://github.com/rstudio/shiny/issues/927
  fluidRow(
    column(5,
           wellPanel(
             #p(HTML("<h5>  <b>Draft model, do not apply, site or quote</b> </h5>")),
             
             useShinyjs(),
             numericInput("Cfeed", label = HTML("<h5>  <b>Contamination level</b>  (mg cadmium per kg feed)  </h5>"), value = 1, min = 0),
             numericInput("Dpobck", label = HTML("<h5>  <b>Background level</b>  (mg cadmium per kg feed)  </h5>"), value = 0.04, min = 0),
             
             
             hr(),
             radioButtons("selected_species", label = HTML("<h5>  <b>Feed intake</b> (kg dry matter/day) </h5>"),
                          choices = list("Total compound feed*" = "Pig;",
                                         "Wet mixes*" = "wetMixes", 
                                         "Compound feed ingredients*" = "feedIngredients", 
                                         "Other feed regime" = "other"), 
                          selected = "Pig;"),
             uiOutput("products"), 
             uiOutput("percIngredient"),
             helpText("* default time-dependent intake as depicted in the figure on the right y-axis, starting at day 28 (compound feed) or at day 66 (wet mixes)."),
             splitLayout(uiOutput("feed1"),
             uiOutput("feed2"),
             uiOutput("feed3"),
             uiOutput("feed4"),
             uiOutput("feed5")),
             hr(),
             p(HTML("<h5>  <b>Exposure period</b> (days) </h5>")),
             splitLayout(
               numericInput("tstart", label = "Age at start", value =12*7, min=12),
               numericInput("tdoseoff", label = "Duration exposure", value = 15*7, min = 0),
               numericInput("tstop", label = "Time after exposure", value = 8*7, min = 0)
             ),
             hr(),
             p(HTML("<h5>  <b>Optional input</b></h5>")),
             numericInput("limitKidney", label =  HTML("<h5><b>Regulatory limit kidney </b>(mg Cd/kg tissue)</h5>"), value = 0, width = "100%"),
             numericInput("limitLiver", label =  HTML("<h5><b>Regulatory limit liver </b>(mg Cd/kg tissue)</h5>"), value = 0, width = "100%"),
             
             p(HTML("<h5>(this value will be displayed in the graph)</h5>")),
             hr(),

             br(),
             fluidRow(
               actionButton("run", "Run model",icon = icon("play", lib = "glyphicon")),
               actionButton("btn", "Report (*.docx)",icon = icon("download")), 
             
             tags$script('
                  document.getElementById("btn").onclick = function(event) {
                  var plotly_svg = 
                  Plotly.Snapshot.toSVG(document.querySelectorAll(".plotly")[0])
                  ;
                  
                 Shiny.setInputValue("plotly_svg", plotly_svg, {priority: "event"});


                };
                  ')),
             downloadButton("download", label="", style = "display:none;"),
             br(),
             br(),
             br(),
             helpText(("Copyright RIVM & WFSR"))
           )
    ),# end column ,
    column(7, 
           #adding columns to in one fluidRow to make sure that the checkboxGroupInput is diplayed closed to the graph, also when a smaller screen is used
           fluidRow(
             column(12,align = "left",

                    hr(),

                    checkboxGroupInput("organName", label = "", 
                                       choices = list("Plot liver" = "liver",
                                                      "Plot kidney" = "kidney"),
                                       selected = "kidney", inline = TRUE),
                    checkboxInput("intakeGraph", label = "Feed intake curve"),
                    plotlyOutput("plot1"),
             )))
  )
  
  )
