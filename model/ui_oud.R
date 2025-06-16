library(shiny)

ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  fluidRow(
    column(6,
    wellPanel( 
      numericInput("Cgrow", label = HTML("<h5>  <b>Contamination level</b>  (mg cadmium per kg feed) </h5>"), value = 1, min = 0),
      hr(),
      radioButtons("selected_species", label = HTML("<h5>  <b>Feed intake</b>  (kg dry matter per day) </h5>"),
                   choices = list("Total compound feed" = "Pig;",
                                  "Wet mixes" = "wetMixes", 
                                  "Compound feed ingredients" = "feedIngredients", 
                                  "Other" = "other"), 
                   selected = "Pig;"),
      uiOutput("products"),
      splitLayout(
        uiOutput("feedIntakeStart"),
        uiOutput("feedIntake"),
        uiOutput("feedIntakeGrow2")),
      hr(),
      p(HTML("<h5>  <b>Exposure period</b> (weeks) </h5>")),
      splitLayout(
        numericInput("age", label = "Age at start", value =12, min=12),
        numericInput("Tdoseoff", label = "Duration exposure", value = 15, min = 0),
        numericInput("tSTOP", label = "Time after exposure", value = 8, min = 0, width = "300%")
      ),
      p(HTML("<h5>(minimal starting age of 12 weeks) </h5>")),
      htmlOutput("note"),
      br(),
      br(),
      p(HTML("<h5>  <b>Optional input</b></h5>")),
      numericInput("limitKidney", label =  HTML("<h5><b>Regulatory limit kidney </b>(mg Cd/kg tissue)</h5>"), value = 0, width = "100%"),
      numericInput("limitLiver", label =  HTML("<h5><b>Regulatory limit liver </b>(mg Cd/kg tissue)</h5>"), value = 0, width = "100%"),
      p(HTML("<h5>(these values will be displayed in the graphs)</h5>")),

      hr(),
         splitLayout(
         downloadButton("saveRes",strong("Report (*.docx)"))
      )),
      hr(),
      br(),
      br(),
      helpText(("Copyright RIVM & WFSR"))
    ),
    
      column(5, align = "center",
             br(),
             plotOutput("plot"),
             selectInput("selected_organ", label = NULL,  c("Liver" = 2,"Kidney" = 1), selected = 1)
      )
    
    
  )
)
