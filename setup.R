list.of.packages <- c("shiny", "ggplot2", "ggplot2", "dplyr", "deSolve", "tidyr",
                      "plotly", "rsvg", "stringr", "shinyjs", "shinyFeedback")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

