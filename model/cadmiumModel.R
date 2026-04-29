source('setup.R')

#-----------------------------------------------------------------------------------------
# code of cadmium in pigs model, re-implemented from ASCL into R
# fixed dose-regime, but time of stopping the regime is user-defined
# dose size is fixed, but has to be made user-defined
# implementation follows previous model implementations
# (1) using R ode-procedure to solve DE
# (2) function that defines gradient for given time-point and values of state-variables 
# (3) ordered series of time points and related event indicator values
#-----------------------------------------------------------------------------------------

norg		<- 2
orgnames	<- c("kidney", "liver")
dayw		<- 7
i= 1

#-----------------------------------------------------------------------------------------
# general parameters
#-----------------------------------------------------------------------------------------

Cback <- input$Dpobck				#User defined in ShinyApp: mg Cd/kg feed ; 
Cstart	<- input$Cfeed #0.89; 
Cgrow <- input$Cfeed #0.86

Tstart <-input$tstart
Tdoseoff <- input$tdoseoff#+input$tstart				
tSTOP		<- input$tstart+input$tdoseoff+input$tstop


alfakl	<- c(.051, .059)
betakl	<- dayw * c(12.4, 37.4)

MT0kl		<- c(8.65, 22.3)
dMTdCkl	<- c(.923, 0)
V0kl		<- c(.114, .626)
dVdtkl	<- c(.0256, .100)					# weight growth parameters

ML<- c(1.,.5)

A0	<- 0.							#mg Cd

Fday1		<<- input$feedIntake1
Fday2		<<- input$feedIntake2
Fday3		<<- input$feedIntake3
Fday4		<<- input$feedIntake4
Fday5		<<- input$feedIntake5

Day1		<<- 0.
Day2		<<- 28.
Day3		<<- 75.
Day4		<<- 125.
Day5		<<- 175.


f.Intake <- function(t) {
  if (input$selected_species == "Pig;"){
    IfeedValuec<-  ifelse(t>=28, ((0.0246*(t)^2+9.9013*t+330.44))*0.88/1000,
                          ifelse(t <28, 0,0))
  } else if ( input$selected_species == "feedIngredients") {
    IfeedValuec<-ifelse(t>=28, ((0.0246*(t)^2+9.9013*t+330.44))*0.88/1000*input$percentage_ingredient/100,
                        ifelse(t <28, 0,0))
  } else if ( input$selected_species == "wetMixes") {
    IfeedValuec<-ifelse(t>=66, ((-0.0496*(t)^2+32.379*(t)-911.2))*0.25/1000,
                        ifelse(t <66, 0,0))
  } else if ( input$selected_species == "other") {
    IfeedValuec<-  if ( t<=Day2) {
      Fday1
    } else if ( t>Day2 & t<=Day3) {
      Fday2
    } else if ( t>Day3 & t<=Day4 ) {
      Fday3
    } else if ( t>Day4 & t<=Day5 ) {
      Fday4
    } else {
      Fday5
    }
  } else {
    IfeedValuec<- filter(compoundfeed_data, 
                         AnimalSubSpecies == "Meat pig", 
                         Year == 2016, 
                         ProductSubCategory== input$selected_product) %>% 
      select("compoundFeedMeatPigIntake") %>% 
      unique(.)%>% 
      as.numeric(.$compoundFeedSummerIntake[1])
  }
  return(IfeedValuec)}

#-----------------------------------------------------------------------------------------
# DERIVATIVE FUNCTION USED FOR SOLVING DIFFERENTIAL EQUATIONS
#-----------------------------------------------------------------------------------------

f.fun <- function(t, y, pars) {
  with(as.list(c(y, pars)), {
    intake<-f.Intake(t)
    V	<- V0 + dVdt * t
    Cd	<- A / V
    MT	<- MT0 + dMTdC * Cd
    A	<- alfa * MT * D*f.Intake(t) / (hbeta + D*f.Intake(t))  
    D	<--D +dayw*Cback*f.Intake(t)
    res<-c(A,V,D)
    result<-(list(res, conc= Cd, intake = intake))
    })
}

#-----------------------------------------------------------------------------------------
# SOLVING DIFFERENTIAL EQUATIONS FOR GIVEN ORGAN, TIME-OF-STOPPING-DOSE, AND ORGAN NAME

# note: all parameters used within the derivative function are defined globally

f.model0 <- function(i) {
  alfa		<<- alfakl[i]
  hbeta		<<- betakl[i]
  MT0		<<- MT0kl[i]
  dMTdC		<<- dMTdCkl[i]
  V0		<<- V0kl[i]
  dVdt		<<- dVdtkl[i]
}

f.model1 <- function(Tdoseoff) {
  valuealine<-1
  yini<-c(A=0, V=0, D=0)
  
  tout<<-seq(0, tSTOP, by = 1)
  ifelse(input$tdoseoff != 0,
         dosing<<-data.frame(var = c("D") ,time = seq(input$tstart,input$tstart+input$tdoseoff, by =1), value = c(dayw*Cgrow), method = c("add")),
         dosing<<-data.frame(var = c("D") ,time = seq(input$tstart,input$tstart+input$tdoseoff, by =1), value = 0, method = c("add"))
  )
  
  hf	<- as.data.frame(ode(y = yini, times = tout, func = f.fun, events = list(data=dosing), parms = NULL))
}




f.model <- function(i, Tdoseoff) { 
  f.model0(i)
  f.model1(Tdoseoff)
  }

solution<-data.frame(compartment = 0, time = 0, A = 0, V = 0, D = 0, conc =0, intake = 0)
for (i in c(1,2)) {
  modelResult<<-f.model(i, Tdoseoff)
  compartment<-data.frame(compartment = orgnames[i],modelResult)
  solution <- rbind(solution, compartment)
  }


