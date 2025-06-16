# PROGRAM pig !two compartments

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

#-----------------------------------------------------------------------------------------
# general parameters
#-----------------------------------------------------------------------------------------

Cstart	<- 0.89; Cgrow <- 0.86; Cback <- 0.04				#mg Cd/kg feed
Fstart	<- dayw * 1.4; Fgrow1 <- dayw * 2.; Fgrow2 <- dayw *  2.6	#kg feed/d
Tgrow1	<- 3.; Tgrow2 <- 8.; Tdoseoff <-16					#feeding and exposure regimen times
tSTOP		<- 25.

alfakl	<- c(.051, .059)
betakl	<- dayw * c(12.4, 37.4)

MT0kl		<- c(8.65, 22.3)
dMTdCkl	<- c(.923, 0)
V0kl		<- c(.114, .626)
dVdtkl	<- c(.0256, .100)					# weight growth parameters

names(alfakl) <- names(betakl) <- names(MT0kl) <- names(dMTdCkl) <- names(V0kl) <- names(dVdtkl) <-
  orgnames

A0	<- 0.							#mg Cd

eps		<- .01
#nrep		<- 2

f.setpar <- function() {

	BWmin		<<- 1.4
	BWmax		<<- 305.
	Tgrowth	<<- 191.
	Pgrowth	<<- 2 
	
	Fwmin		<<- 0.089
	aLin		<<- 0.0008
	rhoF		<<- 0.9
	
	nonsys	<<- 0.052 
	
	Bw0		<<- 70.
	Qc0		<<- 7200.0 
	
	qrf		<<- 0.05 
	
	Pc		<<- 5.
	Pf		<<- 150. 
	
	Fabs		<<- .82
	Dpobck	<<- input$Dpobck
	Dpo		<<- input$Cfeed
	
	Istart	<<- input$tstart
	Istop		<<- input$tdoseoff+input$tstart
	timeTotal <<- input$tstart+input$tdoseoff+input$tstop
	
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
	
	Clc0		<<- 67.
	
	alloQ		<<- .75
	alloC		<<- .7
	
	tstop		<<- 200. + eps
	
}
f.setpar()


f.calcDFperiod <- function() {
  Dperiod	<<- c(Day1, Day2, Day3, Day4, Day5)
  Fperiod	<<- c(Fday1, Fday2, Fday3, Fday4, Fday5)
}


Ac0		<- 0.
Af0		<- 0.
Amet0		<- 0.
Aint0		<- 0.	
Intake	<- 0.


y	<- y0 <- c(Ac0, Af0, Amet0, Aint0)
ny	<- length(y)

f.Bw <- function(t) {
  return(BWmin + (BWmax - BWmin) * (1. - exp(-t/Tgrowth))^Pgrowth) }
f.Qc <- function(t) {
  return((f.Bw(t) / Bw0)^alloQ * Qc0) }
f.Qfl <- function(t) {
  return(qrf * f.Qc(t)) }
f.Clc <- function(t) {
  return((f.Bw(t) / Bw0)^alloC * Clc0) }
f.Fw <- function(t) {
  return((Fwmin + aLin * f.Bw(t)) * f.Bw(t)) }
f.Vf <- function(t) {
  return(f.Fw(t) / rhoF) }
f.Cw <- function(t) {
  return((1 - nonsys) * f.Bw(t) - f.Fw(t)) }
f.Vc <- function(t) {
  return(f.Cw(t)) }
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

f.fun <- function(t, y, pars = NULL){
  
  A	<- y[1]
  V	<- V0 + dVdt * t
  Cd	<- A / V
  MT	<- MT0 + dMTdC * Cd
  D	<-f.Intake(t) #Ifeed * (expon * Cfeed + (1 - expon) * Cback)
  intake<-f.Intake(t)
  
  deriv <- alfa * MT * D / (hbeta + D)
  result<-list(deriv)
  return(result)
  
  #res<-c(dAcdt, dAfdt, dAmet, dAint)
  #result<-list(res, Cc = Cc, Cf = Cf, intake = intake)
  #return(result) 
}


f.makeresy <- function() {
  
  yini<<-c(Ac=0, Af=0, Amet=0, Aint = 0)
  tout<<-seq(0, timeTotal, by = 1)
  ifelse(input$tdoseoff != 0,
         dosing<<-data.frame(var = c("Aint") ,time = seq(input$tstart,input$tstart+input$tdoseoff, by =1), value = c(Dpo), method = c("add")),
         dosing<<-data.frame(var = c("Aint") ,time = seq(input$tstart,input$tstart+input$tdoseoff, by =1), value = c(0), method = c("add"))
                  )
  finalResult<<-as.data.frame(ode(y = yini, times = tout,  events = list(data=dosing), func = f.fun)) #,
#	irep	<- 1
#	resy	<<- array(0, dim = c(ntimrep, ny))

#	for (n in 1:(nt - 1)) {
#		if (timind[n] == "rep") { 
#			resy[irep, ] <<- y
#			irep <- irep + 1 }
#		if (substr(timind[n], 1, 3) == "cha") { 
#			hper	<- as.numeric(substr(timind[n], 9, 9))
#			Intake <<- Fperiod[hper] }
#		if (timind[n] == "Istart") {
#			Dpo	<<- Dpo0 }
#		if (timind[n] == "Istop") {
#			Dpo	<<- 0 }
#		hres	<- ode(y, timval[c(n, n + 1)], f.fun)
#		y	<- hres[-1, -1]
#	}
}

f.makeresy()








	







