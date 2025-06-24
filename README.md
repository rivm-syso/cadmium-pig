# cadmium-pig
This repository hosts a transfer model for Cadmium in pigs. This transfer model is hosted as a web tool on www.feedfoodtransfer.nl

## Getting started
Before running the model scripts, you can install the required R packages by running `setup.R`

## Model structure
The transfer model is used to estimate Cadmium (Cd) concentrations in liver and kidney of pigs. It is 
assumed that the accumulation of Cd in the liver and kidney kinetically can be
described as an irreversible uptake process. The Cd uptake rate depends on the amount of
contaminated feed that is consumed, and the organ-specific metallothionein concentrations. By integrating the 
set of equations, a carryover rate (COR) can be derived and used to estimate liver and 
kidney concentrations. 

## Prerequisites
* R (tested with version 4.5.0)

* deSolve (tested with version 1.1.4)
* dplyr (tested with version 3.5.2)
* ggplot2 (tested with version 3.5.2)
* plotly (tested with version 4.10.4)
* rsvg (tested with version 2.6.2)
* shiny (tested with version 1.10.0)
* shinyFeedback (tested with version 0.4.0)
* shinyjs (tested with version 2.1.0)
* stringr (tested with version 1.5.1)
* tidyr (tested with version 1.3.1)

## Additional resources
A peer-reviewed article has been published by [Hoogenboom et al. (2015)](https://doi.org/10.1080/19440049.2014.979370), 
containing additional details on the model. 



