## global ##

# libraries
library(shiny)
library(shinycssloaders)
set.seed(123)

# plotting params
det_cols = c('1'='black','0'="grey")
det_shapes = c('1'=1,'0'=4)

# functions
source('functions.R')