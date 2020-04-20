#Application was created using Shiny. Run in RStudio using the 'Run App' button.
#Currently only able to load a field map and visualize series in two dimensions.
#Intent is to create a dynamic field report generator.
#
#April 2020.
#Kristof Govaerts, SESVanderhave.

library(shiny)
library(openxlsx)

options(shiny.maxRequestSize = 30*1024^2) #upload size = 30MB

source('visualization_functions.R')
source('data_functions.R')