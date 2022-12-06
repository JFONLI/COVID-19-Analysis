library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(plotly)

default.df <- as.data.frame(data(package = "datasets")["results"])
database.default <- as.character(default.df$results.Item)
