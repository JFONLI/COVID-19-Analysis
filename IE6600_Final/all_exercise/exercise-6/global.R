library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(plotly)

#df.students <- data.frame(Course=as.character(),Country=as.character(), Students=as.numeric(), Gender=as.character())
#write.csv(df.students, df.path)
df.path <- file.path("~/Desktop/visualizationMaterials/data/students.csv")
