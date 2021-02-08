# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(shiny)
library(tidytext)
library(tidyverse)
netflix <- read_csv('netflix_titles.csv')
# Use a fluid Bootstrap layout
fluidPage(
  
  titlePanel(h1("Netflix Prediction Engine",h3("See what to watch on Netflix based on your selection."))),
  selectizeInput('show', 'Enter:', 
                 choices = c("Select a show", netflix$title), 
                 multiple = FALSE, 
                 options = list(maxOptions = 10, 
                                maxItems = 1)),
  verbatimTextOutput("result")
)