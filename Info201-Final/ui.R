library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("UFO sightings in US"),
  navbarPage("",
             tabPanel("Age",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("state",
                                    "States"
                          ),
                          radioButtons("g",
                                       "Graphs",
                                       c("Pop. Pyramid","Bar Graph", "As")
                          )
                          
                        ),
                        mainPanel(
                          plotOutput("map")
                        )
                      )
             ),
             tabPanel("Summary",
                      mainPanel(
                        textOutput("summary"))
             )
  )
  
)

