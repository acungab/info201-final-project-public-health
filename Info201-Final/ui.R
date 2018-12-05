library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Population Overview in the US"),
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
             ),
             tabPanel("Bar Chart", plotOutput("plot"),
                      
                      ##This widget will change the color of the bar on the graph and will be in side panel
                      sidebarPanel(width = 20,
                                   radioButtons("color_scheme",
                                                label = "What color?",
                                                choices = c("red", "Green", "Blue"))))
                                   
                              
  )
  
)

  
    
    