## install.packages("plyr")
library(plyr)
library(shiny)
library(ggplot2)
library(dplyr)

census <- read.csv("./data/stco-mr2010-1.csv", stringsAsFactors = F)
usa_states <- as.data.frame(table(census$STNAME), stringsAsFactors = F)
usa_states <- as.list(usa_states$Var1)

wa <- filter(census, STNAME == "Washington")
wa_county <- as.data.frame(table(wa$CTYNAME), stringsAsFactors = FALSE)
wa_county <- as.list(wa_county$Var1)


ui <- fluidPage(
  titlePanel("Population Overview in the United States"),
  navbarPage("",
             tabPanel("Age and gender analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("state",
                                      label = "Which State?",
                                      choices = usa_states)
                        ),
                        column(4, offset = 2, 
                               radioButtons("g",
                                            "Graphs",
                                            c("Population Pyramid","Bar Graph")
                               )
                               
                        )
                      ),
                      mainPanel(
                        plotOutput("map", width = "150%", height = "500px"),
                        column(12, offset = 3,
                               textOutput("age_gender_messages")
                        )
                      )
             ),
             tabPanel("Racial analysis of Washington State",
                      sidebarLayout (
                        sidebarPanel(
                          selectInput("countyname",
                                      label = "Which county?",
                                      choices = wa_county),
                          radioButtons("raceoption", "Race?",
                                       c("White" = 1, "Black or African Amercian" = 2,
                                         "American Indian and Alaska Native" = 3, "Asian" = 4,
                                         "Native Hawaiian and Other Pacific Islander" = 5, 
                                         "White + Black or African American" = 6,
                                         "White + Asian" = 8,
                                         "Black or African American + Asian" = 11)
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Age distribution", plotOutput("bar")),
                            tabPanel("Gender distribution", plotOutput("genderpie"), textOutput("gendertext")),
                            tabPanel("Ethnic distribution", plotOutput("ethnicpie"), textOutput("ethnictext"))
                          )
                        )
                      )
             ),
             tabPanel("Hispanic Population", plotOutput("hispanic_plot"), 
                      
                      sidebarPanel(width = 20,
                                   checkboxInput("hispanic_or_not",label = "Hispanic")
                      ),
                      selectInput("hispanic_state_pick", label = "State Pick", 
                                  choices = usa_states
                      ),
                      textOutput("hispanic_messages")
             ),
             tabPanel("Race Division Chart", plotOutput("race_plot"),  
                      
                      ##This widget will change the color of the bar on the graph and will be in side panel
                      sidebarPanel(width = 20,
                                   radioButtons("color_scheme",
                                                label = "What color?",
                                                choices = c("Red", "Green", "Blue")
                                   ),   
                                   selectInput("name",
                                              label = "Which State?",
                                              choices = usa_states)
                                   
                      ),
                      textOutput("race_messages")
                      
             )
      )
)



