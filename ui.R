## install.packages("dplyr")
## install.packages("shiny")
library(shiny)
library(dplyr)
census <- read.csv("./data/stco-mr2010-1.csv", stringsAsFactors = FALSE)
gender <- select(census, STNAME, CTYNAME, SEX, RESPOP)
male <- filter(gender, SEX == 1)
male_county <- group_by(male, STNAME) %>%
  summarize(ppl = sum(RESPOP))
female <- filter(gender, SEX == 2)
female_county <- group_by(female, STNAME) %>%
  summarize(ppl = sum(RESPOP))
fluidPage(
  titlePanel("Gender Distribution of the United States"),   
  sidebarLayout (
    sidebarPanel(
      selectInput("var",
                  label = "which gender?",
                  choices = c("Male", "Female"),
                  selected = "Male"), 
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    mainPanel(plotOutput("map"))
  )
)