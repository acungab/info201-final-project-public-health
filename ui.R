
## install.packages("shiny")
## install.packages("plotrix")
library(shiny)
library(dplyr)
census <- read.csv("./data/stco-mr2010-1.csv", stringsAsFactors = FALSE)
wa <- filter(census, STNAME == "Washington")
wa_county <- as.data.frame(table(wa$CTYNAME), stringsAsFactors = FALSE)
wa_county <- as.list(wa_county$Var1)
print(wa_county)
fluidPage(
  titlePanel("Racial analysis of Washington State"),   
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
)