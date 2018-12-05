## install.packages("dplyr")
## install.packages("shiny")
library(shiny)
library(dplyr)
census <- read.csv("./data/stco-mr2010-1.csv", stringsAsFactors = FALSE)
wa <- filter(census, STNAME == "Washington")
wa_county <- as.data.frame(table(wa$CTYNAME), stringsAsFactors = FALSE) 
wa_county <- as.list(wa_county$Var1)
fluidPage(
  titlePanel("Gender Distribution of Washington State"),   
  sidebarLayout (
    sidebarPanel(
      selectInput("countyname",
                  label = "Which county?",
                  choices = wa_county),
      radioButtons("raceoption", "Race?",
                         c("White" = 1, "Black or African Amercian" = 2,
                                "American Indian and Alaska Native" = 3, "Asian" = 4,
                                "Native Hawaiian and Other Pacific Islander" = 5) ##, 
                           ## "interracial" = 6:31)
                         )
    ),
    mainPanel(
      plotOutput("bar"),
      plotOutput("genderpie"),
      plotOutput("ethnicpie")
      )
  )
)