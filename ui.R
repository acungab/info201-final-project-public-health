## install.packages("dplyr")
## install.packages("shiny")
library(shiny)
library(dplyr)
census <- read.csv("./data/stco-mr2010-1.csv", stringsAsFactors = FALSE)
gender <- select(census, STNAME, CTYNAME, SEX, RESPOP)
gender_wa <- filter(gender, STNAME == "Washington")
male <- filter(gender_wa, SEX == 1)
male_county <- group_by(male, CTYNAME) %>%
  summarize(ppl_m = sum(RESPOP))
female <- filter(gender_wa, SEX == 2)
female_county <- group_by(female, CTYNAME) %>%
  summarize(ppl_f = sum(RESPOP))
gender_wa_county <- right_join(male_county,female_county, by = "CTYNAME") %>%
  mutate(ppl = ppl_m + ppl_f) %>%
  mutate(m_perc = ppl_m / ppl) %>%
  mutate(f_perc = ppl_f / ppl)
wa_county <- as.list(gender_wa_county$CTYNAME)
fluidPage(
  titlePanel("Gender Distribution of Washington State"),   
  sidebarLayout (
    sidebarPanel(
      selectInput("countyname",
                  label = "Which county?",
                  choices = wa_county),
    radioButtons("genderoption", "Which gender?", c("Male", "Female"))
    ),
    mainPanel(plotOutput("map"))
  )
)