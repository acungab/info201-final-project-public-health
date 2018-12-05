library(ggplot2)
library(plotrix)
source("./ui.R")
function(input, output) {
  output$bar <- renderPlot({
    county <- filter(wa, CTYNAME == input$countyname) %>%
      filter(IMPRACE == input$raceoption)
    age_data <- group_by(county, AGEGRP) %>%
      summarize(RESPOP = sum(RESPOP))
    age_group <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    ggplot(age_data, aes(x=factor(AGEGRP), y=RESPOP)) +
      geom_bar(stat="identity", fill="cornflowerblue") +
      geom_text(aes(label=RESPOP), vjust=-0.4, size=3) +
      theme_minimal() + xlab("Age group (years)") + ylab("Population") +
      scale_x_discrete(labels = age_group) + theme_bw()
    })
  output$genderpie <- renderPlot({
    county <- filter(wa, CTYNAME == input$countyname) %>%
      filter(IMPRACE == input$raceoption)
    gender_data <- group_by(county, SEX) %>%
      summarize(RESPOP = sum(RESPOP))
    gender_slice <- as.vector(gender_data$RESPOP)
    gender_pct <- round(gender_slice/sum(gender_slice)*100, digits = 2)
    gender_group <- paste0(c("Male: ", "Female: "), gender_pct, "%")
    pie3D(gender_slice, labels = gender_group, col=c("green", "pink"), explode=0.1)
  })
  output$gendertext <- renderText({
    county <- filter(wa, CTYNAME == input$countyname) %>%
      filter(IMPRACE == input$raceoption)
    gender_data <- group_by(county, SEX) %>%
      summarize(RESPOP = sum(RESPOP))
    paste0("In general, there are ", gender_data[1, 2], " males and ", 
           gender_data[2, 2], " females of corresponding race in ", input$countyname, ".")
  })
  output$ethnicpie <- renderPlot({
    county <- filter(wa, CTYNAME == input$countyname) %>%
      filter(IMPRACE == input$raceoption)
    ethnic_data <- group_by(county, ORIGIN) %>%
      summarize(RESPOP = sum(RESPOP))
    ethnic_slice <- as.vector(ethnic_data$RESPOP)
    ethnic_pct <- round(ethnic_slice/sum(ethnic_slice)*100, digits = 2)
    ethnic_group <- paste0(c("Not Hispanic: ", "Hispanic: "), ethnic_pct, "%")
    pie3D(ethnic_slice, labels = ethnic_group, col=c("azure", "brown3"))
  })
}