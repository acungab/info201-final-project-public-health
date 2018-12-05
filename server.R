library(ggplot2)
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
      geom_bar(stat="identity", fill="blue") +
      geom_text(aes(label=RESPOP), vjust=1.6, color="white", size=3) +
      theme_minimal() + xlab("Age group (years)") + ylab("Population") +
      scale_x_discrete(labels = age_group) + theme_bw()
    })
}