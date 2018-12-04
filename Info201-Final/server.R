library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggthemes)

census <- read.csv("census.csv", stringsAsFactors = F)

server <- function(input,output){
#   output$summary <- renderPrint({
#     if(census$STNAME == input$state){
#       message <- print("You are currently looking at the age distribution in", input$state)
#     }
#     if(census$CTYNAME == input$state){
#       message <- print("You are currently looking at the age distribution in", input$state)
#     }
#     return(message)
# })
    
  output$map <- renderPlot({
    
   if(input$g == "Bar Graph"){
      census <- as.data.frame(census)
      census_state <- filter(census, STNAME == input$state | CTYNAME == input$state)

    
      census_state_age <- as.data.frame(table(census_state$AGEGRP), stringsAsFactors = FALSE)
      colnames(census_state_age) <- c("Age_Group", "Number_of_People")
      chart1 <- ggplot(data = census_state_age, aes(x = Age_Group,y = Number_of_People)) +
        geom_bar(stat = "identity", width = .6, fill = "blue") +
        geom_text(aes(label = Number_of_People), vjust = -.3, size = 2.7) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
        ggtitle(paste("Age Distribution of",input$state) )
      chart1

    }
    
    if(input$g == "Pop. Pyramid"){
      census <- as.data.frame(census) 
      # if(census$STNAME == input$state){
      #   census_state <- filter(census, STNAME == input$state)
      # }
      # if(census$CTYNAME == input$state){
      #   census_state <- filter(census, CTYNAME == input$state)
      # }
      listF <- list()
      listM<-list()
      for(i in 1:18){
        bigD <- filter(census, STNAME == input$state)
        f <- filter(bigD, SEX == 2)
        m <- filter(bigD, SEX == 1)
        f <- filter(f, AGEGRP == i)
        m <- filter(m, AGEGRP == i)
        
        fem_pop <- sum(f$RESPOP)
        male_pop <- sum(m$RESPOP)
        listF[i]<- fem_pop
        listM[i] <- male_pop
      }
      dfM <- data.frame(matrix(unlist(listM), nrow=18, byrow=T))
      dfF <- data.frame(matrix(unlist(listF), nrow=18, byrow=T))
      names(dfM)<- c("pop")
      dfM$pop <- dfM$pop * -1
      names(dfF)<-c("pop")
      gender_age <- bind_rows(dfM,dfF)
      gender_age$sex <- c(replicate(18,"MALE"), replicate(18,"FEMALE"))
      gender_age$Age <- c("0-4", "5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                          "50-54", "55-59", "60-64","65-69","70-74","75-79",
                          "80-84","85 <=")
      
      gender_age$INDEX <- c(1:36)
      
      if(max(dfF$pop) < 700001){
        brks <- seq(-150000, 150000, 50000)
        lbls <-paste0(as.character(c(seq(150, 0, -50), seq(50, 150, 50))), "k")
      }
      if(max(dfF$pop > 700000)){
        brks <- seq(-1500000, 1500000, 500000)
        lbls <-paste0(as.character(c(seq(1.5, 0, -.5), seq(.5, 1.5, .5))), "m")
      }
        chart1 <- ggplot(gender_age, aes(x = reorder(Age, INDEX), y = pop, fill = sex)) +   # Fill column
        geom_bar(subset = .(sex == "FEMALE"),stat = "identity", width = .8) +   # draw the bars
        geom_bar(subset = .(sex == "MALE"),stat = "identity", width = .8) +   # draw the bars
        scale_y_continuous(breaks = brks,   # Breaks
                           labels = lbls) + # Labels
        coord_flip() +  # Flip axes
        labs(title=paste0("Age Distribution and Gender of ", input$state)) +
        # theme_tufte() +  # Tufte theme from ggfortify
        theme(plot.title = element_text(hjust = .5),
              axis.ticks = element_blank()) +   # Centre plot title
        scale_fill_brewer(palette = "Dark2")  # Color palette
      }
    chart1
  })
}


