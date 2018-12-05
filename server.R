library(ggplot2)
library(plotrix)
source("ui.R")
function(input, output) {
  
  ##################### Age and Gender Analysis #############
  age_label <- c("0-4", "5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                 "50-54", "55-59", "60-64","65-69","70-74","75-79",
                 "80-84","85 <=")    
  
  DataMale <- function(stateInput){
    listM <- list()
    for(i in 1:18){
      bigD <- filter(census, STNAME == stateInput)
      m <- filter(bigD, SEX == 1)
      m <- filter(m, AGEGRP == i)
      male_pop <- sum(m$RESPOP)
      listM[i] <- male_pop
    }
    dfM <- data.frame(matrix(unlist(listM), nrow=18, byrow=T))
    names(dfM)<- c("Population")
    dfM$Age <- age_label     
    return(dfM)
  }
  
  DataFemale <- function(stateInput){
    listF <- list()
    for(i in 1:18){
      bigD <- filter(census, STNAME == stateInput)
      f <- filter(bigD, SEX == 2)
      f <- filter(f, AGEGRP == i)
      female_pop <- sum(f$RESPOP)
      listF[i] <- female_pop
    }
    dfF <- data.frame(matrix(unlist(listF), nrow=18, byrow=T))
    names(dfF)<- c("Population")
    dfF$Age <- age_label     
    return(dfF)
  }
  
  output$map <- renderPlot({
    
    stateInput <- input$state
    dfF <- DataFemale(stateInput)
    dfM <- DataMale(stateInput)
    
    
    if(input$g == "Bar Graph"){
      dfT <- as.data.frame(c(1:18))
      dfT$Population <- dfM$Population + dfF$Population
      dfT$INDEX <- c(1:18)
      dfT$Age <- age_label
      
      
      chart1 <- ggplot(data = dfT, aes(x = reorder(Age,INDEX),y = Population)) +
        geom_bar(stat = "identity", width = .8, fill = "cornflowerblue") +
        geom_text(aes(label = Population), vjust = -.4, size = 3) +
        theme_minimal() + xlab("Age group (years)") + ylab("Population") +
        ggtitle(paste("Age Distribution of",input$state)) + theme_bw()
    }
    
    if(input$g == "Population Pyramid"){
      dfM$Population <- dfM$Population * -1
      gender_age <- bind_rows(dfM,dfF)
      gender_age$sex <- c(replicate(18,"MALE"), replicate(18,"FEMALE"))
      gender_age$Age <- c("0-4", "5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                          "50-54", "55-59", "60-64","65-69","70-74","75-79",
                          "80-84","85 <=")
      
      gender_age$INDEX <- c(1:36)
      
      if(max(dfF$Population) < 700001){
        brks <- seq(-150000, 150000, 50000)
        lbls <-paste0(as.character(c(seq(150, 0, -50), seq(50, 150, 50))), "k")
      }else if(max(dfF$Population > 700000)){
        brks <- seq(-1500000, 1500000, 500000)
        lbls <-paste0(as.character(c(seq(1.5, 0, -.5), seq(.5, 1.5, .5))), "m")
      }
      chart1 <- ggplot(gender_age, aes(x = reorder(Age, INDEX), y = Population, fill = sex)) +   # Fill column
        geom_bar(subset = .(sex == "FEMALE"),stat = "identity", width = .8) +   # draw the bars
        geom_bar(subset = .(sex == "MALE"),stat = "identity", width = .8) +   # draw the bars
        scale_y_continuous(breaks = brks,   # Breaks
                           labels = lbls) + # Labels
        coord_flip() +  # Flip axes
        labs(title=paste0("Age Distribution and Gender of ", input$state)) +
        # theme_tufte() +  # Tufte theme from ggfortify
        theme(plot.title = element_text(hjust = .5),
              axis.ticks = element_blank()) +   # Centre plot title
        scale_fill_brewer(palette = "green1") + # Color palette
        xlab("Age group (years)") + geom_text(aes(label = Population), vjust = 0, size = 3) + theme_bw()
    }
    
    chart1
  })
  
  output$message <- renderText({
    
    stateInput <- input$state
    dfM <- DataMale(stateInput)
    dfF <- DataFemale(stateInput)
    tot <- round(sum(dfM$Population)/(sum(dfM$Population)+sum(dfF$Population)), digits = 3) * 100
    
    paste0("There are approximately ",sum(dfM$Population)  ," males and ", sum(dfF$Population),
           " females in the state of ", input$state, " thefore we can see that ", tot ,
           "% of all citiezens are male in the state of ", input$state,
           ". The age group for males with the largest number of people is between ",
           dfM[which.max(dfM$Populatio),2], "years old and in that group there is ",
           dfM[which.max(dfM$Populatio),1], " people. On the other hand, the biggest age group for females is between ",
           dfF[which.max(dfF$Populatio),2], " years old containing ",dfF[which.max(dfF$Populatio),1], 
           " people.")
  })
  
  ################## Racial Analysis Washington #############
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
  output$ethnictext <- renderText({
    county <- filter(wa, CTYNAME == input$countyname) %>%
      filter(IMPRACE == input$raceoption)
    ethnic_data <- group_by(county, ORIGIN) %>%
      summarize(RESPOP = sum(RESPOP))
    paste0("In general, there are ", ethnic_data[1, 2], " non Hispanics and ", 
           ethnic_data[2, 2], " Hispanics of corresponding race in ", input$countyname, ".")
  })
  
  ################ Hispanic Division ############
  non_hispanic_men <- filter(census, SEX == 1, ORIGIN == 1) %>% nrow()
  non_hispanic_women <- filter(census, SEX == 2, ORIGIN == 1) %>% nrow()
  
  num <- c(non_hispanic_men,non_hispanic_women)
  gender <- c("male", "female")
  
  non_hispanic_df <- data.frame(num,gender)
  non_hispanic_df <- mutate(non_hispanic_df, percentages = round(num/sum(non_hispanic_df[[1]]), digits = 3) * 100)

  hispanic_men <- filter(census, SEX == 1, ORIGIN == 2) %>% nrow()
  hispanic_women <- filter(census, SEX == 2, ORIGIN == 2) %>% nrow()
  
  num <- c(hispanic_men,hispanic_women)
  gender <- c("male", "female")
  
  hispanic_df <- data.frame(num,gender)
  hispanic_df <- mutate(hispanic_df, percentages = round(num/sum(hispanic_df[[1]]), digits = 3) * 100)
  
  
  # Here we obtain a dataset with ALL Hispanic people, or no Hispanic people at all
  pie_dataset <- reactive({
    if (input$hispanic_or_not) {
      pie_dataset <- hispanic_df
    } else {
      pie_dataset <- non_hispanic_df
    }
  })
  
  hispanic_pie <- reactive({
    ggplot(data = pie_dataset(), aes(x = "", y = num, fill = gender)) +
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = paste0(percentages, "%")), position = position_stack(vjust = 0.5)) +
      coord_polar("y", start = 0) + xlab(" ") + ylab("Population")
  })
  output$hispanic_plot <- renderPlot(hispanic_pie())
  
  ############### Race Division ###########
  filtered_ethinicity <- filter(census, IMPRACE == 1 | IMPRACE == 2 | IMPRACE == 3 | IMPRACE == 4 | IMPRACE == 5)
  state <- as.data.frame(table(filtered_ethinicity$IMPRACE), stringsAsFactors = FALSE)
  colnames(state) <- c("Race", "Number_of_People")
  state$Race[1] = "White Population"
  state$Race[2] = "Black Population"
  state$Race[3] = "American Indian Population"
  state$Race[4] = "Asian Population"
  state$Race[5] = "Native Hawaiian Population"
  
  reactive_data = reactive({
    selected_output = input$cases
    return(state[state$Race==selected_output,])
  })
  
  output$race_plot <- renderPlot({
    
    ggplot(data = state, aes(x = Race, y = Number_of_People)) +
      geom_bar(stat = 'identity', width = .4, fill = paste0("dark", input$color_scheme)) + 
      geom_text(aes(label = Race), vjust = -.3, size = 2.5) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
      ggtitle("Race Division in states") 
    
  })
}