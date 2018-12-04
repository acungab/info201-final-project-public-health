## install.packages("maps")
## install.packages("mapproj")
## install.packages(c("ggplot2", "ggmap", "mapdata"))
## install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(maps)
library(ggplot2)
library(ggmap)
library(mapdata)
library(mapproj)
function(input, output) {
  output$map <- renderPlot({
    wa_df <- subset(map_data("state"), region == "washington")
    wa_county <- subset(map_data("county"), region == "washington")
    wa_base <- ggplot(data = wa_df, mapping = aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.3) + 
      geom_polygon(color = "black", fill = "gray") +
      theme_nothing() + 
      geom_polygon(data = wa_county, fill = NA, color = "white") +
      geom_polygon(color = "black", fill = NA)
  })
}