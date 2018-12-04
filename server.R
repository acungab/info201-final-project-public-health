## install.packages("maps")
## install.packages("mapproj")
library(maps)
library(mapproj)
function(input, output) {
  output$map <- renderPlot({
    ## percent_map()
  })
}