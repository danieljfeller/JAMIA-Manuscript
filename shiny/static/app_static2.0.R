library(shiny)
library(gplots)
library(plyr)
library(d3heatmap)
require(d3heatmap)
library(tableHTML)
library(shinythemes)

# read in data for clustering
df <- read.csv("shiny_df.csv", as.is=T)

ui <- fluidPage(theme = shinytheme("slate"), # set css theme; (background)
                fluidRow(
                  column(4,
                         selectInput("scale",
                                     "Macronutrient Scale",
                                     c("Grams" = "grams", "Calories" = "calories", "% Total Calories" = "% total calories"))),
                  column(4,
                         selectInput("fiber",
                                     "Fiber:",
                                     c("-" = "Ignore", "Grams" = "grams", "Grams / Calories" = "grams / calories", "Grams Fiber / Grams Carbs" = "grams fiber / grams carbs"))),
                  column(4,
                         selectInput("outcome",
                                     "Analysis Type:",
                                     c("Clustering" = "Not Included", "Clustering w/ Blood Glucose Change" = "Blood Glucose Change - Unordered", 
                                       "Ranking w/ Blood Glucose Change" = "Blood Glucose Change - Ordered")))
                ),
                fluidRow(
                  column(4,
                         checkboxGroupInput("meals",
                                            label = "Meals",
                                            choices = c("Breakfast", "Lunch", "Dinner"),
                                            selected = c("Breakfast", "Lunch", "Dinner"),
                                            inline = TRUE)),
                  column(4,
                         sliderInput("clusters",
                                     label = "Clusters",
                                     min = 1,
                                     max = 25,
                                     value = 5))
                ),
                fluidRow(textOutput("columns")),
                # Create a new row for the table.
                fluidRow(
                  plotOutput("heatmap",
                                  height = 600)
                )
)


server <- function(input, output) {
  # Filter data based on selections
  # ordered <- dist_df[hC$order,] # indicies added to map between indicies from hclust() to heatmap.2()
  output$heatmap <- renderPlot({
    # select meals
    select_meals <- function(original_data){
      selection <- input$meals
      return(original_data[original_data$mealType %in% selection, ])
    }
    data0 <- select_meals(df)
    # select columns that reflect input$scale attribute
    select_scale <- function(original_data, scale){
      if (scale == "grams") {cols <- c('calories', 'grams_carbs','grams_protein','grams_fat')}
      else if  (scale == "calories") {cols <- c('calories', 'calories_carbs', 'calories_protein', 'calories_fat')}
      else {cols <- c('calories', 'pcalories_carbs', 'pcalories_protein', 'pcalories_fat')}
      new_data <- original_data[ ,cols]
      return(new_data)
    }
    data1 <- select_scale(data0, input$scale)
    
    # insert column that reflects the input$fiber attribute 
    select_fiber <- function(modified_data, original_data, fiber){
      if (fiber == "grams") {new_data <- cbind(modified_data, original_data$grams_fiber)}
      else if (fiber == "grams / calories") {new_data <- cbind(modified_data, original_data$gc_fiber)}
      else if (fiber == "grams fiber / grams carbs") {new_data <- cbind(modified_data, original_data$gfiber_gcarb)}
      else {new_data <- modified_data}
      return(new_data)
    }
    
    # do max/min normalization (line 1080 in Script6.R)
    data2 <- select_fiber(data1, data0, input$fiber)
    #data2 <-  as.data.frame(apply(t(data2.0, 
    # 1, FUN=function(x)(x-min(x))/(max(x)-min(x)))))
    
    # insert blood glucose column
    select_glucose <- function(modified_data, original_data, BG){
      if (BG == "Blood Glucose Change - Unordered") {new_data <- cbind(modified_data, original_data$BGchange)}
      else if (BG == "Blood Glucose Change - Ordered") {new_data <- cbind(modified_data, original_data$BGchange)}
      else {new_data <- modified_data}
      return(new_data)
    }
    data3 <- select_glucose(data2, data0, input$outcome)
    
    dist_df <- dist(x = data3, method = "euclidean")
    hc <- hclust(dist_df, method = "average")
    
    # prepare data for the heatmap
    mutate_df <- function(x){
      if (input$outcome == "Not Included") {
        as.matrix(x[hc$order, ])
      }
      else if (input$outcome == "Blood Glucose Change - Ordered") {
        n <- nrow(x)
        as.matrix(x[order(data0$BGchange), ]) # add empty column; for formatting
      }
      else {
        as.matrix(x)
      }
    }
    
    map <- mutate_df(data3)
    #rotate <- function(x) t(apply(x, 2, rev))
    #map <- rotate(map0)
    
    plot <- function(data){
      if (input$outcome == "Blood Glucose Change - Ordered") {
        heatmap.2(x = map,
                  cexRow=0.5,
                  cexCol=0.01,
                  trace="none",
                  col = c("#DAEDFF","#C5D9F1","#9FC3F1","#8DB4E2","#4B84CF","#4270AF","#365D8E","#16365C","#0F243E","#000000"),
                  key = FALSE,
                  scale= "column",
                  Colv = NULL,
                  Rowv = NULL,
                  density.info="none",
                  dendrogram = "none")
        
        # manually set column names in heatmap
        pos2 <- structure(list(x = c(0.26, 0.87),
                               y = c(0.90, 0.90)),
                          .Names = c("x", "y"))
        
        koo <- ncol(data3)
        text(x=seq(pos2$x[1], pos2$x[2], len=koo), y=rep(pos2$y[1],koo),
             srt=50, xpd=TRUE, adj = 0, cex = 1.5,
             labels= revalue(names(data3), c("df$BGchange" = "Blood Glucose Change", "grams_carbs" = "Carbohydrates", "grams_protein" = "Protein",     
                                             "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "calories_carbs" = "Carbohydrates", "calories_protein" = "Protein",  
                                             "calories_fat" = "Fat", "pcalories_carbs" = "Carbohydrates", "pcalories_protein" = "Protein", "pcalories_fat" ="Fat", "total_carbs" ="Carbohydrates",        
                                             "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "Blood Glucose Change")))
      }
      else if (input$outcome == "Blood Glucose Change - Unordered"){
        heatmap.2(x = map,
                  cexRow=0.5,
                  cexCol=0.01,
                  trace="none",
                  col = c("#DAEDFF","#C5D9F1","#9FC3F1","#8DB4E2","#4B84CF","#4270AF","#365D8E","#16365C","#0F243E","#000000"),
                  key = FALSE,
                  scale= "column",
                  Colv = NULL,
                  density.info="none",
                  dendrogram='row')
        
        # manually set column names in heatmap
        pos2 <- structure(list(x = c(0.26, 0.87),
                               y = c(0.90, 0.90)),
                          .Names = c("x", "y"))
        
        koo <- ncol(data3)
        text(x=seq(pos2$x[1], pos2$x[2], len=koo), y=rep(pos2$y[1],koo),
             srt=50, xpd=TRUE, adj = 0, cex = 1.5,
             labels=revalue(names(data3), c("df$BGchange" = "Blood Glucose Change", "grams_carbs" = "Carbohydrates", "grams_protein" = "Protein",     
                                            "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "calories_carbs" = "Carbohydrates", "calories_protein" = "Protein",  
                                            "calories_fat" = "Fat", "pcalories_carbs" = "Carbohydrates", "pcalories_protein" = "Protein", "pcalories_fat" ="Fat", "total_carbs" ="Carbohydrates",        
                                            "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "Blood Glucose Change")))
      }
      else {
        heatmap.2(x = map,
                  cexRow=0.5,
                  cexCol=0.01,
                  trace="none",
                  col = c("#DAEDFF","#C5D9F1","#9FC3F1","#8DB4E2","#4B84CF","#4270AF","#365D8E","#16365C","#0F243E","#000000"),
                  key = FALSE,
                  scale= "column",
                  Colv = NULL,
                  density.info="none",
                  dendrogram='row')
        
        # manually set column names in heatmap
        pos2 <- structure(list(x = c(0.26, 0.87),
                               y = c(0.90, 0.90)),
                          .Names = c("x", "y"))
        
        koo <- ncol(data3)
        text(x=seq(pos2$x[1], pos2$x[2], len=koo), y=rep(pos2$y[1],koo),
             srt=50, xpd=TRUE, adj = 0, cex = 1.5,
             labels=revalue(names(data3), c("df$BGchange" = "Blood Glucose Change", "grams_carbs" = "Carbohydrates", "grams_protein" = "Protein",     
                                            "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "calories_carbs" = "Carbohydrates", "calories_protein" = "Protein",  
                                            "calories_fat" = "Fat", "pcalories_carbs" = "Carbohydrates", "pcalories_protein" = "Protein", "pcalories_fat" ="Fat", "total_carbs" ="Carbohydrates",        
                                            "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "Blood Glucose Change")))
      }
    }
    plot(map)
  })
}

shinyApp(ui = ui, server = server)

