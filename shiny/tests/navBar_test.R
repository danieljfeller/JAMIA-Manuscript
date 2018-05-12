library(shiny)
library(gplots)
library(plyr)
library(dplyr)
library(d3heatmap)
require(d3heatmap)
library(tableHTML)
library(shinythemes)
library(plotly)

# read in data for clustering
df <- read.csv("shiny_df.csv", as.is=T)

ui <- navbarPage(theme = shinytheme("slate"), # set css theme; (background)
  tabPanel(" "),
  tabPanel("Analytics",
                tags$head(tags$style(HTML("
                                          svg.xaxis text {
                                          fill: grey;
                                          }
                                          "))),
                fluidRow(
                  column(4,
                         selectInput("scale",
                                     "Macronutrient Scale",
                                     c("Grams" = "grams", 
                                       "% Calories" = "% total calories", 
                                       "% Grams" = "% total grams"))),
                  column(4,
                         selectInput("fiber",
                                     "Fiber:",
                                     c("-" = "Ignore", "Grams" = "grams", "Grams / Calories" = "grams / calories", "Grams Fiber / Grams Carbs" = "grams fiber / grams carbs"))),
                  column(4,
                         selectInput("outcome",
                                     "Analysis Type:",
                                     c("Ranking w/ Glucose Change" = "Blood Glucose Change - Ordered", "Clustering" = "Not Included", "Clustering w/ Glucose Change" = "Blood Glucose Change - Unordered", 
                                       "Clustering w/ Glucose Change (Quartiles)" = "Glucose Change - Unordered (Quantiles)")))
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
                                     value = 5)),
                  column(4,
                         radioButtons("colors",
                                      "Color Scheme",
                                      c("Monochromatic" = "monochromatic", "Diverging" = "diverging"),
                                            inline = TRUE)),
                  column(4,
                  radioButtons("calories",
                               "Insert Calories",
                               choice = c("No" = "do not insert", "Yes" = "insert"),
                               inline = TRUE))
                ),
                fluidRow(textOutput("columns")),
                # Create a new row for the table.
                fluidRow(
                  d3heatmapOutput("heatmap",
                                  height = 500)
                )
                ),
  tabPanel("Breakfast",
                    sidebarLayout(
                      sidebarPanel(h3("Recommended Composition:"),
                           sliderInput("calories_breakfast", label = h4("Calories"), min = 0, 
                                       max = 1500, value = c(500)),
                           sliderInput("carbs_breakfast", label = h4("Carbohydres"), min = 0, 
                                       max = 100, value = c(40)),
                           sliderInput("protein_breakfast", label = h4("Protein"), min = 0, 
                                       max = 50, value = c(14)),
                           sliderInput("fiber_breakfast", label = h4("Fiber"), min = 0, 
                                       max = 30, value = c(15))),
                      mainPanel(
                        plotlyOutput("breakfast1", height = "140px"),
                        plotlyOutput("breakfast2"),
                        checkboxGroupInput("breakfast2_meals",
                                           label = "Meals",
                                           choices = c("Breakfast", "Lunch", "Dinner"),
                                           selected = c("Breakfast"),
                                           inline=TRUE)))),
  tabPanel("Lunch & Dinner",
           sidebarLayout(
             sidebarPanel(h3("Recommended Composition:"),
               sliderInput("calories_dinner", label = h4("Calories"), min = 0, 
                           max = 1500, value = c(700)),
               sliderInput("carbs_dinner", label = h4("Carbohydres"), min = 0, 
                           max = 100, value = c(60)),
               sliderInput("protein_dinner", label = h4("Protein"), min = 0, 
                           max = 50, value = c(20)),
               sliderInput("fiber_dinner", label = h4("Fiber"), min = 0, 
                           max = 30, value = c(20))),
             mainPanel(
               plotlyOutput("dinner1", height = "140px"),
               plotlyOutput("dinner2"),
               checkboxGroupInput("dinner2_meals",
                                  label = "Meals",
                                  choices = c("Breakfast", "Lunch", "Dinner"),
                                  selected = c("Lunch", "Dinner"),
                                  inline=TRUE))))
)

server <- function(input, output) {
  # Filter data based on selections
  output$heatmap <- renderD3heatmap({
    # select meals
    select_meals <- function(original_data){
      selection <- input$meals
      return(original_data[original_data$mealType %in% selection, ])
    }
    data0 <- select_meals(df)
    # select & transform columns using the input$scale 
    select_scale <- function(original_data, scale){
      if (scale == "grams") {cols <- c('grams_carbs','grams_protein','grams_fat')}
      else if  (scale == "% total calories") {cols <- c('pcalories_carbs', 'pcalories_protein', 'pcalories_fat')}
      else if (scale == "% total grams") {cols <- c('pgrams_carbs', 'pgrams_protein', 'pgrams_fat')}
      else {print(error)}
      new_data <- original_data[ ,cols]
      return(new_data)
    }
    data1 <- select_scale(data0, input$scale)
    
    # insert calories columns
    select_calories <- function(modified_data, original_data){
      if (input$calories == "insert") {calories <- original_data$calories
      new_data <- cbind(calories, modified_data)}
      else {new_data <- modified_data}
      return(new_data)
    }
    data1.1 <- select_calories(data1, data0)
    
    # insert column that reflects the input$fiber attribute 
    select_fiber <- function(modified_data, original_data, fiber){
      if (fiber == "grams") {new_data <- cbind(modified_data, original_data$grams_fiber)}
      else if (fiber == "grams / calories") {new_data <- cbind(modified_data, original_data$gc_fiber)}
      else if (fiber == "grams fiber / grams carbs") {new_data <- cbind(modified_data, original_data$gfiber_gcarb)}
      else {new_data <- modified_data}
      return(new_data)
    }
    data2 <- select_fiber(data1.1, data0, input$fiber)
    
    # insert blood glucose column
    select_glucose <- function(modified_data, original_data, BG){
      if (BG == "Blood Glucose Change - Unordered") {new_data <- cbind(modified_data, original_data$BGchange)}
      else if (BG == "Blood Glucose Change - Ordered") {new_data <- cbind(rep(0, nrow(modified_data)),
                                                                          cbind(modified_data, original_data$BGchange))}
      else if (BG == "Blood Glucose Change - Unordered (Quantiles)") {new_data <- cbind(modified_data, original_data$BGquantile)}
      else {new_data <- modified_data}
      return(new_data)
    }
    data3 <- select_glucose(data2, data0, input$outcome)
    
    # perform max/min normalization
    normalize <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                          min(x, na.rm=TRUE))} # min/max normalization
    data4 <- as.data.frame(apply(t(data3), 1, FUN=normalize)) 
    
    # perform clustering
    dist_df <- dist(data4, method = "euclidean")
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
    data5 <- mutate_df(data4)
    
    # select color scheme
    select_colors <- function(x){
      if (x == "monochromatic"){colors <- c("#DAEDFF","#C5D9F1","#9FC3F1","#8DB4E2","#4B84CF","#4270AF","#365D8E","#16365C","#0F243E","#000000")}
      else if (x == "diverging"){colors <- rev(c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))}
      else {colors <- "reds"}
      return(colors)
    }
    color_scheme <- select_colors(input$colors)
    
    plot <- function(data){
      if (input$outcome == "Blood Glucose Change - Ordered") {
        d3heatmap(x =  data,
                  #cellnote = data4,
                  yaxis_font_size = "0pt",
                  colors = color_scheme,
                  key = FALSE,
                  Rowv = NULL,
                  Colv = NULL,
                  scale= "column",
                  #show_grid = FALSE,
                  xaxis_font_size = "10pt", 
                  labCol = revalue(names(data3), c("rep(0, nrow(modified_data))" = " ", "BGchange" = "BG Change", "grams_carbs" = "Carbs", "grams_protein" = "Protein",     
                                                   "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "pcalories_carbs" = "Carbs", "pcalories_protein" = "Protein",  
                                                   "pcalories_fat" = "Fat", "pgrams_carbs" = "Carbs", "pgrams_protein" = "Protein", "pgrams_fat" ="Fat", "total_carbs" ="Carbs",        
                                                   "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "BG Change", "original_data$BGquantile" = "BG Change",
                                                   "original_data$BGchange" = "BG Change")))
      }
      else if (input$outcome == "Blood Glucose Change - Unordered"){
        d3heatmap(x = data,
                  #cellnote = data4,
                  Colv = NULL,
                  Rowv = as.dendrogram(hc),
                  scale= "column",
                  colors = color_scheme,
                  key = FALSE,
                  k_row = input$clusters,
                  #show_grid = FALSE,
                  yaxis_font_size = "0pt",
                  xaxis_font_size = "10pt", 
                  labCol = revalue(names(data3), c("rep(0, nrow(modified_data))" = " ", "BGchange" = "BG Change", "grams_carbs" = "Carbs", "grams_protein" = "Protein",     
                                                   "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "pcalories_carbs" = "Carbs", "pcalories_protein" = "Protein",  
                                                   "pcalories_fat" = "Fat", "pgrams_carbs" = "Carbs", "pgrams_protein" = "Protein", "pgrams_fat" ="Fat", "total_carbs" ="Carbs",        
                                                   "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "BG Change", "original_data$BGquantile" = "BG Change",
                                                   "original_data$BGchange" = "BG Change")))
      }
      else {
        d3heatmap(x = data,
                  #cellnote = data4,
                  Colv = NULL,
                  Rowv = as.dendrogram(hc),
                  scale= "column",
                  colors = color_scheme,
                  key = FALSE,
                  #show_grid = FALSE,
                  k_row = input$clusters,
                  yaxis_font_size = "0pt",
                  xaxis_font_size = "10pt", 
                  labCol = revalue(names(data3), c("rep(0, nrow(modified_data))" = " ", "BGchange" = "BG Change", "grams_carbs" = "Carbs", "grams_protein" = "Protein",     
                                                   "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "pcalories_carbs" = "Carbs", "pcalories_protein" = "Protein",  
                                                   "pcalories_fat" = "Fat", "pgrams_carbs" = "Carbs", "pgrams_protein" = "Protein", "pgrams_fat" ="Fat", "total_carbs" ="Carbs",        
                                                   "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "BG Change", "original_data$BGquantile" = "BG Change",
                                                   "original_data$BGchange" = "BG Change")))
      }
    }
    # plot the heatmap
    plot(data5)
  })
  output$breakfast1 <- renderPlotly({
    calories_breakfast <- density(df[df$mealType %in% input$breakfast2_meals, ]$calories)
    plot_ly(x = ~calories_breakfast$x, y = ~calories_breakfast$y, type = 'scatter', mode = 'lines', name = 'Carbs', fill = 'tozeroy') %>%
      layout(xaxis = list(title = 'Calories'),
             yaxis = list(title = 'Density'))})
  output$breakfast2 <- renderPlotly({
    plotly_breakfast <- df[df$mealType %in% input$breakfast2_meals, ]
    carbs <- density(plotly_breakfast$grams_carbs)
    fat <- density(plotly_breakfast$grams_fat)
    fiber <- density(plotly_breakfast$grams_fiber)
    protein <- density(plotly_breakfast$grams_protein)
    
    plot_ly(x = ~carbs$x, y = ~carbs$y, type = 'scatter', mode = 'lines', name = 'Carbs', fill = 'tozeroy') %>%
      add_trace(x = ~fat$x, y = ~fat$y, name = 'Fat', fill = 'tozeroy') %>%
      add_trace(x = ~fiber$x, y = ~fiber$y, name = 'Fiber', fill = 'tozeroy') %>%
      add_trace(x = ~protein$x, y = ~protein$y, name = 'Protein', fill = 'tozeroy') %>%
      layout(xaxis = list(title = 'Grams of Macronutrients'),
             yaxis = list(title = 'Density'))})
  output$dinner1 <- renderPlotly({
    calories_dinner <- density(df[df$mealType %in% input$dinner2_meals, ]$calories)
    plot_ly(x = ~calories_dinner$x, y = ~calories_dinner$y, type = 'scatter', mode = 'lines', name = 'Carbs', fill = 'tozeroy') %>%
      layout(xaxis = list(title = 'Calories'),
             yaxis = list(title = 'Density'))})
  output$dinner2 <- renderPlotly({
  plotly_dinner <- df[df$mealType %in% input$dinner2_meals, ]
  carbs <- density(plotly_dinner$grams_carbs)
  fat <- density(plotly_dinner$grams_fat)
  fiber <- density(plotly_dinner$grams_fiber)
  protein <- density(plotly_dinner$grams_protein)
  
  plot_ly(x = ~carbs$x, y = ~carbs$y, type = 'scatter', mode = 'lines', name = 'Carbs', fill = 'tozeroy') %>%
    add_trace(x = ~fat$x, y = ~fat$y, name = 'Fat', fill = 'tozeroy') %>%
    add_trace(x = ~fiber$x, y = ~fiber$y, name = 'Fiber', fill = 'tozeroy') %>%
    add_trace(x = ~protein$x, y = ~protein$y, name = 'Protein', fill = 'tozeroy') %>%
    layout(xaxis = list(title = 'Grams of Macronutrients'),
           yaxis = list(title = 'Density'))})
}

shinyApp(ui = ui, server = server)
