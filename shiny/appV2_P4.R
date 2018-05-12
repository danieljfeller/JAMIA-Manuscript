library(shiny)
library(gplots)
library(plyr)
library(dplyr)
require(dplyr)
library(d3heatmap)
require(d3heatmap)
library(tableHTML)
library(shinythemes)
library(plotly)
library(fpc)

df <- read.csv("shinyP4.csv", as.is=T) # 264 meals (80 breakfasts, 95 lunches, 89 dinners)

ui <- navbarPage(theme = shinytheme("slate"), fluid = TRUE, # set css theme; (background)
                 tabPanel(" "),
                 tabPanel("Analytics",
                          tags$head(tags$style(HTML("
                                                    svg.xaxis text {
                                                    fill: grey;
                                                    }
                                                    "))),
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          sliderInput("days", 
                                                      label = "# Days",
                                                      value = "24",
                                                      min = 1,
                                                      max = round(nrow(df)/3)),
                                          checkboxGroupInput("meals",
                                                             NULL,
                                                             choices = c("Breakfast", "Lunch", "Dinner"),
                                                             selected = c("Breakfast", "Lunch", "Dinner"),
                                                             inline = TRUE),
                                          radioButtons("calories",
                                                       "Insert Calories",
                                                       choice = c("No" = "do not insert", "Yes" = "insert"),
                                                       inline = TRUE),
                                          selectInput("scale",
                                                      "Macronutrient Scale",
                                                      c("Grams" = "grams",
                                                        "Grams w/ Recommendation" = "grams w/ rec",
                                                        "% Calories" = "% total calories", 
                                                        "% Grams" = "% total grams"),
                                                      selected = "% Grams"),
                                          selectInput("fiber",
                                                      "Fiber:",
                                                      c("-" = "Ignore", "Grams" = "grams", "Recommended" = "R grams", "% Grams" = "% grams", "Grams / Calories" = "grams / calories", "Grams Fiber / Grams Carbs" = "grams fiber / grams carbs"),
                                                      selected = "% Grams"),
                                          selectInput("outcome",
                                                      "Analysis Type:",
                                                      c("Ranking" = "Blood Glucose Change - Ordered", "Clustering" = "Blood Glucose Change - Unordered", 
                                                        "Clustering by Quartiles" = "Glucose Change - Unordered (Quantiles)", "Easy Clustering" = "Clustering - Means"),
                                                      selected = "Easy Clustering"),
                                          sliderInput("clusters",
                                                      label = "Clusters",
                                                      min = 1,
                                                      max = 25,
                                                      value = 5),
                                          h3(textOutput("purity")),
                                          radioButtons("colors",
                                                       "Color Scheme",
                                                       c("Monochromatic" = "monochromatic", "Diverging" = "diverging"),
                                                       selected = "Diverging",
                                                       inline = FALSE)
                                        ),
                                        mainPanel(d3heatmapOutput("heatmap",
                                                                  height = 650,
                                                                  width = 800))
                          )),
                 tabPanel("Explore",
                          fluidPage(
                            checkboxGroupInput("distribution_meals",
                                               label = "Meals",
                                               choices = c("Breakfast", "Lunch", "Dinner"),
                                               selected = c("Breakfast", "Lunch", "Dinner"),
                                               inline = TRUE),
                            plotlyOutput("calorie_distribution", height = "140px"),
                            plotlyOutput("macronutrient_distribution"),
                            plotlyOutput("BG_distribution", height = "140px"))),
                 tabPanel("Clustering ",
                          plotlyOutput("cluster_plot")))


server <- function(input, output) {
  
  # Filter data based on selections
  output$heatmap <- renderD3heatmap({
    # select appropriate number of weeks
    select_days <- function(original_data){
      min <- min((df %>% group_by(mealType) %>% summarise(n = n()))$n)
      days <- input$days
      if (days <= min){data <- ddply(original_data,.(mealType),function(x) x[sample(nrow(x),days),])}
      else {data <- original_data}
      return(data)}
    data0.0 <- select_days(df)
    
    # select meals
    select_meals <- function(original_data){
      selection <- input$meals
      return(original_data[original_data$mealType %in% selection, ])
    }
    data0 <- select_meals(data0.0)
    print("OK")
    
    # select columns using the input$scale 
    select_scale <- function(original_data, scale){
      if (scale == "grams") {cols <- c('grams_carbs','grams_protein','grams_fat')
      new_data <- original_data[ ,cols]}
      else if  (scale == "% total calories") {cols <- c('pcalories_carbs', 'pcalories_protein', 'pcalories_fat')
      new_data <- original_data[ ,cols]}
      else if (scale == "% total grams") {cols <- c('pgrams_carbs', 'pgrams_protein', 'pgrams_fat')
      new_data <- original_data[ ,cols]}
      else {cols <- c('Rgrams_carbs', 'Rgrams_protein', 'Rgrams_fat')
      new_data <- original_data[ ,cols]}
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
      else if (fiber == "R grams") {new_data <- cbind(modified_data, original_data$Rgrams_fiber)}
      else if (fiber == "% grams") {new_data <- cbind(modified_data, original_data$pgrams_fiber)}
      else if (fiber == "grams / calories") {new_data <- cbind(modified_data, original_data$gc_fiber)}
      else if (fiber == "grams fiber / grams carbs") {new_data <- cbind(modified_data, original_data$gfiber_gcarb)}
      else {new_data <- modified_data}
      return(new_data)
    }
    data2 <- select_fiber(data1.1, data0, input$fiber)
    
    # insert blood glucose column
    select_glucose <- function(modified_data, original_data, BG){
      if (BG == "Blood Glucose Change - Unordered") {new_data <- cbind(modified_data, original_data$BGchange)}
      else if (BG == "Blood Glucose Change - Ordered") {new_data <- cbind(modified_data, original_data$BGchange)}
      else if (BG == "Blood Glucose Change - Unordered (Quantiles)") {new_data <- cbind(modified_data, original_data$BGquantile)}
      else {new_data <- cbind(modified_data, original_data$BGchange)}
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
    
    # prepare matrix of base64 images
    select_images <- function(x, final_data){
      n = dim(final_data)[2]
      matrix(rep(x,each=n), ncol=n)
    }
    image_matrix <- select_images(data0$photo, data5)
    
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
        if (input$scale == "grams w/ rec"){
          d3heatmap(x =  data,
                    cellnote = image_matrix,
                    yaxis_font_size = "0pt",
                    colors = color_scheme,
                    key = FALSE,
                    Rowv = NULL,
                    Colv = NULL,
                    scale= "none",
                    #show_grid = FALSE,
                    xaxis_font_size = "10pt", 
                    labCol = revalue(names(data3), c("rep(0, nrow(modified_data))" = " ", "original_data$BGchange" = "BG Change", "grams_carbs" = "Carbs", "grams_protein" = "Protein",     
                                                     "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "pcalories_carbs" = "Carbs", "pcalories_protein" = "Protein",  
                                                     "pcalories_fat" = "Fat", "pgrams_carbs" = "Carbs", "pgrams_protein" = "Protein", "pgrams_fat" ="Fat", "total_carbs" ="Carbs",        
                                                     "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "BG Change", "original_data$BGquantile" = "BG Change",
                                                     "original_data$pgrams_fiber" = "Fiber")))
        }
        else{
          d3heatmap(x =  data,
                    cellnote = image_matrix,
                    yaxis_font_size = "0pt",
                    colors = color_scheme,
                    key = FALSE,
                    Rowv = NULL,
                    Colv = NULL,
                    scale= "column",
                    #show_grid = FALSE,
                    xaxis_font_size = "10pt", 
                    labCol = revalue(names(data3), c("rep(0, nrow(modified_data))" = " ", "original_data$BGchange" = "BG Change", "grams_carbs" = "Carbs", "grams_protein" = "Protein",     
                                                     "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "pcalories_carbs" = "Carbs", "pcalories_protein" = "Protein",  
                                                     "pcalories_fat" = "Fat", "pgrams_carbs" = "Carbs", "pgrams_protein" = "Protein", "pgrams_fat" ="Fat", "total_carbs" ="Carbs",        
                                                     "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "BG Change", "original_data$BGquantile" = "BG Change",
                                                     "original_data$pgrams_fiber" = "Fiber")))
        }
      }
      else if (input$outcome == "Blood Glucose Change - Unordered"){
        if (input$scale == "grams w/ rec") {
          d3heatmap(x = data,
                    cellnote = image_matrix,
                    Colv = NULL,
                    Rowv = as.dendrogram(hc),
                    scale= "none",
                    colors = color_scheme,
                    key = FALSE,
                    k_row = input$clusters,
                    #show_grid = FALSE,
                    yaxis_font_size = "0pt",
                    xaxis_font_size = "10pt", 
                    labCol = revalue(names(data3), c("rep(0, nrow(modified_data))" = " ", "original_data$BGchange" = "BG Change", "grams_carbs" = "Carbs", "grams_protein" = "Protein",     
                                                     "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "pcalories_carbs" = "Carbs", "pcalories_protein" = "Protein",  
                                                     "pcalories_fat" = "Fat", "pgrams_carbs" = "Carbs", "pgrams_protein" = "Protein", "pgrams_fat" ="Fat", "total_carbs" ="Carbs",        
                                                     "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "BG Change", "original_data$BGquantile" = "BG Change",
                                                     "original_data$pgrams_fiber" = "Fiber")))
        }
        else {
          d3heatmap(x = data,
                    cellnote = image_matrix,
                    Colv = NULL,
                    Rowv = as.dendrogram(hc),
                    scale= "column",
                    colors = color_scheme,
                    key = FALSE,
                    k_row = input$clusters,
                    #show_grid = FALSE,
                    yaxis_font_size = "0pt",
                    xaxis_font_size = "10pt", 
                    labCol = revalue(names(data3), c("rep(0, nrow(modified_data))" = " ", "original_data$BGchange" = "BG Change", "grams_carbs" = "Carbs", "grams_protein" = "Protein",     
                                                     "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "pcalories_carbs" = "Carbs", "pcalories_protein" = "Protein",  
                                                     "pcalories_fat" = "Fat", "pgrams_carbs" = "Carbs", "pgrams_protein" = "Protein", "pgrams_fat" ="Fat", "total_carbs" ="Carbs",        
                                                     "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "BG Change", "original_data$BGquantile" = "BG Change",
                                                     "original_data$pgrams_fiber" = "Fiber")))
        }
      }
      else if (input$outcome == "Clustering - Means") {
        cluster_means <- function(table, n.clusters, hclust){
          table1 <- data.frame(table)
          clusters <- cutree(hclust, k = n.clusters)
          table1$clusters <- clusters
          
          reference <- data.frame(
            table1 %>% 
              group_by(clusters) %>%
              summarise_each(funs(mean))) 
          
          return(merge(data.frame(clusters), reference, by = "clusters", sort = FALSE) %>% select(-clusters))
        }
        means <- cluster_means(data, input$clusters, hc)
        
        d3heatmap(x = means,
                  cellnote = image_matrix,
                  Colv = NULL,
                  scale= "column",
                  colors = color_scheme,
                  key = FALSE,
                  show_grid = TRUE,
                  #show_grid = FALSE,
                  k_row = input$clusters,
                  yaxis_font_size = "0pt",
                  xaxis_font_size = "10pt",
                  labCol = revalue(names(data3), c("rep(0, nrow(modified_data))" = " ", "original_data$BGchange" = "BG Change", "grams_carbs" = "Carbs", "grams_protein" = "Protein",     
                                                   "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "pcalories_carbs" = "Carbs", "pcalories_protein" = "Protein",  
                                                   "pcalories_fat" = "Fat", "pgrams_carbs" = "Carbs", "pgrams_protein" = "Protein", "pgrams_fat" ="Fat", "total_carbs" ="Carbs",        
                                                   "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "BG Change", "original_data$BGquantile" = "BG Change",
                                                   "original_data$pgrams_fiber" = "Fiber")))
      }
      else {
        d3heatmap(x = data,
                  cellnote = image_matrix,
                  Colv = NULL,
                  Rowv = as.dendrogram(hc),
                  scale= "column",
                  colors = color_scheme,
                  key = FALSE,
                  #show_grid = FALSE,
                  k_row = input$clusters,
                  yaxis_font_size = "0pt",
                  xaxis_font_size = "10pt", 
                  labCol = revalue(names(data3), c("rep(0, nrow(modified_data))" = " ", "original_data$BGchange" = "BG Change", "grams_carbs" = "Carbs", "grams_protein" = "Protein",     
                                                   "grams_fat" = "Fat",  "original_data$grams_fiber" = "Fiber", "calories" ="Calories", "pcalories_carbs" = "Carbs", "pcalories_protein" = "Protein",  
                                                   "pcalories_fat" = "Fat", "pgrams_carbs" = "Carbs", "pgrams_protein" = "Protein", "pgrams_fat" ="Fat", "total_carbs" ="Carbs",        
                                                   "original_data$gc_fiber" = "Fiber",   "original_data$gfiber_gcarb" = "Fiber", "BGchange0" = "BG Change", "original_data$BGquantile" = "BG Change",
                                                   "original_data$pgrams_fiber" = "Fiber")))
      }
    }
    plot(data5)
  })
  output$purity <- renderText({
    # Filter data based on selections
    # select appropriate number of weeks
    select_days <- function(original_data){
      min <- min((df %>% group_by(mealType) %>% summarise(n = n()))$n)
      days <- input$days
      if (days <= min){data <- ddply(original_data,.(mealType),function(x) x[sample(nrow(x),days),])}
      else {data <- original_data}
      return(data)}
    data0.0 <- select_days(df)
    
    # select meals
    select_meals <- function(original_data){
      selection <- input$meals
      return(original_data[original_data$mealType %in% selection, ])
    }
    data0 <- select_meals(data0.0)
    print("OK")
    
    # select columns using the input$scale 
    select_scale <- function(original_data, scale){
      if (scale == "grams") {cols <- c('grams_carbs','grams_protein','grams_fat')
      new_data <- original_data[ ,cols]}
      else if  (scale == "% total calories") {cols <- c('pcalories_carbs', 'pcalories_protein', 'pcalories_fat')
      new_data <- original_data[ ,cols]}
      else if (scale == "% total grams") {cols <- c('pgrams_carbs', 'pgrams_protein', 'pgrams_fat')
      new_data <- original_data[ ,cols]}
      else {cols <- c('Rgrams_carbs', 'Rgrams_protein', 'Rgrams_fat')
      new_data <- original_data[ ,cols]}
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
      else if (fiber == "R grams") {new_data <- cbind(modified_data, original_data$Rgrams_fiber)}
      else if (fiber == "% grams") {new_data <- cbind(modified_data, original_data$pgrams_fiber)}
      else if (fiber == "grams / calories") {new_data <- cbind(modified_data, original_data$gc_fiber)}
      else if (fiber == "grams fiber / grams carbs") {new_data <- cbind(modified_data, original_data$gfiber_gcarb)}
      else {new_data <- modified_data}
      return(new_data)
    }
    data2 <- select_fiber(data1.1, data0, input$fiber)
    
    # insert blood glucose column
    select_glucose <- function(modified_data, original_data, BG){
      if (BG == "Blood Glucose Change - Unordered") {new_data <- cbind(modified_data, original_data$BGchange)}
      else if (BG == "Blood Glucose Change - Ordered") {new_data <- cbind(modified_data, original_data$BGchange)}
      else if (BG == "Blood Glucose Change - Unordered (Quantiles)") {new_data <- cbind(modified_data, original_data$BGquantile)}
      else {new_data <- cbind(modified_data, original_data$BGchange)}
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
    
    # compute R2 or 'node purity'
    get_R2 <- function(distances, hclust, k){
      if (k == 1){
        R2 <- 0
      }
      else{
        clusters_min <- cutree(hclust, k = 2)
        within_ss_total <- cluster.stats(distances, clusters_min)$within.cluster.ss
        clusters_k <- cutree(hclust, k = k) #replace with input$clusters
        within_ss_k <- cluster.stats(distances, clusters_k)$within.cluster.ss
        R2 <- (within_ss_total-within_ss_k)/(within_ss_total)
      }
      return(R2)
    }
    R2 <- round(get_R2(dist_df, hc, input$clusters), digits = 3)
    paste(c("Purity: ", R2))
  })
  output$calorie_distribution <- renderPlotly({
    df <- df[df$mealType %in% input$distribution_meals, ]
    
    cal <- density(df$calories)
    plot_ly(x = ~cal$x, y = ~cal$y, type = 'scatter', mode = 'lines', name = 'Calories', fill = 'tozeroy') %>%
      layout(xaxis = list(title = 'Calories'),
             yaxis = list(title = '% Meals'))})
  output$BG_distribution <- renderPlotly({
    df <- df[df$mealType %in% input$distribution_meals, ]
    
    BG <- density(df$BGchange)
    plot_ly(x = ~BG$x, y = ~BG$y, type = 'scatter', mode = 'lines', name = 'Blood Glucose', fill = 'tozeroy') %>%
      layout(xaxis = list(title = 'Blood Glucose'),
             yaxis = list(title = '% Meals'))})
  output$macronutrient_distribution <- renderPlotly({
    meals <- df[df$mealType %in% input$distribution_meals, ]
    
    carbs <- density(meals$grams_carbs)
    fat <- density(meals$grams_fat)
    fiber <- density(meals$grams_fiber)
    protein <- density(meals$grams_protein)
    
    plot_ly(x = ~carbs$x, y = ~carbs$y, type = 'scatter', mode = 'lines', name = 'Carbs', fill = 'tozeroy') %>%
      add_trace(x = ~fat$x, y = ~fat$y, name = 'Fat', fill = 'tozeroy') %>%
      add_trace(x = ~fiber$x, y = ~fiber$y, name = 'Fiber', fill = 'tozeroy') %>%
      add_trace(x = ~protein$x, y = ~protein$y, name = 'Protein', fill = 'tozeroy') %>%
      layout(xaxis = list(title = 'Grams of Macronutrients'),
             yaxis = list(title = '% Meals'))})
  output$cluster_plot <- renderPlotly({
    select_days <- function(original_data){
      min <- min((df %>% group_by(mealType) %>% summarise(n = n()))$n)
      days <- input$days
      if (days <= min){data <- ddply(original_data,.(mealType),function(x) x[sample(nrow(x),days),])}
      else {data <- original_data}
      return(data)}
    data0.0 <- select_days(df)
    
    # select meals
    select_meals <- function(original_data){
      selection <- input$meals
      return(original_data[original_data$mealType %in% selection, ])
    }
    data0 <- select_meals(data0.0)
    print("OK")
    
    # select columns using the input$scale 
    select_scale <- function(original_data, scale){
      if (scale == "grams") {cols <- c('grams_carbs','grams_protein','grams_fat')
      new_data <- original_data[ ,cols]}
      else if  (scale == "% total calories") {cols <- c('pcalories_carbs', 'pcalories_protein', 'pcalories_fat')
      new_data <- original_data[ ,cols]}
      else if (scale == "% total grams") {cols <- c('pgrams_carbs', 'pgrams_protein', 'pgrams_fat')
      new_data <- original_data[ ,cols]}
      else {cols <- c('Rgrams_carbs', 'Rgrams_protein', 'Rgrams_fat')
      new_data <- original_data[ ,cols]}
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
      else if (fiber == "R grams") {new_data <- cbind(modified_data, original_data$Rgrams_fiber)}
      else if (fiber == "% grams") {new_data <- cbind(modified_data, original_data$pgrams_fiber)}
      else if (fiber == "grams / calories") {new_data <- cbind(modified_data, original_data$gc_fiber)}
      else if (fiber == "grams fiber / grams carbs") {new_data <- cbind(modified_data, original_data$gfiber_gcarb)}
      else {new_data <- modified_data}
      return(new_data)
    }
    data2 <- select_fiber(data1.1, data0, input$fiber)
    
    # insert blood glucose column
    select_glucose <- function(modified_data, original_data, BG){
      if (BG == "Blood Glucose Change - Unordered") {new_data <- cbind(modified_data, original_data$BGchange)}
      else if (BG == "Blood Glucose Change - Ordered") {new_data <- cbind(modified_data, original_data$BGchange)}
      else if (BG == "Blood Glucose Change - Unordered (Quantiles)") {new_data <- cbind(modified_data, original_data$BGquantile)}
      else {new_data <- cbind(modified_data, original_data$BGchange)}
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
    
    get_R2 <- function(distances, hclust, k){
      if (k == 1){
        R2 <- 0
      }
      else{
        clusters_min <- cutree(hclust, k = 2)
        within_ss_total <- cluster.stats(distances, 
                                         clusters_min)$within.cluster.ss # rep(1, each=nrow(distances))
        clusters_k <- cutree(hclust, k = k) #replace with input$clusters
        within_ss_k <- cluster.stats(distances, clusters_k)$within.cluster.ss
        R2 <- (within_ss_total-within_ss_k)/(within_ss_total)
      }
      return(R2)
    } # function for (TSS-ESS)/TSS
    purity_list <- c() # list with values
    for (i in 1:25){purity_list[i] <- get_R2(dist_df, hc, i)} # compute; iterate over number of clusters
    
    purity <- data.frame(purity_list)
    purity$rows <- row_number(purity)
    names(purity) <- c("Purity", "Clusters")
    
    p <- ggplot(purity, aes(Clusters, Purity))+
      geom_point()+
      geom_line()
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
