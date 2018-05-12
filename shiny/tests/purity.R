library(fpc)
library(dendextend)
library(stats)


test0 <- df %>% select(grams_carbs, grams_protein, grams_fat, grams_fiber, BGchange)
dist_df <- dist(test0, method = "euclidean")
hc <- hclust(dist_df, method = "average")



cluster_means <- function(data, n.clusters, hclust){
  df <- data
  clusters <- cutree(hclust, k = n.clusters)
  df$clusters <- clusters
  
  reference <- data.frame(
    df %>% 
      group_by(clusters) %>%
      summarise_each(funs(mean)))
  
  return(merge(data.frame(clusters), reference, by = "clusters", sort = FALSE))
}

test <- cluster_means(df %>% select(grams_carbs, grams_protein, grams_fat, BGchange), 8, hc)


data <- df %>% select(grams_carbs, grams_protein)
clusters <- cutree(hc, k = 10)
data$clusters <- clusters

reference <- data.frame(
  data %>% 
    group_by(clusters) %>%
    summarise_each(funs(mean)))

tryt <- merge(data.frame(clusters), reference, by = "clusters")

r
