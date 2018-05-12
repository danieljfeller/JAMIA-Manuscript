library(d3heatmap)
library(dplyr)
library(gplots)
library(plotly)

pdata <- read.csv('shiny_df.csv') %>%
  select(grams_carbs, grams_protein, grams_fat, grams_fiber, BGchange)


# NORMALIZATION
normalize <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                 min(x, na.rm=TRUE))} # min/max normalization
raw_df <- pdata %>% # select data for heatmap 
  select(carbs, protein, fat, fiber, BGchange)
normalized_df <- as.data.frame(apply(t(raw_df), 1, FUN=normalize)) # normalize selected data
dist_df <- dist(x = raw_df, method = "euclidean") # compute distances of selected data
hc <- hclust(dist_df, method = "average") # derive histogram for selected data

# THIS is the same as 
hc_order <- raw_df[hc$order, ] # order selected data; dendogram is implicit 

d3heatmap(normalized_df,
          k_row = 6,
          col = c("#DAEDFF","#C5D9F1","#9FC3F1","#8DB4E2","#4B84CF","#4270AF","#365D8E","#16365C","#0F243E","#000000"),
          Colv = NULL)

d3heatmap(normalized_df,
          col = c("#DAEDFF","#C5D9F1","#9FC3F1","#8DB4E2","#4B84CF","#4270AF","#365D8E","#16365C","#0F243E","#000000"),
          Colv = NULL,
          k_row = 6,
          Rowv = as.dendrogram(hc))

# stacked density plot
ggplot(pdata, aes()
