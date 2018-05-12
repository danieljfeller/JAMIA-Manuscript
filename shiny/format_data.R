library(dplyr)
library(lubridate)
library(stringr)

for (i in c("P1", "P2")){ # only viable expert evaluations
  
  df0 <- read.csv(paste0("from_python/", i, ".csv"), as.is=T) # parse.py was used to parse raw data
  names(df0) <- c("ind", "time", "grams_carbs", "grams_protein", "grams_fat", "grams_fiber", "calories", "BGchange")
  print(paste0("from_python/", i, ".csv"))
  
  time <- substring(format(as.POSIXlt('2000/1/1 00:00:00') + df0$time*60, '%H:%M:%S'), 1, 2)
  df0$mealType <- ifelse(time < 11, "Breakfast",
                         ifelse(time < 16, "Lunch", "Dinner"))
  
  # absoluate values
  df <- df0 %>%
    select(mealType, time, BGchange, grams_carbs, grams_protein, grams_fat, grams_fiber, calories) %>%
    mutate(
           pcalories_carbs = (grams_carbs * 4)/calories,
           pcalories_protein = (grams_protein * 4)/calories,
           pcalories_fat = (grams_fat * 9)/calories,
           pcalories_fat = (grams_fiber * 4)/calories,
           grams = grams_fat + grams_protein + grams_carbs + grams_fiber,
           pgrams_carbs = grams_carbs/grams,
           pgrams_protein = grams_protein/grams,
           pgrams_fat = grams_fat/grams,
           pgrams_fiber = grams_fiber/grams,
           gc_fiber = (grams_fiber/calories),
           gfiber_gcarb = (grams_fiber/grams_carbs))
  
  df$BGchange0 <- ifelse(df$BGchange < 0, 0, df$BGchange) # any decrease in BG -> 0
  df$BGquantile <- as.numeric(cut(df$BGchange, # BG changes put into quantiles
      breaks = quantile(df$BGchange, probs=seq(0,1, by=0.20)),
      labels = c("1", "2", "3", "4", "5")))
  df[is.na(df$BGquantile), ]$BGquantile <- 1 # fix one NA
  


