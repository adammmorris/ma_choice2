#install and load packages
install.packages(c("rjson","tidyr","dplyr"))
library("rjson")
library("tidyr")
library("dplyr")

#set the working directory to where you've saved the data set

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("c:/Users/hp/Desktop/movieAtts")

#read metadata_updated.json or metadata_updated.csv as data frame 
#metadata <- fromJSON(file = "metadata_updated.json")
#metadata_df <- as.data.frame(metadata)

metadata_df <- read.csv("formatted_imdb.csv")

#read tagdl.csv
tags_score <- read.csv("tagdl.csv")

#25 most relevant tags
rel_tags <- c('original','great ending','catastrophe','good soundtrack','brutality','predictable','life philosophy','destiny','culture clash','pg-13','melancholic','so bad it\'s funny','family','cinematography','social commentary','loneliness','enigmatic','childhood','fantasy world','relationships','redemption','violence','visually appealing','oscar (best direting)')
rel_tags_df <- filter(tags_score, tag %in% rel_tags)

#pivot wider tags_score
tags_score_wider <- rel_tags_df %>%
  pivot_wider(names_from = tag, values_from = score)

#the giant data set without additonal metadata
final_df = merge(metadata_df, tags_score_wider, by = "item_id")
final_df <- subset(final_df, select = -c(X))

#getting more metadata
#install & load omdbapi
devtools::install_github("hrbrmstr/omdbapi")
library(omdbapi)
library(tidyverse)

packageVersion("omdbapi")

### https://github.com/hrbrmstr/omdbapi 

#following method didn't work
#for (i in 1:length(final_df$imdbId)) {
#  genres_vec <-(get_genres(find_by_id(final_df$imdbId[i])))
#  genres <- paste(genres_vec, collapse=" ")
#  final_df$genres[i] <- genres
#}

for (i in 1:length(final_df$imdbId)) {
  genres_vec <-find_by_id(final_df$imdbId[i])%>% pull(Genre)
  imdbRatings_vec <- find_by_id(final_df$imdbId[i])%>% pull(imdbRating)
  metascore_vec <- find_by_id(final_df$imdbId[i])%>% pull(Metascore)
  genres <- paste(genres_vec[1], collapse=" ")
  imdbRatings <- paste(imdbRatings_vec[1], collapse=" ")
  metascore <- paste(metascore_vec[1], collapse=" ")
  final_df$genres[i] <- genres
  final_df$imdbRatings[i] <- imdbRatings
  final_df$metascore[i] <- metascore
}





