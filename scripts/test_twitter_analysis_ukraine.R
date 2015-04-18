# source function that makes DTM of tweets
source("~/test_repository/scripts/MakeTweetsDTM.R")

# load packages
library(twitteR)
library(httpuv)

# authenticate twitter account
setup_twitter_oauth(consumer_key = "", consumer_secret = "")

# pull tweets
ukr.tweets <- searchTwitter("#Ukraine", n = 200, lang = "en")

# get a corpus of tweets to analyze
ukr.dtm <- MakeTweetsDTM(tweets = ukr.tweets)


# make a wordcloud
require(wordcloud)
tweets.mat <- as.matrix(ukr.dtm)
freq.sort <- sort(colSums(tweets.mat), decreasing = TRUE)
color.levels <- gray((freq.sort + 10) / (max(freq.sort) + 10))
wordcloud(names(freq.sort), freq.sort, colors = color.levels, min.freq = 5, 
          random.order = FALSE)
