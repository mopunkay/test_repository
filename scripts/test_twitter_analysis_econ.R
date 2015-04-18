# CREATED 16 APRIL 2015, MODIFIED 16 APRIL 2015
# EXTRACT AND PERFORM TEXT ANALYSIS ON A SET OF TWEETS

# source function that makes DTM of tweets
source("~/test_repository/scripts/MakeTweetsDTM.R")

# load packages
library(twitteR)
library(httpuv)

# authenticate twitter account
setup_twitter_oauth(consumer_key = "", consumer_secret = "")

# pull tweets
econ.tweets <- userTimeline("theeconomist", n = 400)

# get a corpus of tweets to analyze
econ.dtm <- MakeTweetsDTM(tweets = econ.tweets, wordstokeep = c("economy", "apps"))

# make a wordcloud
require(wordcloud)
tweets.mat <- as.matrix(econ.dtm)
freq.sort <- sort(colSums(tweets.mat), decreasing = TRUE)
color.levels <- gray((freq.sort + 10) / (max(freq.sort) + 10))
wordcloud(names(freq.sort), freq.sort, colors = color.levels, min.freq = 5, 
          random.order = FALSE)

# perform and plot a hierarchical clustering
dtm.nonsparse <- removeSparseTerms(econ.dtm, sparse = 0.96)
nonsparse.mat <- t(as.matrix(dtm.nonsparse))
dist.mat <- dist(scale(nonsparse.mat))
hclust.fit <- hclust(dist.mat, method = "ward.D")
plot(hclust.fit, cex = 0.9, hang = -1, main = "Word Cluster Dendrogram")
rect.hclust(hclust.fit, k = 5)

