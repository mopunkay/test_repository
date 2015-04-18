# load packages
library(twitteR)
library(httpuv)
library(tm)
library(SnowballC)
library(RWeka)
library(RWekajars)
library(rJava)
library(ggplot2)

# authenticate twitter account
setup_twitter_oauth(consumer_key = "", consumer_secret = "")

# pull tweets and create corpus
#example.tweets.search <- searchTwitter('@delta', n=200)
nba.tweets <- userTimeline("nbastats", n = 200)
tweets.df <- do.call("rbind", lapply(nba.tweets, as.data.frame))
tweets.corpus <- Corpus(VectorSource(tweets.df$text))

# clean up text in corpus for more efficient analysis
tweets.corpus <- tm_map(tweets.corpus, content_transformer(tolower))
tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
tweets.corpus <- tm_map(tweets.corpus, removeNumbers)
removeURLs <- function(x) {
	return(gsub("http[[:alnum:]]*", "", x))
}
tweets.corpus <- tm_map(tweets.corpus, content_transformer(removeURLs))
myStopWords <- c(stopwords("english"), "available", "via")
myStopWords <- setdiff(myStopWords, c("ball", "basketball"))
tweets.corpus <- tm_map(tweets.corpus, removeWords, myStopWords)
tweets.corpus <- tm_map(tweets.corpus, stripWhitespace)

# stem corpus
tweets.corpus.st <- tm_map(tweets.corpus, stemDocument)
#tweets.corpus.st <- tm_map(tweets.corpus.st, stemCompletion, 
#														dictionary = tweets.corpus)

# create document-term matrix out of corpus
tweets.dtm <- DocumentTermMatrix(tweets.corpus.st)

# do some analysis
## freqs
findFreqTerms(tweets.dtm, lowfreq = 20)
termFreq <- colSums(as.matrix(tweets.dtm))
termFreq <- termFreq[termFreq >= 20]
qplot(names(termFreq), termFreq, main = "Term Frequencies", geom = "bar", 
      stat = "identity", xlab = "Terms") + coord_flip()
##assocs
findAssocs(tweets.dtm, "spur", 0.25)
## wordcloud
tweets.m <- as.matrix(tweets.dtm)
freq.sort <- sort(colSums(tweets.m), decreasing = TRUE)
color.levels <- gray((freq.sort + 10) / (max(freq.sort) + 10))
wordcloud(names(freq.sort), freq.sort, min.freq = 3, random.order = FALSE, 
          colors = color.levels)
          


