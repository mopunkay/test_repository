library(twitteR)
library(httpuv)
library(tm)
library(SnowballC)
library(RWeka)
library(RWekajars)
library(rJava)

setup_twitter_oauth(consumer_key = "n3WHdHx6dkyHRmCgG8Esf4pfG", 
	consumer_secret = "8nchLgJlwuEl8LnfJCiamkwcTskt1EVqvYa1caHzDBqBHmvk4K")


#example.tweets.search <- searchTwitter('@delta', n=200)

nba.tweets <- userTimeline("nbastats", n = 200)
tweets.df <- do.call("rbind", lapply(nba.tweets, as.data.frame))
tweets.corpus <- Corpus(VectorSource(tweets.df$text))

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

tweets.corpus.st <- tm_map(tweets.corpus, stemDocument)
#tweets.corpus.st <- tm_map(tweets.corpus.st, stemCompletion, 
#														dictionary = tweets.corpus)

tweets.dtm <- DocumentTermMatrix(tweets.corpus.st)

findFreqTerms(tweets.dtm, lowfreq = 20)
