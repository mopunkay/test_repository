# CREATED 16 APRIL 2015, MODIFIED 16 APRIL 2015
# FUNCTION TO CREATE DOCUMENT TERM MATRIX OF TWEETS
# NOTE: THIS IS A STAND_ALONE FUNCTION

MakeTweetsDTM <- function(tweets, wordstokeep = NULL) {
	# Pulls a specified number of a user's tweets and 
	#   creates a document-term matrix out of them.
	# The function will search for tweets based on either the provided user or 
	#   the provided search string, so provide one and not the other.
	#
	# Args:
	#   tweetsobject: The object of twitter data to convert to a corpus.
	#   WordsToKeep: optional - Which common words will not be removed with the other 		#     stopwords.
	#
	# Returns:
	#   A corpus of the extracted tweets.
	#
	require(tm)
	require(SnowballC)
	require(RWeka)
	require(RWekajars)
	require(rJava)

	# create corpus from tweets object
	tweets.df <- do.call("rbind", lapply(tweets, as.data.frame))
	tweets.corpus <- Corpus(VectorSource(tweets.df$text))
	
	# clean up corpus text in corpus
	tweets.corpus <- tm_map(tweets.corpus, content_transformer(tolower))
	tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
	tweets.corpus <- tm_map(tweets.corpus, removeNumbers)
	removeURLs <- function(x) {
		return(gsub("http[[:alnum:]]*", "", x))
	}
	tweets.corpus <- tm_map(tweets.corpus, content_transformer(removeURLs))
	myStopWords <- c(stopwords("english"), "available", "via")
	myStopWords <- myStopWords[!myStopWords %in% wordstokeep]
	tweets.corpus <- tm_map(tweets.corpus, removeWords, myStopWords)
	tweets.corpus <- tm_map(tweets.corpus, stripWhitespace)
	
	# stem corpus
	tweets.corpus.st <- tm_map(tweets.corpus, stemDocument)
	
	# create document-term matrix out of corpus
	tweets.dtm <- DocumentTermMatrix(tweets.corpus.st)
	return(tweets.dtm)
}
