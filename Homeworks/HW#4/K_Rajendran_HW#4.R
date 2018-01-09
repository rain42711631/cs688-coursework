rm(list = ls())
setwd("/Users/Ravi/Documents/Fall-2016/CS688/Homeworks/HW#4/")
library("twitteR")
library("ROAuth")
library("bitops")
library("RCurl")
library("rjson")
library("tm")
library("SnowballC")
library("wordcloud")
library("tm.plugin.webmining")
library("ggplot2")
cat("\014")

# OAuth parameters
# Use your own Twitter Developer API Key and Secret
t.api.key <- "API_KEY"
t.api.secret <- "API_SECRET"
t.access.token <-"ACCESS_TOKEN"
t.access.secret <- "ACCESS_SECRET"

# Register credentials
setup_twitter_oauth(t.api.key, t.api.secret, access_token=t.access.token, access_secret=t.access.secret)

# Problem-a
# Tweets for "Massachusetts"
massachusetts.tweets <- searchTwitter("#massachusetts", n = 100)
massachusetts.tweets <- lapply(massachusetts.tweets, function(x) {x$getText()})
mode(massachusetts.tweets)

# Tweets for "California"
california.tweets <- searchTwitter("#california", n = 100)
california.tweets <- lapply(california.tweets, function(x) {x$getText()})
mode(california.tweets)

# Problem-b
# Creating a corpus for "Massachusetts"
massachusetts.tweets.corpus <- Corpus(VectorSource(massachusetts.tweets))
california.tweets.corpus <- Corpus(VectorSource(california.tweets))

# Problem-c
# Preprocessing
# Function to remove URL's
remove.url <- content_transformer(function(x) gsub("(f|ht)tp[[:alnum:][:punct:]]*", " ", x))
# Function to remove NON-ASCII's
remove.non.ascii <- content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=""))

# Preprocessing the corpus for "Massachusetts"
massachusetts.tweets.temp <- massachusetts.tweets.corpus
massachusetts.tweets.temp <- tm_map(massachusetts.tweets.temp, remove.url) # Remove URL's
massachusetts.tweets.temp <- tm_map(massachusetts.tweets.temp, removeNumbers) # Remove numbers
massachusetts.tweets.temp <- tm_map(massachusetts.tweets.temp, removePunctuation) # Remove Punctuations
massachusetts.tweets.temp <- tm_map(massachusetts.tweets.temp, removeWords, stopwords("english")) # Remove stopwords
# massachusetts.tweets.temp <- tm_map(massachusetts.tweets.temp, stemDocument)
massachusetts.tweets.temp <- tm_map(massachusetts.tweets.temp, remove.non.ascii) # Remove Non-ASCII's
massachusetts.tweets.temp <- tm_map(massachusetts.tweets.temp, content_transformer(tolower)) # Convert to lower case
massachusetts.tweets.temp <- tm_map(massachusetts.tweets.temp, stripWhitespace) # Strip all the unwanted white spaces

# Preprocessing the corpus for "California"
california.tweets.temp <- california.tweets.corpus
california.tweets.temp <- tm_map(california.tweets.temp, remove.url) # Remove URL's
california.tweets.temp <- tm_map(california.tweets.temp, removeNumbers) # Remove numbers
california.tweets.temp <- tm_map(california.tweets.temp, removePunctuation) # Remove Punctuations
california.tweets.temp <- tm_map(california.tweets.temp, removeWords, stopwords("english")) # Remove stopwords
california.tweets.temp <- tm_map(california.tweets.temp, remove.non.ascii) # Remove Non-ASCII's
california.tweets.temp <- tm_map(california.tweets.temp, content_transformer(tolower)) # Convert to lower case
california.tweets.temp <- tm_map(california.tweets.temp, stripWhitespace) # Strip all the unwanted white spaces

# Problem-d
# Creating a Term Document Matrix for "Massachusetts"
massachusetts.tweets.tdm <- TermDocumentMatrix(massachusetts.tweets.temp)
massachusetts.tweets.temp[[1]]$content
inspect(massachusetts.tweets.tdm[1:10, 1:10])

# Creating a Term Document Matrix for "California"
california.tweets.tdm <- TermDocumentMatrix(california.tweets.temp)
california.tweets.temp[[1]]$content
inspect(california.tweets.tdm[1:10, 1:10])

# Problem-e
# Finding the frequency terms
mass.word.frequency <- rowSums(as.matrix(massachusetts.tweets.tdm))
cal.word.frequency <- rowSums(as.matrix(california.tweets.tdm))

mass.ordered <- order(mass.word.frequency)
cal.ordered <- order(cal.word.frequency)

mass.word.frequency[tail(mass.ordered)]
cal.word.frequency[tail(cal.ordered)]

findFreqTerms(massachusetts.tweets.tdm, lowfreq = 4)
findAssocs(massachusetts.tweets.tdm, terms = "massachusetts", corlimit = 0.3)
findFreqTerms(california.tweets.tdm, lowfreq = 4)
findAssocs(california.tweets.tdm, terms = "california", corlimit = 0.3)

mass.freq.frame <- data.frame(word = names(sort(mass.word.frequency, decreasing = TRUE)), freq = sort(mass.word.frequency, decreasing = TRUE))
cal.freq.frame <- data.frame(word = names(sort(cal.word.frequency, decreasing = TRUE)), freq = sort(cal.word.frequency, decreasing = TRUE))

# Most frequent terms
barplot(mass.freq.frame[1:10,]$freq, las = 2, names.arg = mass.freq.frame[1:10,]$word, col = rgb(0,0,1,0.5), main = "Most frequent words", ylab = "Word frequencies")
barplot(cal.freq.frame[2:11,]$freq, las = 2, names.arg = cal.freq.frame[2:11,]$word, col = rgb(0,0,1,0.5), main = "Most frequent words", ylab = "Word frequencies")

# Frequent terms
ggplot(subset(mass.freq.frame, freq>4), aes(word, freq)) + geom_bar(stat = "identity") +
  ggtitle("Frequent Words for Massachusetts Tweets") + theme(axis.text.x=element_text(angle=45, size =10))

ggplot(subset(mass.freq.frame, freq>4), aes(word, freq)) + geom_bar(stat = "identity") +
  ggtitle("Frequent Words for California Tweets") + theme(axis.text.x=element_text(angle=45, size =10))

# Problem-f
# Creating a Word cloud for both states
wordcloud(names(mass.word.frequency), mass.word.frequency, min.freq = 5, colors=brewer.pal(8, "Dark2"))
wordcloud(names(cal.word.frequency), cal.word.frequency, min.freq = 5, colors=brewer.pal(8, "Dark2"))


# Problem-g
# Sentiment analysis
sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  # text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  score <- sum(pos.matches) - sum(neg.matches)
  cat("sum of pos and neg:", sum(pos.matches), sum(neg.matches), "\n")
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}

# Read positve and negative text files
positive.words <- scan('positive.words.txt', what = "character", comment.char = ';')
negative.words <- scan('negative-words.txt', what = "character", comment.char = ';')

sprintf("The positive sentiment score for massachusetts is %d", sentiment(names(mass.word.frequency), positive.words, negative.words))
sprintf("The positive sentiment score for california is %d", sentiment(names(cal.word.frequency), positive.words, negative.words))
