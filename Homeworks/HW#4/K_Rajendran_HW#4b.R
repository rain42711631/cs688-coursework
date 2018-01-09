rm(list = ls())
library(Rfacebook)
library(httr)
library(rjson)
library(httpuv)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
cat("\014")

# Authentication
fb.app.id <- "FACEBOOK_APP_ID" #  Credentials
fb.app.secret <- "FACEBOOK_APP_SECRET" #  Credentials
fb.oauth <- fbOAuth(app_id = fb.app.id, app_secret = fb.app.secret, extended_permissions = TRUE)

# Chosen page is "Google"
posts <- getPage("Google", n=100, token=fb.oauth)
posts$message[1:10]
categories <- posts$type

# categories.type <-unique(categories)
# categories.count <- rep(0, length(unique(categories)))
# for (i in 1:length(categories)){
#   if (categories[i] == categories.type[1]){
#     categories.count[1] = categories.count[1] + 1
#   }else{
#     categories.count[2] = categories.count[2] +1
#   }
# }

# categories.count[1] <- length(grep(categories.type[1], categories))
# categories.count[2] <- length(grep(categories.type[2], categories))

categories
catg <- factor(categories)
levels(catg) <- c("Photo", "Video")
levels(catg)
barplot(table(catg), main = "Frequency of categories", xlab = "Categories", ylab = "Frequencies")


# Message of most liked post
sprintf("The most liked post is: %s ",posts$message[posts$likes_count == max(posts$likes_count)])
sprintf("# of likes of most liked post is: %d ", max(posts$likes_count))

# Message of most commented post
sprintf("The most commented post is: %s" ,posts$message[posts$comments_count == max(posts$comments_count)])
sprintf("# of comments of most commented post is %d", max(posts$comments_count))

# Comments of the most commented post
most_comments_postid <- posts[which.max(posts$comments_count),]$id
posts.comments <- getPost(most_comments_postid, n = max(posts$comments_count), token = fb.oauth)
posts.comments$comments$message[1:10]

# Creating a corpus
posts.corpus <- Corpus(VectorSource(posts.comments$comments$message))
posts.corpus[[1]]$content
posts.tmp <- posts.corpus

# Function to remove URL's
removeUrl <- content_transformer(function(x) gsub("(f|ht)tp[[:alnum:][:punct:]]*", " ", x))
# Function to remove Non Ascii characters
removeNonASCII <- content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=""))


post.processed <- tm_map(posts.tmp, removeUrl) # Remove URL's
post.processed <- tm_map(post.processed, removeNumbers) # Remove Numbers
post.processed <- tm_map(post.processed, removePunctuation) #Remove Punctuations
post.processed <- tm_map(post.processed, removeNonASCII) # Remove the NonASCII
post.processed <- tm_map(post.processed, removeWords, stopwords("english"), lazy = TRUE) # Remove the stopwords
post.processed <- tm_map(post.processed, stripWhitespace, lazy = TRUE) # Remove the whitespaces
post.processed <- tm_map(post.processed, content_transformer(tolower), lazy = TRUE) # Convert the characters to lowercase

post.processed[[1]]$content
posts.tmp[[1]]$content

# ----- Creating a Term document Matrix -----
posts.tdm <- TermDocumentMatrix(post.processed)
inspect(posts.tdm[1:10,1:10])
dim(posts.tdm)

# -----Finding the frequencies -----
word.frequencies <- rowSums(as.matrix(posts.tdm))
head(sort(word.frequencies, decreasing = TRUE), 50)

posts.word.frequencies <- data.frame(word=names(word.frequencies), freq=word.frequencies)

# ----- Barplot of the most frequent terms -----
ggplot(subset(posts.word.frequencies, freq>75), aes(word, freq)) +
  geom_bar(stat = "identity") +
  ggtitle("Most Frequent Words for Google") +
  theme(axis.text.x=element_text(angle=45,hjust = 1))

# ----- Wordcloud of the most frequent terms -----
wordcloud(words = names(word.frequencies), freq = word.frequencies, min.freq = 25, random.order = FALSE, rot.per = 0.35, colors=brewer.pal(8, "Dark2"))
