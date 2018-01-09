rm(list = ls())
library(tm)
library(SnowballC)
library(class)
cat("\014")
#######################################################################################
# Q-1
# R-object for dataset location
crude <- system.file("texts","crude", package = "tm")
crude

# Q-2
# Creating a corpus
reuters <- Corpus(DirSource(crude))
inspect(reuters)

# Q-3
# Number of documents in the corpus
length(reuters)

# Q-4
# Examime metadata of the third document
reuters[[3]]$meta

# Q-5
# Creating the document term matrix
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[1:10,1:10])

# Q-6
# Finding the top 5-frequent terms
head(sort(colSums(as.matrix(dtm)), decreasing = TRUE), 5)

#######################################################################################
# Q-7
# Newsgroup classification problem for 200 documents(Total of 800 including the test and train documents)
setwd("Documents/Fall-2016/CS688/20Newsgroups/")
Doc1.Train.path <- DirSource("20news-bydate-train/sci.electronics/")
Doc1.Test.path <- DirSource("20news-bydate-test/sci.electronics/")
Doc2.Train.path <- DirSource("20news-bydate-train/talk.religion.misc/")
Doc2.Test.path <- DirSource("20news-bydate-train/talk.religion.misc/")

# Creating the corpus
Doc.corpus <- Corpus(URISource(c(Doc1.Train.path$filelist[1:200],Doc1.Test.path$filelist[1:200],
                                 Doc2.Train.path$filelist[1:200],Doc2.Test.path$filelist[1:200])),
                     readerControl=list(reader=readPlain))
inspect(Doc.corpus)

Doc.corpus.temp <- tm_map(Doc.corpus, removeNumbers)
Doc.corpus.temp <- tm_map(Doc.corpus.temp, removePunctuation)
Doc.corpus.temp <- tm_map(Doc.corpus.temp, removeWords, stopwords("english"))

# Stemming the document
Doc.corpus.temp <- tm_map(Doc.corpus.temp, stemDocument)

# ---------- Creating a DTM ----------
Doc.corpus.dtm <-DocumentTermMatrix(Doc.corpus.temp,
                                                control=list(wordLengths =c(2,Inf),bounds=list(global = c(5,Inf))))
dtm.conditioned.counts <-colSums(as.matrix(Doc.corpus.dtm))

# ---------- Creating a test and train document for kNN() ----------
train.doc <- Doc.corpus.dtm[c(c(1:200), c(401:600)),]
test.doc <- Doc.corpus.dtm[c(201:400, 601:800),]

dim(Doc.corpus.dtm)
dim(train.doc)
dim(test.doc)

tags <- factor(c(rep("Science",200), rep("Religion",200)))
# kNN function is applied over the first test and train set
prob.test <- knn(train.doc, test.doc, tags, k = 2, prob = TRUE)
head(prob.test,100)

# Classification result
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test]
c <- attributes(prob.test)$prob
d <- prob.test == tags
result <- data.frame(Doc=a, Predict=b, Prob=c, Correct=d)
head(result,10)

# True Classification result
true.classifications <- sum(c)/ length(tags)
sprintf("The true classification is %.4f", true.classifications)

# Confusion Matrix
confusion.matrix <- table(matrix(tags,ncol=1), matrix(prob.test,ncol=1))

colnames(confusion.matrix) <- c("Religion(1)", "Science(0)")
rownames(confusion.matrix) <- c("Religion(1)", "Science(0)")

confusion.matrix

## Rel as positive and Sci as negative
TP <- confusion.matrix[[1,1]]
TN <- confusion.matrix[[2,2]]
FP <- confusion.matrix[[1,2]]
FN <- confusion.matrix[[2,1]]
sprintf("TP = %d, TN = %d, FP = %d, FN = %d",TP, TN, FP, FN)

# Calculating the precision, recall and f-score
precision <- TP /sum(confusion.matrix[1,]) 
recall <- TP /sum(confusion.matrix[,1])
f.score <- 2*precision*recall/ (precision+recall)
sprintf("The precision is %.4f", precision)
sprintf("The recall is %.4f", recall)
sprintf("The f-score is %.4f", f.score)

#######################################################################################
# Q-8
# Creating a corpus using only the Subject
Subject.List <-list()
for (ff in 1:length(Doc.corpus)){
  temp <- unlist(Doc.corpus[[ff]][1])
  for (ff1 in 1:length(temp)){
    Textline <- temp[ff1]
    if (grepl("Subject: ", Textline)){
      Subject.List[ff] <-gsub("Subject: ", "", Textline)
      Doc.corpus[[ff]]$meta$Subject <- gsub("Subject: ", "", Textline)
    }
  }
}

Subject.corpus <- Corpus(VectorSource(Subject.List))
head(inspect(Subject.corpus))

Subject.corpus.temp <- tm_map(Subject.corpus, removeNumbers)
Subject.corpus.temp <- tm_map(Subject.corpus.temp, removePunctuation)
Subject.corpus.temp <- tm_map(Subject.corpus.temp, removeWords, stopwords("english"))

# Stemming the document
Subject.corpus.temp <- tm_map(Subject.corpus.temp, stemDocument)

# ---------- Creating a DTM ----------
Subject.corpus.dtm <-DocumentTermMatrix(Subject.corpus.temp,
                                                control=list(wordLengths =c(2,Inf),bounds=list(global = c(5,Inf))))
subject.dtm.conditioned.counts <-colSums(as.matrix(Subject.corpus.dtm))

# ---------- Creating a test and train document for kNN() ----------
Subject.train.doc <- Subject.corpus.dtm[c(c(1:200), c(401:600)),]
Subject.test.doc <- Subject.corpus.dtm[c(201:400, 601:800),]


tags <- factor(c(rep("Science",200), rep("Religion",200)))
# kNN function is applied over the first test and train set
Subject.prob.test <- knn(Subject.train.doc, Subject.test.doc, tags, k = 2, prob = TRUE)
head(Subject.prob.test,100)

# Classification result
a <- 1:length(Subject.prob.test)
b <- levels(Subject.prob.test)[Subject.prob.test]
c <- attributes(Subject.prob.test)$prob
d <- Subject.prob.test == tags
result <- data.frame(Doc=a, Predict=b, Prob=c, Correct=d)
head(result,10)


# True Classification result
Subject.true.classifications <- sum(c)/ length(tags)
sprintf("The true classification is %.4f", Subject.true.classifications)

# Confusion Matrix
Subject.confusion.matrix <- table(matrix(tags,ncol=1), matrix(Subject.prob.test,ncol=1))

colnames(Subject.confusion.matrix) <- c("Religion(1)", "Science(0)")
rownames(Subject.confusion.matrix) <- c("Religion(1)", "Science(0)")

Subject.confusion.matrix

## Reg as positive and Sci as negative
TP <- Subject.confusion.matrix[[1,1]]
TN <- Subject.confusion.matrix[[2,2]]
FP <- Subject.confusion.matrix[[1,2]]
FN <- Subject.confusion.matrix[[2,1]]
sprintf("TP = %d, TN = %d, FP = %d, FN = %d",TP, TN, FP, FN)

# Calculating the precision, recall and f-score
precision <- TP /sum(Subject.confusion.matrix[1,]) 
recall <- TP /sum(Subject.confusion.matrix[,1])
f.score <- 2*precision*recall/ (precision+recall)
sprintf("The precision is %.4f", precision)
sprintf("The recall is %.4f", recall)
sprintf("The f-score is %.4f", f.score)


