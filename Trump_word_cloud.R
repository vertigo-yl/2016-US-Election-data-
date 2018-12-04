install.packages("SnowballC")
install.packages('wordcloud')
install.packages('tm')
library(SnowballC)
library(tm)
library(wordcloud)

install.packages("RColorBrewer")
install.packages("tm")
install.packages("wordcloud")
install.packages('base64enc')
install.packages('ROAuth')
install.packages('plyr')
install.packages('stringr')
install.packages('twitteR')
library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
# Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"
# Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"
# Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"

setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessTokenSecret) 
consumerKey <- '756ADo9VtbgOtGVPyAWtDufQr'
consumerSecret <- 'DRlGpXpFvGSTRsP5nKg7qusW18HCH5eSCpvNXwOgfGBakoYZn1'
accessToken <- '2428680278-IDYLsO9LFiWZcuCOw83cNElAw17TQS8obs7VhVy'
accessTokenSecret <- 'cuiBib7zVXvXf4rXgsgs5AdbVgZ5H9oCswGChhD11Rn7A'

trump <- searchTwitter('Trump' ,n=10000,lang = 'en')

trump_text <- sapply(trump, function(x) x$getText())

trump_text_corpus <- Corpus(VectorSource(trump_text))

trump_text_corpus <- tm_map(trump_text_corpus, removePunctuation)

trump_text_corpus <- tm_map(trump_text_corpus, content_transformer(tolower))

trump_text_corpus <- tm_map(trump_text_corpus, function(x)removeWords(x,stopwords()))

trump_text_corpus <- tm_map(trump_text_corpus, removeWords, c("RT","are","that","might","think","said","going","word","...","just",""))

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
trump_text_corpus <- tm_map(trump_text_corpus, content_transformer(removeURL))

trump_2 <- TermDocumentMatrix(trump_text_corpus)
trump_2 <- as.matrix(trump_2)
trump_2 <- sort(rowSums(trump_2),decreasing=TRUE)

trump_2 <- data.frame(word = names(trump_2),freq=trump_2)

set.seed(1231)
wordcloud(trump_text_corpus,min.freq=1,max.words=100,scale=c(2.2,1), colors=brewer.pal(8, "Dark2"), random.color=T, random.order=F)
