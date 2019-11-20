
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(tm)
library(SnowballC)
library(SentimentAnalysis)

setwd("~/Desktop/Data-Science-Portfolio/Projects/DisneyPlus")

getSentiment <- function(df) {
  # Convert Tweet texts to corpus datatype
  corpus = SimpleCorpus(VectorSource(df$Text))
  # Continue to clean corpus
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  # Stemming
  corpus = tm_map(corpus, stemDocument)
  
  # Document-Term Matrix
  dtm = DocumentTermMatrix(corpus)
  
  
  sentiment = analyzeSentiment(dtm, language = "english") 
  sentiment
}


# Incrementally read in json file, since it is too large
# to read in all at once in my IDE.
tweets <- stream_in(file("tweets.json"), pagesize = 10000)
tweets$Text <- tolower(tweets$Text)
tweets$DisneyPlus = ifelse(grepl("#disneyplus(?!f)", tweets$Text, perl=TRUE), 1, 0)
tweets$DisneyPlusFail = ifelse(grepl("#disneyplusfail", tweets$Text), 1, 0)
tweets$Both = ifelse(tweets$DisneyPlus == 0, 0, ifelse(tweets$DisneyPlusFail == 1, 1, 0))
n <- length(rownames(tweets))
partial.1 <- tweets[1:25000,]
partial.2 <- tweets[25001:50000,]
partial.3 <- tweets[50001:75000,]
partial.4 <- tweets[75001:100000,]
partial.5 <- tweets[100001:n,]
split.dfs <- list(partial.1, partial.2, partial.3, partial.4, partial.5)

for (i in 1:5) {
  if (i == 1) {
    tmp <- getSentiment(split.dfs[[i]])
  } else {
    tmp <- rbind(tmp, getSentiment(split.dfs[[i]]))
  }
}
qdap <- subset(tmp, select=c(SentimentQDAP, NegativityQDAP, PositivityQDAP))
a <- cbind(tweets, qdap)
fail <- subset(a, (DisneyPlusFail == 1 & DisneyPlus == 0))
plus <- subset(a, (DisneyPlus == 1 & DisneyPlusFail == 0))
a$Descriptor <- ifelse((a$DisneyPlus == 1 & a$DisneyPlusFail == 0), "#DisneyPlus",
                       ifelse((a$DisneyPlus == 0 & a$DisneyPlusFail == 1), "#DisneyPlusFail",
                              "Both"))

ggplot(fail, aes(y=SentimentQDAP)) +
  geom_boxplot()

ggplot(plus, aes(y=SentimentQDAP)) +
  geom_boxplot()

ggplot(a, aes(x=Descriptor, y=SentimentQDAP, color=Descriptor)) +
  geom_boxplot()

removeURL <- function(x) gsub("http[\\S]+[[:space:]]", "", x)
removeHashtags <- function(x) gsub("#\\S+", "", x)
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)

clean <- function(tweet.subset) {
  # Subset so that we are looking at original Tweets only, rather than including Retweets
  # Also turns our there are some accounts spamming advertisements for other products,
  # using the #DisneyPlus hashtag for publicity. Remove those Tweets + all Retweets,
  # since having the exact same Tweets repeatedly will bias our word counts.
  tweet.subset <- subset(tweet.subset, tweet.subset$Retweet == "Original Tweet")
  tweet.subset <- tweet.subset[!grepl("fortnite|minecraft", tweet.subset$Text),]
  tweet.subset <- unique(tweet.subset[,'Text'])
  # Merge the negative Tweets into a single character vector
  negative.text <- concatenate(tweet.subset)
  #text <- concatenate(tweets[,"Text"])
  # Perform some cleaning of the text, similar to what was
  # done during the sentiment analysis.
  corpus = SimpleCorpus(VectorSource(negative.text))
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, content_transformer(removeURL))
  corpus = tm_map(corpus, content_transformer(removeHashtags))
  corpus = tm_map(corpus, content_transformer(removeTwitterHandles))
  corpus = tm_map(corpus, removeWords, stopwords("english")) 
  corpus = tm_map(corpus, removePunctuation)
  #corpus = tm_map(corpus, stemDocument)
  corpus
}


library(wordcloud)
library(RColorBrewer)
sub <- sample_n(tweets, 25000)
corpus <-  clean(sub)
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word=names(v), freq=v)
d <- subset(d, nchar(d$word) > 3)

w <- wordcloud(words = d$word, freq= d$freq, min.freq = 5,
          max.words = 200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Paired"))
ggsave("~/Desktop/Data-Science-Portfolio/Projects/DisneyPlus/wordcloud.jpg", w)
