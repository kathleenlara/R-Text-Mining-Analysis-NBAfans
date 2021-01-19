#' Title: Fan Engagement NBA Tweets
#' Purpose: A2- Case 1
#' Author: Kathleen Lara
#' email: kathleencastrolara@gmail.com
#' Date: January , 2021

# Set the working directory
setwd("~/Desktop/hult_NLP_student/cases/NBA Fan Engagement/data")

#Libraries
library(ggplot2)
library(ggthemes)
library(tm)
library(qdap)
library(wordcloud)
library(wordcloud)
library(RColorBrewer)

#Options and Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')


tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}


cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

df_stopwords <- c(stopwords("SMART"),
                  "nba","boston","celtics", "boston celtics", "coach","butler", "game","will","lol","tyler",
                  "score","nike","us","jimmy","league","team","lebron","heat", "bam", "adebayo",
                  "duncan", "playoffs", "score", "stevens", "los", "angeles", "games", "finals", "season",
                  "championship", "won", "win", "tonight", "rt", "Tawhidi","evidence", "scandal", "james", "miamiheat", 
                  "nbatwitter", "twitter", "announcement")


# Reading the data 
TweetsDF   <- read.csv('B_Nov2019.csv')


#Exploring the number of tweets that talks about teams 
tweets_count <- count(TweetsDF$team)
tweets_count[order(tweets_count$freq), ]

#Subset the tweets of the fans that I only want to see
TweetsDF_celtics <- subset(TweetsDF, team == "Boston Celtics")
write.csv(TweetsDF_celtics,"celtics_nov2019.csv", row.names = FALSE)
TweetsDF   <- read.csv('celtics_nov2019.csv', nrows=1000)


#Clean and Organize
txtCorpus <- VCorpus(VectorSource(TweetsDF_celtics$text))
txtCorpus <- cleanCorpus(txtCorpus, df_stopwords)
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)


# Inspect word associations
associations <- findAssocs(tweetTDM, 'sale', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF


# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3) 


# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(TweetsDF))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, df_stopwords)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
tweetsTDM  <- TermDocumentMatrix(txtCorpus, 
                                 control=list(tokenize=bigramTokens))
tweetsTDMm <- as.matrix(tweetsTDM)


# Get Row Sums & organize
tweetsTDMv <- sort(rowSums(tweetsTDMm), decreasing = TRUE)
tweets_DF   <- data.frame(word = names(tweetsTDMv), freq = tweetsTDMv)

# Review all Palettes
display.brewer.all()

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Greens")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(tweets_DF$word,
          tweets_DF$freq,
          max.words    = 200,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

# End



