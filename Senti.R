library(twitteR)
library(RCurl)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(syuzhet)
library(plotly)
library(base64enc)
library(RODBC)
#Setwd needs to be updated
setwd('D:/Data Science Transformation/Sentiment Analysis')
#kindly run search_txt before proceeding to the twitter key updation
search_txt <- 'Automatic Cars'

consumer_key <- "cBI4YCPrSyglMXYPkagA3H9RP"
consumer_secret <- "aTyJZVdl0y99iIn4XTxVX7miAMfU3uQM5L3R2l7qZ2ElMeViBm"
access_token <- "251417137-KPz5zoGvhLNN8azgWCuhDjzg5YhUCPUs2R95leOU"
access_secret <- "U01uJbYAgyu7b7oxg7sEZwqzJbxAtbt7h6tP4ds13XaCZ"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

pull_twits("")

###
pull_twits <- function(search_txt) {
  require(stringr)
  search_txt <<- search_txt
  list <- searchTwitter(search_txt, n = 100)
  df <- twListToDF(list)
  df <- df[, order(names(df))]
  df$created <- strftime(df$created, '%Y-%m-%d')
  df$text <- as.character(df$text)
  clean_tweet = gsub("&amp", "", df$text)
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
  clean_tweet = gsub("@\\w+", "", clean_tweet)
  clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
  clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
  clean_tweet = gsub("http\\w+", "", clean_tweet)
  clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
  clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)
  clean_tweet = gsub("\n", "", clean_tweet)
  clean_tweet=str_replace_all(clean_tweet,"[^[:graph:]]", " ")
  df$text <- clean_tweet
  if (file.exists('stack.csv') == FALSE) write.csv(df, file= 'stack.csv', row.names = F)
  
  df1 <- read.csv(file = 'stack.csv')
  df2 <-df
  df <- rbind(df1,df2)
  write.csv(df, file= 'stack.csv', row.names = F)
  
  pos <- scan('D:/Data Science Transformation/Sentiment Analysis/positive-words.txt', what = 'character', comment.char = ';') #folder with positive dictionary
  neg <- scan('D:/Data Science Transformation/Sentiment Analysis/negative-words.txt', what = 'character', comment.char = ';') #folder with negative dictionary
  pos.words <- c(pos)
  neg.words <- c(neg)
  
  stack <- df
  stack <- subset(stack, !duplicated(stack$text))
  Dataset <- read.csv(file = 'stack.csv')
  tweets <- c(as.character(Dataset$text))

  require(plyr)
  require(stringr)
  
  scores = laply(Dataset$text, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://', '', tweet) # removes https://
    tweet = gsub('http://', '', tweet) # removes http://
    tweet = gsub('[^[:graph:]]', ' ', tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet = str_replace_all(tweet, "[^[:graph:]]", " ")
    tweet = gsub("\n", "", tweet)
    
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words)
  
  scores.df = data.frame(score = scores, text = Dataset$text,id = Dataset$id, date = Dataset$created, query = Dataset$truncated, user = "NA")
  scores.df$sentiment = scores.df$score
  scores.df$sentiment[scores.df$score == 0] = "Neutral"
  scores.df$sentiment[scores.df$score < 0] = "Negative"
  scores.df$sentiment[scores.df$score > 0] = "Positive"
  
  write.csv(scores.df, 'stack_results.csv', row.names = F, append = TRUE, quote = F )
 
  
  
}

library(twitteR)
library(ROAuth)
library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)

setwd('D:/Data Science Transformation/Sentiment Analysis')
tweets_classified <- read_csv('stack_results.csv')
tweets_classified$text = str_replace_all(tweets_classified$text, "[^[:graph:]]", " ")
tweets_classified$text<- iconv(tweets_classified$text, "ASCII", "UTF-8", sub="")
# data splitting on train and test
set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

##### Vectorization #####
# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = tweets_train$id,
                   progressbar = TRUE)
it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = tweets_test$id,
                  progressbar = TRUE)

# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
# define tf-idf model
tfidf <- TfIdf$new()
# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
# apply pre-trained tf-idf transformation to test data
dtm_test_tfidf  <- create_dtm(it_test, vectorizer) %>% 
  transform(tfidf)

# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf,
                               y = tweets_train[['sentiment']], 
                               family = 'multinomial', 
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 5-fold cross-validation
                               nfolds = 5,
                               # high value is less accurate, but has faster training
                               thresh = 1e-3,
                               # again lower number of iterations for faster training
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
auc(as.numeric(tweets_test$sentiment), preds)

write.csv(vocab, 'stack_WordCloud.csv', row.names = F, append = TRUE, quote = F )


user <- getUser(fg$screenName[2])

print(user$location)

for (i in 1:nrow(fg)) {
  
  print(fg$screenName[i])
  
}