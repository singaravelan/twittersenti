library(twitteR)
library(RCurl)
library(ROAuth)
library(shiny)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(syuzhet)
library(plotly)
#Setwd needs to be updated
setwd('C:/Users/si305132/source/repos/RProject2/RProject2/')
#kindly run search_txt before proceeding to the twitter key updation
search_txt <- NULL

consumer_key <- "cBI4YCPrSyglMXYPkagA3H9RP"
consumer_secret <- "aTyJZVdl0y99iIn4XTxVX7miAMfU3uQM5L3R2l7qZ2ElMeViBm"
access_token <- "251417137-KPz5zoGvhLNN8azgWCuhDjzg5YhUCPUs2R95leOU"
access_secret <- "U01uJbYAgyu7b7oxg7sEZwqzJbxAtbt7h6tP4ds13XaCZ"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


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
    df$text <- clean_tweet
    if (file.exists(paste(search_txt, '_stack.csv')) == FALSE) write.csv(df, file = paste(search_txt, '_stack.csv'), row.names = F)

    pos <- scan('C:/Users/si305132/source/repos/RProject2/RProject2/positive-words.txt', what = 'character', comment.char = ';') #folder with positive dictionary
    neg <- scan('C:/Users/si305132/source/repos/RProject2/RProject2/negative-words.txt', what = 'character', comment.char = ';') #folder with negative dictionary
    pos.words <- c(pos)
    neg.words <- c(neg)

    stack <- df
    stack <- subset(stack, !duplicated(stack$text))
    Dataset <- stack
    tweets <- Dataset$text
    analysis <- score.sentiment(tweets, pos.words, neg.words,search_txt)
    

}

score.sentiment = function(tweets, pos.words, neg.words,src) {

    require(plyr)
    require(stringr)

    scores = laply(tweets, function(tweet, pos.words, neg.words) {



        tweet = gsub('https://', '', tweet) # removes https://
        tweet = gsub('http://', '', tweet) # removes http://
        tweet = gsub('[^[:graph:]]', ' ', tweet) ## removes graphic characters 
        #like emoticons 
        tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
        tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
        tweet = gsub('\\d+', '', tweet) # removes numbers
        tweet = str_replace_all(tweet, "[^[:graph:]]", " ")

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

    scores.df = data.frame(score = scores, text = tweets)
    scores.df$emo =scores.df$score
    scores.df$emo[scores.df$score == 0] = "Neutral"
    scores.df$emo[scores.df$score < 0] = "Negative"
    scores.df$emo[scores.df$score > 0] = "Positive"
    write.csv(scores.df, file = paste(src,'_stack_results.csv'), row.names = F)
    return(scores.df)
}

plot_a <- function(search) {
    data <- read.csv(file = paste(search, '_stack_results.csv'), header = TRUE)
    neg <- length(data$emo[data$emo == "Negative"])
    neu <- length(data$emo[data$emo == "Neutral"])
    pos <- length(data$emo[data$emo == "Positive"])
    slices <- c(neg, neu, pos)
    lbls <- c("Negative", "Neutral", "Positive")
    pie(slices, labels = lbls, main = "Pie Chart of Sentiment Break up")
}

if (interactive()) {

    shinyApp(

 ui = pageWithSidebar
 (
            headerPanel("Twitter Sentiment Analysis"),
            sidebarPanel("Side Bar",
            textInput("name", "Name:"),
            submitButton("Click to Analyze", icon("refresh"))),
            mainPanel("Main Panel",plotOutput("plot", width = "100%", height = "400px") ,tableOutput("value"))
            ),
  server = function(input, output) {

      output$value <- renderTable({ pull_twits(input$name) })
      output$plot <- renderPlot({plot_a(input$name)})

  }
    )
}