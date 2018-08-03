packages <- c("shiny", "twitteR", "tm", "RColorBrewer", "wordcloud", "rjson", "stringr", "plyr", "sqldf")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#Load the neccessary libraries
library(shiny)
library(twitteR)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(rjson)
library(stringr)
library(plyr)
library(sqldf)


#Connecting R to Twitter
#Update consumerKey, consumerSecret, accessToken, accessTokenSecret using your app details. 
consumerKey <- "xxxxxxxxx" 
consumerSecret <- "xxxxxxxxx"
accessToken <- "xxxxxxxxx"
accessTokenSecret <- "xxxxxxxxx"
options(httr_oauth_cache=T)
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

#The Twitter function // getting Twitter messages and cleaning them.
analysing_tweets  = function(search_tweets){
  #Get the Twitter messages
  text <- search_tweets$text
  # Load the data as a corpus
  docs <- Corpus(VectorSource(text))
  #Text transformation_Replacing 
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  #Cleaning Data
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  # Remove your own stop word
  # specify your stopwords as a character vector
  #docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
  
  
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  #Build a term-document matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}

#search all trends in Nairobi
#Use this function to get WOEID for Nairobi > closestTrendLocations(-1.2921,36.8219)
trends = getTrends(1528488)
all_trends = trends$name

shinyServer(function(input, output) {
  
  output$more_trends <- renderUI({
    selectInput("select", "All Trending Topics", 
                choices = all_trends, selected = all_trends[1])
  })
  
  #Create a function to search Twitter
  search_tweets <- reactive({
    #Get Trend/Keyword and Search Twitter using searchTwitter function   
    my_search = input$select
    no_tweets = input$no_tweets
    trend_search <- searchTwitter(my_search, n = no_tweets, lang="en")
    trend_search = twListToDF(trend_search)
    return(trend_search)
  })

  #Create a function to analyse data obtained
  cleaned_data <- reactive({
    
    tweets_data = search_tweets()
    analysing_tweets(tweets_data) 
    
  })
  
  output$cloud_plot <- renderPlot({
    #Get cleaned data
    d = cleaned_data()
    
    #Generate the Word cloud
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$tweets_table <- renderTable({
    tweets_table = search_tweets()
    Tweet_Created = as.character.Date(tweets_table$created)
    tweets_table = cbind(tweets_table,Tweet_Created)
    tweets_table = tweets_table[,c("text","screenName","Tweet_Created")]
    tweets_table = sqldf("SELECT text,screenName,Tweet_Created FROM tweets_table")
  })
})
