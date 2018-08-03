library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sentiment Analysis on Twitter"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #An explanation on how to use the app.
      helpText("Trend and Number of Tweets to search"),
      #The number of Tweets to search
      numericInput("no_tweets", "Number of Tweets:", 100),
      #The Keyword and Twitter handle to search
      uiOutput("more_trends")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Wordcloud for Trend", plotOutput("cloud_plot")),
        tabPanel("Tweets Sample", tableOutput("tweets_table"))
      )
    )
  )
))
