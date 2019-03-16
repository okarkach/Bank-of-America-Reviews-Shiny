
getwd()
# loading libraries

#Scraping Reviews from Itunes with itunesr library
library(itunesr)

# Reading files
library(readr)

# Cleaning, Data Manipulation, string operation and  dates
library(tidyr)
library(RWeka)
library(dplyr)
library(stringr)
library(DAAG)
library(lubridate)

# Frequency of words, TDM, DTM, TF-IDF etc, text2vec;
library(qdap)
library(tm)
library(RWeka)
library(text2vec)

# To get the sentiments and lexicons
library(tidytext)
library(sentimentr)
library(lexicon)


# For wordclouds
library(wordcloud)
library(wordcloud2)
# General plotting tools
library(plotly)
library(ggplot2)
library(highcharter)
# For emotional Analytics
library(radarchart)

# To create shiny app
library(shiny)
library(shinythemes)
library(DT)
library(shinydashboard)

# Scraping 10 most recent pages of reviews of the Bank of America app from itunes and binding dataframes
reviews_bulk<- dplyr::bind_rows(lapply(1:10, function(n) {getReviews(284847138,'us',n)}))

#Creating a visual for average rating over the past week
reviews_bulk$Date <- as.Date(reviews_bulk$Date)
reviews_bulk$Rating <- as.numeric(reviews_bulk$Rating)    
reviews_bulk_sum <- reviews_bulk %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2))

#Saving datafrane in working directory
write.csv(reviews_bulk, "Bank of America.csv")

#Creating list feeds for funtion options used later in the code
Companies <- c("Bank of America")

Comp_emo <- c("Reviews")

Type <- c("Reviews")

Gram <- c("Uni-gram", "Bi-gram", "Tri-gram", "Quadra-gram")

Lex <- c("nrc", "bing", "afinn")

Read_me_files <- c("BOA")

# Getting the lexicons and arranging them to get sentiment and emotional analytics
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")
afinn <- inner_join(afinn , nrc, by = c("word" = "word"))

#Normalizing the sentiments 
Sentiment <- function(senti){
  if(senti == "fear"| senti == "negative" | senti == "sadness" | senti == "anger"| senti == "disgust"){
    return("negative")
  }
  else{
    return("positive")
  }
}

# Getting the sentiments of the affin lexicon
afinn$sentiment <- sapply(afinn$sentiment, Sentiment)

# For sentiment analysis extractin positive and negative sentiments
nrc_sent <- nrc[(nrc$sentiment %in% c("positive","negative")),]

# For emotional analysis extracting the emotions from the lexicons
nrc_emo <- nrc[!(nrc$sentiment %in% c("positive","negative")),]


# Word cloud matrix
get_term_matrix <- function(comp, type, gram){
  
  if(comp %in% Companies){
    if(type %in% Type){
      if(type == "Reviews"){
        File_name = paste(comp,".csv", sep = "")
        Com <- read.csv(File_name)
        Com$Review<- as.character(Com$Review)
        Com_Review<- Com$Review%>%
          tolower %>%
          stripWhitespace %>%
          removeNumbers %>%
          removePunctuation %>%
          word_tokenizer
        stop_words <- c(stopwords("english"))
        it <- itoken(Com_Review)
      }
      
    if(gram %in% Gram){
      if(gram == "Uni-gram"){
        vocab <- create_vocabulary(it,  stopwords = stop_words, ngram = c(ngram_min = 1L, ngram_max = 1L))
      }
      else if(gram == "Bi-gram"){
        vocab <- create_vocabulary(it,  stopwords = stop_words, ngram = c(ngram_min = 2L, ngram_max = 2L))
      }
      else if(gram == "Tri-gram"){
        vocab <- create_vocabulary(it,  stopwords = stop_words, ngram = c(ngram_min = 3L, ngram_max = 3L))
      }
      else{
        vocab <- create_vocabulary(it,  stopwords = stop_words, ngram = c(ngram_min = 4L, ngram_max = 4L))
      }
    }
    vocab <- vocab[, -c(3)]
    vocab$term_count <- sort(vocab$term_count, dec = TRUE)
    return(vocab)
  }
  }
}



# For sentiment analysis
get_sent_matrix <- function(comp3, type3, lexi){
  
  if(comp3 %in% Companies){
    if(type3 %in% Type){
      if(type3 == "Reviews"){
        File_name = paste(comp3,".csv", sep = "")
        Com <- read.csv(File_name)
        Com$Review<- as.character(Com$Review)
        Com_Review<- Com$Review%>%
          tolower %>%
          stripWhitespace %>%
          removeNumbers %>%
          removePunctuation %>%
          word_tokenizer
        stop_words <- c(stopwords("english"))
        it <- itoken(Com_Review)
      }
      
      }
    }
    vocab <- create_vocabulary(it,  stopwords = stop_words, ngram = c(ngram_min = 1L, ngram_max = 1L))
    vectorizer <- vocab_vectorizer(vocab)
    TDM_Com_Review<- create_dtm(it, vectorizer, type = "dgTMatrix")
    TDM_s <- tidy(TDM_Com_Review)
    if(lexi %in% Lex){
      if(lexi == "nrc"){
        TDM_lex <- inner_join(TDM_s , nrc_sent, by = c("column" = "word"))
        TDM_lex$sentiment_n <- ifelse(TDM_lex$sentiment=="negative", -1, 1)
        TDM_lex$sentiment_score <- TDM_lex$value * TDM_lex$sentiment_n
      }
      else if(lexi == "bing"){
        TDM_lex <- inner_join(TDM_s , bing, by = c("column" = "word"))
        TDM_lex$sentiment_n <- ifelse(TDM_lex$sentiment=="negative", -1, 1)
        TDM_lex$sentiment_score <- TDM_lex$value * TDM_lex$sentiment_n
      }
      else{
        TDM_lex <- inner_join(TDM_s , afinn, by = c("column" = "word"))
        TDM_lex$sentiment_score <- TDM_lex$value * TDM_lex$score
      }
      sentiment_summary <- TDM_lex %>%
        group_by(row) %>%
        summarize(Sentiment = sum(sentiment_score))
      sentiment_summary$row <- as.integer(sentiment_summary$row)
      colnames(sentiment_summary) <- c("document", "Sentiment_score")

    }
    return(sentiment_summary)
  }


# For Emotional analysis
get_emo_matrix <- function(comp4, type4){
  
  if(comp4 %in% Companies){
    if(type4 %in% Type){
      if(type4 == "Reviews"){
        File_name = paste(comp4,".csv", sep = "")
        Com <- read.csv(File_name)
        Com$Review<- as.character(Com$Review)
        Com_Review<- Com$Review%>%
          tolower %>% 
          stripWhitespace %>%
          removePunctuation %>%
          word_tokenizer
        stop_words <- c(stopwords("english"))
        it <- itoken(Com_Review)
        vocab <- create_vocabulary(it,  stopwords = stop_words, ngram = c(ngram_min = 1L, ngram_max = 1L))  
        vectorizer <- vocab_vectorizer(vocab )
        dtm <- create_dtm(it , vectorizer, type = "dgTMatrix")
      }
      
      }
    }
   
    TDM_s <- tidy(dtm)
    TDM_lex <- inner_join(TDM_s , nrc_emo, by = c("column" = "word"))
    TDM_emo <- TDM_lex %>%
      group_by(sentiment) %>%
      summarize(Emotion = sum(value)) %>%
      arrange(desc(Emotion))
    return(TDM_emo)
  }
test <- get_emo_matrix("Bank of America", "Reviews")



# Defining ui function
ui <- dashboardPage(
  # Application Title
  
  dashboardHeader(title = "Bank of America App"),
  dashboardSidebar(
    
    # Side bar Menu items
    sidebarMenu(
      menuItem("Ratings", tabName = "RT", icon = icon("star-half-alt")),
      menuItem("Word Cloud", tabName = "Wc", icon = icon("cloud")),
      menuItem("Sentiment Analysis", tabName = "Sent", icon = icon("chart-line")),
      menuItem("Emotional Analysis", tabName = "Emo", icon = icon("smile-beam")),
      menuItem("Read Me", tabName = "RM", icon = icon("file"))
    )
  ),
  
  # Shiny Body
  dashboardBody(
    tabItems(
      
      # ReadMe tab
      tabItem(
        "RM",
        fluidRow(
          
          # ReadMe file output
          box(
            htmlOutput("Rmd")
          ),
          
          # Inputs for ReadMe file
          box(
            selectInput('rmd', "Select Description:", choices = Read_me_files, selected = "Bank of America App")
          )
        )
      ),
      
      # Ratings tab
      tabItem("RT",
              fluidRow(
                
                #Ratings output
                tabBox( width = 10,
                  tabPanel(
                    title = "Average Ratings (7 day period)", solidHeader = TRUE, status = "primary",
                    highchartOutput("rt"))
                  )
                )
              ),
      # Word cloud tab
      tabItem("Wc",
              fluidRow(
                
                # Word Cloud outputs
                tabBox(
                  tabPanel(
                    title = "Word Cloud" , solidHeader = TRUE, status = "primary",
                    plotOutput("wc"), textOutput("text")
                  ),
                  tabPanel(
                    title = "Word Cloud2", solidHeader = TRUE, status = "primary",
                    wordcloud2Output("wc1"), textOutput("text1")
                  )
                ),
                
                # Inputs for Word cloud
                box( title = "Controls", collapsible = TRUE, status = "success",
                     selectInput('type', "Select Type:", choices = Type),
                     selectInput('company', "Select Company:", choices = Companies),
                     radioButtons("tokenizer", "Select n-gram:", choices = Gram),
                     actionButton("update", "Change", icon = icon("sync-alt"), style = "color: blue; border-color: green")
                ),
                
                # Inputs to control word cloud
                box( title = "Control Options for WordCloud", collapsible = TRUE, status = "success",
                     sliderInput("freq", "Minimum Frequency:", min = 1,  max = 50, value = 4),
                     sliderInput("max", "Maximum Number of Words:", min = 1,  max = 300,  value = 75),
                     paste0("Play area is only for Word Cloud tab not for Word Cloud2 tab")
                )
              )
      ),
      
      # Sentiment analysis tab
      tabItem("Sent",
              fluidRow(
                
                # Sentiment analysis plot
                box( title = "Sentiment Analysis", solidHeader = TRUE, status = "primary",
                     plotlyOutput("sent"), plotlyOutput("sent2")
                ),
                
                # Inputs for sentiment analysis
                box(
                  title = "Controls", collapsible = TRUE, status = "success",
                  selectInput('type3', "Select Type:", choices = Type),
                  selectInput('company3', "Select Company:", choices = Companies),
                  radioButtons("lex", "Select lexicon:", choices = Lex),
                  actionButton("update3", "Change", icon = icon("sync-alt"), style = "color: blue; border-color: green")
                )
                
            
              )
      ),
      
      # Emotional analysis tab
      tabItem("Emo",
              fluidRow(
                
                # Radar chart output 
                box( title = "Emotional Analysis", solidHeader = TRUE, status = "primary",
                     chartJSRadarOutput("emo", width = "150", height = "150")
                ),
                
                # Inputs for emotional analysis
                box(
                  title = "Controls", collapsible = TRUE, status = "success",
                  selectInput("type4", "Select Type:", choices = Type),
                  selectInput("companies", "Select Company:", choices = Companies),
                  actionButton('update4', "Change", icon = icon("sync-alt"), style = "color: blue; border-color: green")
                ),
                
                # Data table output of emotions and their scores
                box(
                  title = "Emotional Analysis Data Table", solidHeader = TRUE, status = "warning", collapsible = TRUE,
                  dataTableOutput("data_emo")
                )
              )
      )
      

    )
  )
)

# Defining the server function
server <- function(input, output, session) {
  
  #Ratings plot output
  output$rt <- renderHighchart({
    highchart() %>%   hc_add_series_times_values(reviews_bulk_sum$Date,reviews_bulk_sum$Rating, name = 'Average Rating')
  })
  # To create word cloud
  filtered_data <- reactive({
    
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        get_term_matrix(input$company, input$type, input$tokenizer)
      })
    })
  })
  
  # Word cloud plot
  output$wc <- renderPlot({
    wordcloud(filtered_data()$term, filtered_data()$term_count, min.freq = input$freq, max.words = input$max, colors=brewer.pal(8, "Paired"))
  })
  
  # Word cloud2 plot
  output$wc1 <- renderWordcloud2({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Generating Word Cloud...")
        wordcloud2(filtered_data(), shape = "cardiod", size = 1.5)
        
      })
    })
  })
  
  # Review representation of desired selection from wordcloud tab control and play area inputs under wordcloud sidebar menu item
  output$Review<- renderText({
    input$update
    isolate(({
      withProgress({
        paste0("A WordCloud of ", input$company, " company's ", input$type)
      })
    }))
  })
  
  # Review representation of desired selection from wordcloud2 tab control and play area inputs under wordcloud sidebar menu item
  output$text1 <- renderText({
    input$update
    isolate(({
      withProgress({
        paste0("A Wordcloud of ", input$company, " company's ", input$type)
      })
    }))
  })
  

  
 

  
  
  
  # To generate sentiment analysis
  filtered_data3 <- reactive({
    input$update3
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        as.data.frame(get_sent_matrix(input$company3, input$type3, input$lex))
      })
    })
  })
  
  
  
  # Sentiment analysis smoothed plot
  output$sent <- renderPlotly({
    S_P <- ggplot(filtered_data3(), aes(document, Sentiment_score)) + geom_smooth() + theme_bw()+
      geom_hline(yintercept = 0, color = "red")+xlab("sentence")+ylab("sentiment")
    ggplotly(S_P)
  })
  
  # Sentiment analysis bar plot
  output$sent2 <- renderPlotly({
    N_P <- ggplot(filtered_data3(),aes(document, Sentiment_score)) + geom_bar(stat = "identity") + theme_bw()
    ggplotly(N_P)
  })

 
  

  # To generate emotional analysis
  filtered_data4 <- reactive({
    input$update4
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        if(input$companies == "Both"){
          get_emo_matrix2(input$type4)
        }
        else{
          as.data.frame(get_emo_matrix(input$companies, input$type4))
        }
      })
    })
  })
  
  # Radar chart plot
  output$emo <- renderChartJSRadar({
    input$update4
    isolate({
      withProgress({
        setProgress(message = "Generating Emotional Analysis.....")
        
          chartJSRadar(filtered_data4(), showLegend = TRUE,
                       main = "Bank of America Emotional Analysis", labelSize = 15)
        
      })
    })
  })
  
  # Data table output of emotions
  output$data_emo <- renderDataTable({
    input$update4
    isolate({
      withProgress({
        setProgress(message = "Generating Emotional Analysis.....")
        filtered_data4()
      })
    })
  })
  
  # Read me files output
  output$Rmd <- renderUI({
    includeMarkdown(knitr::knit(paste0(input$rmd, ".Rmd")))
  })
  
}

# Run app
shinyApp(ui = ui, server = server)
