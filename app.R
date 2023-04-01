library(shiny)
library(tidyverse)
library(sentimentr)
library(quanteda)
library(quanteda.textstats)


# UI
ui <- fluidPage(
  # title
  titlePanel(h1("Generate Sentiment & Readability Scores")),
  # spiel
  fluidRow(
    column(
      wellPanel(textAreaInput("input_text", "Enter Text:", "There's more to the new iPhone 14 than meets the eye. The iPhone 14 features an improved camera system that delivers better low-light performance and a faster and brighter TrueDepth camera with autofocus. On the video front, there’s a new Action mode for super steady footage.",
                          width = '100%', height = '56px'),
                br(),
                fluidRow(column(4,checkboxGroupInput("readability_metric", "Readability Metrics:",
                                   choices = c("Flesch", 
                                               "FOG",
                                               "SMOG",
                                               "ARI"),
                                   selected = c("Flesch","FOG","SMOG","ARI"))),
                         column(8, 
                                h2("README"),
                                tags$li("This web app was designed as a tool for anyone to quickly evaluate any text for",
                                        tags$b("Sentiment & Readability")),
                                tags$li("Learn more about ", 
                                        tags$a(href = "https://readable.com/readability/", "Readability"),
                                        "and ",
                                        tags$a(href = "https://en.wikipedia.org/wiki/Sentiment_analysis", "Sentiment Analysis")),
                                tags$li("Readability scores were generated using the ",
                                        tags$a(href = "https://cran.r-project.org/web/packages/quanteda.textstats/index.html", "quanteda.textstats"),
                                        "package in R"),
                                tags$li("Sentiment analysis was conducted with the ",
                                        tags$a(href = "https://cran.r-project.org/web/packages/sentimentr/sentimentr.pdf", "sentimentr"),
                                        "package in R using the default settings")
                                )),
                br()), 
      width = 10),
    column(
      wellPanel("Access the README and codes on ", tags$a(href = "https://github.com/Aaron0696/text_tool", "Github."),
                br(),
                "Connect with me on ", tags$a(href = "https://www.linkedin.com/in/aaron-lim-b30898135/", "LinkedIn.")),
      width = 2)),
  fluidRow(column(3, wellPanel(h2("Readability Scores"),
                               tableOutput("readability_table"),
                               tags$b(h4("Recommended Readability Scores")),
                               tags$li("Flesch (More than 70)"),
                               tags$li("FOG (Less than 8)"),
                               tags$li("SMOG (Less than 9)"),
                               tags$li("ARI (Less than 10)"))),
           column(7, wellPanel(h2("Sentiment Scores"),
                               tableOutput("sentiment_table"))))
)


# Server
server <- function(input, output) {
  
  # Sentiment analysis
  output$sentiment_table <- renderTable({
    input_text <- tibble(text = input$input_text)
    sentences <- str_squish(str_split_1(input_text$text, "\\.|\\?|\\!"))
    sentences <- sentences[nchar(sentences) > 0]
    sentiment_scores <- sentiment(sentences)
    sentiment_scores$Sentence <- sentences
    sentiment_scores$Characters <- nchar(sentiment_scores$Sentence)
    sentiment_scores <- sentiment_scores %>% 
      select(-element_id,-sentence_id) %>% 
      rename(Sentiment = 'sentiment',
             WordCount = 'word_count') %>% 
      select(Characters, WordCount, Sentiment, Sentence) %>% 
      filter(!is.na(WordCount))
    sentiment_scores
    })
  
  # Readability analysis
  output$readability_table <- renderTable({
    input_text <- corpus(input$input_text)
    readability_scores <- textstat_readability(input_text, measure = input$readability_metric)
    readability <- readability_scores %>% 
      pivot_longer(cols = -document) %>% 
      select(-document) %>% 
      rename(Metric = 'name', Value = 'value') %>% 
      mutate(Metric = paste0("Readability_",Metric))
    
    words <- str_count(input$input_text, "\\S+")
    chars <- nchar(input$input_text)
    overall_sentiment <- mean(sentiment(input$input_text)$sentiment, 
                              na.rm = TRUE)
    
    readability <- rbind(data.frame(Metric = "WordCount",Value = words),
                         data.frame(Metric = "Characters",Value = chars),
                         data.frame(Metric = "Overall Sentiment",Value = overall_sentiment),
                         readability)
    readability
  })
  
}

# Run app
shinyApp(ui, server)
