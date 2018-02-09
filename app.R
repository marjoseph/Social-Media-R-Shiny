
#Created on Sunday December 21 7:21 PM 2017
#Author: marjoseph


################################
######--Read in Libraries--#####
################################
library(shinydashboard)
library(shinythemes)
library(data.table)
library(rtweet)
library(RedditExtractoR)
library(SocialMediaMineR)
library(stringr)
library(readr)
library(shinycssloaders)
library(rsconnect)
library(tm)
library(wordcloud)
library(tidytext)
library(shinyjs)
library(V8)
library(tidytext)
library(googlesheets)
library(reshape2)
library(ggplot2)
library(DT)
library(dplyr)
library(sentimentr)
library(xml2)
library(rvest)
library(lexRankr)
library(shinyswipr)
library(fortunes)
library(shiny.semantic)

################################
########--Prep for Shiny--######
################################
#Note that many of these API's require an OAUTH code and thus may not work initially as intended

#Load city/country data for Twitter location trends section
country_city_list = read_csv("country and city list.csv")

#Fix Function for Reddit
get_reddit = function (search_terms = NA,
                       regex_filter = "",
                       subreddit = NA,
                       cn_threshold = 1,
                       page_threshold = 1,
                       sort_by = "comments",
                       wait_time = 2)
{
  URL = unique(as.character(
    reddit_urls(
      search_terms,
      regex_filter,
      subreddit,
      cn_threshold,
      page_threshold,
      sort_by,
      wait_time
    )$URL
   )
  )
  retrieved_data = reddit_content(URL, wait_time)
  return(retrieved_data)
}

# Create "top of page" buttons
jscode = "shinyjs.toTop = function() {document.body.scrollTop = 0;}"
jscode2 = "shinyjs.toTop2 = function() {document.body.scrollTop = 0;}"
jscode3 = "shinyjs.toTop3 = function() {document.body.scrollTop = 0;}"
jscode4 = "shinyjs.toTop4 = function() {document.body.scrollTop = 0;}"
jscode5 = "shinyjs.toTop5 = function() {document.body.scrollTop = 0;}"
jscode6 = "shinyjs.toTop6 = function() {document.body.scrollTop = 0;}"

#Set up logger
#flog.threshold(TRACE)
#log.appender(appender.file("TestLog.logger"))
#flog.layout(layout.format('[~l] [~t] [~n.~f] ~m'))

################################
#########--Begin App--##########
################################
shinyApp(
  ui = tagList(
    # Center the Text
    tags$head(tags$style(
      HTML(
        "#final_text {
        text-align: center;
        }
        div.box-header {
        text-align: center;
        }"
      )
    )
  ),
    #This will hide any error messages
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    navbarPage(
      #This is the name of the UI's theme (ShinyThemes)
      theme = shinytheme("paper"),
      #This is the top left title in the header
      "Social Analysis",
      
      ######################################
      #######----Begin HOME Page-----#######
      ######################################
      # This panel is the landing page
      tabPanel(
        "Home",
        icon = icon("home"),
        semanticPage(
          title = "test",
          fluidRow(hr()),
          fluidRow(hr()),
          fluidRow(
            box(
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE,
              width = 12,
              title = "Welcome: Gather Media data & reveal insights"
            )
          ),
          hr(),
          wellPanel(tags$h5("What Does this tool do?")),
          tags$h6(
            "This tool gathers social media data and helps show insights into the data via tables
            and visualizations. Click one of the social media tabs at the top of the page to get
            started."
          )
        )
      ),
      
      ################################
      #####--Begin Twitter Page--#####
      ################################
      tabPanel(
        "Twitter",
        icon = icon("twitter"),
        sidebarPanel(
          #This is the sidebar
          sidebarMenu(
            hr(),
            icon("twitter"),
            "Twitter Username Output",
            verbatimTextOutput("twitterText", placeholder = 1),
            hr(),
            icon("twitter"),
            "Twitter Term Output",
            verbatimTextOutput("twitterPhraseText", placeholder = 1),
            hr(),
            icon("twitter"),
            "Twitter City Output",
            verbatimTextOutput("city", placeholder = 1),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr()
          )
        ),
        mainPanel(
          #This is the main page section where the output is shown
          tabsetPanel(
            
            tabPanel(
              "Username Search",
              icon = icon("user-circle"),
              #This is where the main TWITTER table output is shown for USERNAMES
              hr(),
              h4("Search Twitter for Usernames"),
              wellPanel(
                sidebarSearchForm(textId = "twitterText", 
                                  buttonId = "searchButton",
                                  "Twitter Username ...")
              ),
              hr(),
              # Create a "number of tweets" option
              wellPanel(numericInput(
                "num",
                label = h5("How many tweets to return?"),
                value = 100
              )
            ),
              # Create a "Retweets" option
              wellPanel(radioButtons(
                "radio",
                label = h5("Include Retweets?"),
                choices = list("Yes" = 1, "No" = 2),
                selected = 1
              )
            ),
              withSpinner(dataTableOutput("timeline_1"), 
                          type = 7),
              hr(),
             # Create the "jump to top of page" button
              useShinyjs(),
              extendShinyjs(text = jscode),
              actionButton("toTop", "jump to top"),
              hr()
            ),
            
            tabPanel(
              #This is where the main TWITTER Visualization outputs are shown for USERNAMES
              "Username - Visualizations",
              icon = icon("area-chart"),
              mainPanel(
                hr(),
                hr(),
                # Here is the wordcloud for the Twitter Username text
                h4("Twitter Username Wordcloud"),
                wellPanel(
                  # This is for the "Number of words" slider
                  sliderInput(
                    "words",
                    "Number of Words:",
                    min = 1,
                    max = 50,
                    value = 15
                  )
                ),
                wellPanel(withSpinner(
                  plotOutput("wordcloud10"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Top 10 sentiment visualization with "bing" sentiment terms
                h4("Most used Positive & Negative terms"),
                wellPanel(withSpinner(
                  plotOutput("bing_top10"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Sentiment wordcloud for Twitter Username
                h4("Twitter Sentiment Wordcloud for Username"),
                wellPanel(withSpinner(
                  plotOutput("sentiment_wordcloud"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Twitter Username posting frequency visualization
                h4("Twitter Username Post Frequency"),
                wellPanel(withSpinner(
                  plotOutput("tweetFreq"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Twitter global map for Username Coordinates
                h4("Twitter Username Global Coordinates"),
                h6("If available..."),
                wellPanel(withSpinner(
                  plotOutput("map"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Twitter USA map for Username Coordinates
                h4("Twitter Username USA Coordinates"),
                h6("If available..."),
                wellPanel(withSpinner(
                  plotOutput("map2"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Simple barplot of the sentiment from "NRC" sentiment terms
                h4("Barplot of Tweet Sentiment for Username"),
                wellPanel(withSpinner(
                  plotOutput("nrc_barplot"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # A table of the Twitter Username's followers
                h4("Twitter Followers Table"),
                withSpinner(dataTableOutput("followers"), type = 7),
                hr(),
                dataTableOutput("select"),
                hr(),
                # Create the "jump to top of page" button
                useShinyjs(),
                extendShinyjs(text = jscode2),
                actionButton("toTop2", "jump to top"),
                hr()
              )
            ),
            
            tabPanel(
              # This is where you can do a PHRASE search for Twitter
              "Word Search",
              icon = icon("users"),
              hr(),
              h4("Search Twitter for Phrase"),
              wellPanel(
                sidebarSearchForm(
                  textId = "twitterPhraseText",
                  buttonId = "searchButton",
                  "Twitter Phrase Search ..."
                )
              ),
              hr(),
              # Here is the output for the Twitter PHRASE search
              withSpinner(dataTableOutput("phrase_1"), type = 7),
              hr(),
              # Create the "jump to top of page" button
              useShinyjs(),
              extendShinyjs(text = jscode3),
              actionButton("toTop3", "jump to top"),
              hr()
            ),
            
            tabPanel(
              # Here is the visualization page for Twitter Phrase search
              "Word/Phrase - Visualizations",
              icon = icon("area-chart"),
              mainPanel(
                hr(),
                # Here is the Twitter PHRASE basic wordcloud slider for the "Number of words"
                h4("Twitter Phrase Wordcloud"),
                wellPanel(
                  sliderInput(
                    "words_1",
                    "Number of Words:",
                    min = 1,
                    max = 50,
                    value = 15
                  )
                ),
                # Here is the Twitter PHRASE basic wordcloud
                wellPanel(withSpinner(
                  plotOutput("wordcloud"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Top 10 sentiment visualization with "bing" sentiment terms
                h4("Most used Positive & Negative terms"),
                wellPanel(withSpinner(
                  plotOutput("bing_top101"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Twitter PHRASE sentiment wordcloud
                h4("Twitter Sentiment Wordcloud for Username"),
                wellPanel(withSpinner(
                  plotOutput("sentiment_wordcloud_10"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Twitter PHRASE posting frequency visualization
                h4("Twitter Username Post Frequency"),
                wellPanel(withSpinner(
                  plotOutput("tweetFreq_1"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Twitter PHRASE Global map visualization
                h4("Twitter Phrase Global Coordinates"),
                h6("If available..."),
                wellPanel(withSpinner(
                  plotOutput("map_1"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Twitter PHRASE USA map visualization
                h4("Twitter Phrase USA Coordinates"),
                h6("If available..."),
                wellPanel(withSpinner(
                  plotOutput("map2_1"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Twitter PHRASE Barplot of "NRC" sentiment terms
                h4("Barplot of Tweet Sentiment for Phrase"),
                wellPanel(withSpinner(
                  plotOutput("nrc_barplot_1"),
                  type = 3,
                  color.background = "white"
                  )
                ),
                hr(),
                # Create the "jump to top of page" button
                useShinyjs(),
                extendShinyjs(text = jscode4),
                actionButton("toTop4", "jump to top"),
                hr(),
                hr()
              )
            ),
            
            tabPanel(
              "Trends Search / City",
              icon = icon("map-o"),
              #This is where the main TWITTER table output is shown for LOCATIONS
              mainPanel(
                hr(),
                fluidRow(
                  h4("Search Trending Hashtags by City"),
                  wellPanel(selectInput(
                    "nation",
                    "Select Nation",
                    choices = c("No Nation Selected", country_city_list$Country)
                  )
                ),
                  # This outputs the dynamic UI component
                  uiOutput("ui")
                ),
                hr(),
                dataTableOutput("city_1"),
                hr(),
                useShinyjs(),
                extendShinyjs(text = jscode5),
                actionButton("toTop5", "jump to top"),
                hr()
              )
            )
          )
        )
      ),

      ######################################
      ########--Begin Reddit Page--#########
      ######################################
      tabPanel(
        "Reddit",
        icon = icon("reddit"),
        sidebarPanel(
          #This is the sidebar
          sidebarMenu(
            hr(),
            icon("reddit"),
            "Reddit Username/Word Output",
            verbatimTextOutput("redditText", placeholder = 1),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr()
          )
        ),
        mainPanel(tabsetPanel(
          tabPanel(
            "Username/Term Search",
            icon = icon("users"),
            #This is where the main REDDIT table output is shown
            hr(),
            h4("Search for Reddit Usernames or Terms"),
            wellPanel(
              sidebarSearchForm(textId = "redditText", 
                                buttonId = "searchButton",
                                "Username/Term ...")
            ),
            hr(),
            withSpinner(dataTableOutput("reddit2"), type = 7),
            hr(),
            hr()
          ),
          
          tabPanel(
            "Reddit Visualizations",
            icon = icon("area-chart"),
            hr(),
            h4("Visalization of selected Reddit Comment Thread"),
            wellPanel(
              plotOutput("reddit_comment_plot"),
              type = 3,
              color.background = "white"
            )
          )
        )
      )
    ),
      
      tabPanel(
        "Media Links",
        icon = icon("newspaper-o"),
        sidebarPanel(
          #This is the sidebar
          sidebarMenu(
            hr(),
            icon("newspaper-o"),
            "Media Link Output",
            verbatimTextOutput("mediaText", placeholder = 1),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr(),
            hr()
          )
        ),
        mainPanel(#This is the main page section where the output is shown
          tabsetPanel(
            tabPanel(
              "Link Search",
              icon = icon("table"),
              #This is where the main TWITTER table output is shown
              hr(),
              h4("Search how Popular a Link is"),
              wellPanel(
                sidebarSearchForm(textId = "mediaText", 
                                  buttonId = "searchButton",
                                  "Paste link here ...")
              ),
              hr(),
              withSpinner(dataTableOutput("media_1"), type = 7),
              hr(),
              useShinyjs(),
              extendShinyjs(text = jscode6),
              actionButton("toTop6", "jump to top"),
              hr()
            ),
            
            tabPanel(
              "Article Summary",
              icon = icon("area-chart"),
              hr(),
              numericInput(
                "num2",
                label = h4("Select how long to make summary"),
                value = 3
              ),
              wellPanel(
                uiOutput("summary_output"),
                type = 3,
                color.background = "white"
              )
            )
          )
        )
      ),
      
      ######################################
      #######--Begin Downloads Page--#######
      ######################################
      # This panel helps users download their searches
      #     tabPanel("Download", icon = icon("cloud-download"),
      #              mainPanel(
      #                 fluidPage(
      #                  box(title = "Download Analysis",
      #                     "THIS PAGE IS UNDER CONSTRUCTION." )),
      #              hr(),
      #
      #           tags$h5("Deafult actionButton:"),
      #           actionButton("action", "Search"),
      #
      #        tags$h5("actionButton with CSS class:"),
      #        actionButton("action2", "Action button", class = "btn-primary"))),
      
      ######################################
      #######---Begin Contact Page---#######
      ######################################
      tabPanel(
        "Contact",
        icon = icon("envelope"),
        mainPanel(
          fluidPage(box(title = "Contact",
                        "THIS PAGE IS UNDER CONSTRUCTION.")),
          hr(),
          fluidPage(
            textInput("name",
                      "Full Name",
                      "First Last ..."),
            
            textInput("emailText",
                      "Email",
                      "email address ..."),
            
            textInput("descriptionText",
                      "Description",
                      "feedback / bugs / etc ..."),
            
            actionButton("submit", "Submit", class = "btn-primary"),
            hr(),
            hr()
          )
        )
      )
    )
  ),
  
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################  
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  ######----Begin Server----##########################################
  ####################################################################
  ################################
  server = function(input, output) {
    ########################################
    #######--Twitter Username Table--#######
    ########################################
    output$twitterText = renderText({
      paste(input$twitterText)
    })
    
    output$timeline_1 = renderDataTable(data.table({
      validate(need(input$twitterText != "", "Table will output here"))
      timeline_5 = get_timeline(paste(input$twitterText), n = as.numeric(paste(input$num)))
      timeline_0 = timeline_5[c(2, 4, 5, 6, 11, 13, 14)]
      colnames(timeline_0) = c(
        "Time_Stamp",
        "Screen_Name",
        "Tweet",
        "Source",
        "Is_ReTweet?",
        "ReTweet_Count",
        "Hashtags"
      )
      
      # Logging on cloud doc
      data = paste(input$twitterText)
      ss = gs_title("testing")
      read = gs_read_cellfeed(ss, ws = "TwitterUsername", range = "D1")
      read = data.table(read)
      anchorX = read[1, 5]
      log = gs_title("testing")
      log = log %>%
        gs_edit_cells(
          ws = "TwitterUsername",
          input = data,
          byrow = FALSE,
          anchor = paste(anchorX)
        )

      if ((input$radio) == 1) {
        timeline_0
      }
      else if ((input$radio) == 2) {
        r = timeline_0[!(timeline_0$'Is_ReTweet?' == "TRUE"), ]
        r
      }
    }
  )
)
    ########################################
    #######----Twitter Pic Swiper ----######
    ########################################
    
    card_swipe = callModule(shinyswipr, "quote_swiper")
    
    appVals = reactiveValues(

      quote = fortune(),
      swipes = data.frame(
      quote = character(),
      author = character(),
      swipe = character()
      )
    )
    
    our_quote = isolate(appVals$quote)
    output$quote = renderText({
      our_quote$quote
    })
    
    output$quote_author = renderText({
        paste0("-", our_quote$author)
    })
    
    output$resultsTable = renderDataTable({
      appVals$swipes
    })
    
    observeEvent(card_swipe(), {
      #Record our last swipe results.
      appVals$swipes = rbind(
        data.frame(
          quote = appVals$quote$quote,
          author = appVals$quote$author,
          swipe = card_swipe()
        ),
      appVals$swipes
    )
    #send results to the output.
    output$resultsTable = renderTable({
      appVals$swipes
    })
      #update the quote
      appVals$quote = fortune()
      
      #send update to the ui.
      output$quote = renderText({
        appVals$quote$quote
    })
      output$quote_author =
        renderText({
          paste0("-", appVals$quote$author)
    })
  }) 

    ########################################
    #####--Twitter Username WordCloud--#####
    ########################################
    output$wordcloud10 =  renderPlot({
      validate(need(input$twitterText != "", "Please Input a Twitter Username"))
      timeline = get_timeline(paste(input$twitterText))
      corpusTestAll = Corpus(VectorSource(timeline$text))
      corpusTestAll = tm_map(corpusTestAll, tolower)
      corpusTestAll = tm_map(corpusTestAll, removePunctuation)
      corpusTestAll = tm_map(corpusTestAll, removeWords, stopwords("English"))
      cloud = wordcloud(
        corpusTestAll,
        scale = c(4, .5),
        max.words = input$words,
        random.order = FALSE,
        rot.per = 0.35,
        use.r.layout = FALSE,
        colors = brewer.pal(8, "Dark2")
      )
      cloud
    })
    
    ########################################
    ###-Bing top terms sentiment Username-##
    ########################################
    output$bing_top10 =  renderPlot({
      validate(need(input$twitterText != "", "Pos/Neg Bar Plot will output here"))
      #custom stop words (to remove trump) - complete
      custom_stop_words =
        bind_rows(data_frame(word = c("trump"), lexicon = c("custom")), stop_words)
      srch3 = get_timeline(paste(input$twitterText), n = 100)
      Word_timeline_sentiment3 =
        unnest_tokens(srch3, word, text) %>%
        select(word) %>%
        left_join(get_sentiments("bing")) %>%
        anti_join(custom_stop_words) %>%
        na.omit() %>%
        count(word, sentiment, sort = 1) %>%
        ungroup()
      Word_timeline_sentiment3 %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap( ~ sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
             coord_flip()
    })
    
    ########################################
    ####--Sentiment Username WordCloud--####
    ########################################
    output$sentiment_wordcloud =  renderPlot({
      validate(need(
        input$twitterText != "",
        "Sentiment Wordcloud will output here"
      ))
      custom_stop_words =
        bind_rows(data_frame(word = c("trump"), lexicon = c("custom")), stop_words)
      
      srch2 = get_timeline(paste(input$twitterText), n = 100)
      Word_timeline_sentiment2 =
        unnest_tokens(srch2, word, text, to_lower = 1) %>%
        select(word) %>%
        left_join(get_sentiments("bing")) %>%
        anti_join(custom_stop_words) %>%
        na.omit() %>%
        count(word, sentiment, sort = 1) %>%
        ungroup()
      data.table(Word_timeline_sentiment2) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("lightcoral", "cyan3"),
                         max.words = 100)
    })
    #########################################
    #########---Freq Plot Username---########
    #########################################
    output$tweetFreq = renderPlot({
      validate(need(input$twitterText != "", "Frequency Plot will output here"))
      timeline = get_timeline(paste(input$twitterText), n = 100)
      ts_plot(data = timeline,
              by = "6 hour",
              trim = 0L) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          x = NULL,
          y = NULL,
          title = "Frequency - Twitter posts from past 2 months",
          subtitle = "Tweet counts aggregated using 6 hour intervals",
          caption = paste(input$twitterText)
      )
    })
    ########################################
    #######-Global Map Plot Username-#######
    ########################################
    output$map =  renderPlot({
      validate(need(input$twitterText != "", "Global map will output here"))
      timeline = get_timeline(paste(input$twitterText), n = 1000)
      tweet_USA = lat_lng(timeline)
      par(mar = c(1, 1, 1, 1))
      maps::map("world", lwd = .25)
      with(tweet_USA, points(
        lng,
        lat,
        pch = 20,
        cex = .75,
        col = rgb(0, .3, .7, .75)
      ))
    })
    ########################################
    #########-USA Map Plot Username-########
    ########################################
    output$map2 =  renderPlot({
      validate(need(input$twitterText != "", "USA map will output here"))
      timeline = get_timeline(paste(input$twitterText), n = 1000)
      tweet_USA2 = lat_lng(timeline)
      par(mar = c(2, 2, 2, 2))
      maps::map("state", lwd = .25)
      with(tweet_USA2, points(
        lng,
        lat,
        pch = 20,
        cex = .75,
        col = rgb(0, .3, .7, .75)
      ))
    })
    ########################################
    ###-NRC sentiment barchart Username-####
    ########################################
    output$nrc_barplot =  renderPlot({
      validate(need(input$twitterText != "", "Emotions Bar Plot will output here"))
      #custom stop words (to remove trump) - complete
      custom_stop_words =
        bind_rows(data_frame(word = c("trump"), lexicon = c("custom")), stop_words)
      srch4 = get_timeline(paste(input$twitterText), n = 100)
      NRC_sentiment2 =
        unnest_tokens(srch4, word, text) %>%
        select(word) %>%
        left_join(get_sentiments("nrc")) %>%
        anti_join(custom_stop_words) %>%
        na.omit() %>%
        count(sentiment)
      par(mar = c(10, 4, 4, 2) + .1)
      barplot(
        NRC_sentiment2$n,
        #Y-axis
        names.arg = c(
          "Anger",
          "Anticipation",
          "Disgust",
          "Fear",
          "Joy",
          "Negative",
          "Positive",
          "Sadness",
          "Surprise",
          "Trust"
        ),
        #X-axis
        col = c(
          "lightcoral",
          "grey",
          "lightcoral",
          "lightcoral",
          "cyan3",
          "lightcoral",
          "cyan3",
          "lightcoral",
          "grey",
          "cyan3"
        ),
        xlab = "",
        #x axis title
        las = 2
      )
      legend(
        "topleft",
        legend = c("Positive", 
                   "Neutral", 
                   "Negative"),
        fill = c("cyan3", 
                 "grey", 
                 "lightcoral")
      )
    })
    output$twitterPhraseText = renderText({
      paste(input$twitterPhraseText)
    })
    ########################################
    #######--Twitter Phrase Search--########
    ########################################
    output$phrase_1 = renderDataTable(data.table({
      validate(need(input$twitterPhraseText != "", "Table will output here"))
      data2 = paste(input$twitterPhraseText)
      ss = gs_title("testing")
      read2 = gs_read_cellfeed(ss, ws = "TwitterPhrase", range = "D1")
      read2 = data.table(read2)
      anchorXX = read2[1, 5]
      log2 = gs_title("testing")
      log2 = log2 %>%
        gs_edit_cells(
          ws = "TwitterPhrase",
          input = data2,
          byrow = FALSE,
          anchor = paste(anchorXX)
        )
      phrase = search_tweets(paste(input$twitterPhraseText),
                             n = 100,
                             retryonratelimit = TRUE)
      phrase_1 = phrase[c(2, 4, 5, 6, 11, 13, 14)]
      colnames(phrase_1) = c(
        "Time_Stamp",
        "Screen_Name",
        "Tweet",
        "Source",
        "Is_ReTweet?",
        "ReTweet_Count",
        "Hashtags"
      )
      phrase_1
    }))
    ########################################
    #####--Twitter Phrase WordCloud--#####
    ########################################
    output$wordcloud =  renderPlot({
      validate(need(input$twitterPhraseText != "", "Please Input a Word Search"))
      phrase2 = search_tweets(paste(input$twitterPhraseText),
                              n = 500,
                              retryonratelimit = TRUE)
      corpus2 = Corpus(VectorSource(phrase2$text))
      corpus2 = tm_map(corpus2, tolower)
      corpus2 = tm_map(corpus2, removePunctuation)
      corpus2 = tm_map(corpus2, removeWords, stopwords("English"))
      cloud_2 = wordcloud(
        corpus2,
        scale = c(4, .4),
        max.words = input$words_1,
        random.order = FALSE,
        rot.per = 0.35,
        use.r.layout = FALSE,
        colors = brewer.pal(8, "Dark2")
      )
      cloud_2
    })
    
    ########################################
    ###-Bing top terms sentiment Phrase-####
    ########################################
    output$bing_top101 =  renderPlot({
      validate(need(
        input$twitterPhraseText != "",
        "Pos/Neg Bar Plot will output here"
      ))
      #custom stop words (to remove trump) - complete
      custom_stop_words =
        bind_rows(data_frame(word = c("trump", "cloud","Ibm"), lexicon = c("custom")), stop_words)
      srch5 = get_timeline(paste(input$twitterPhraseText), n = 500)
      Word_timeline_sentiment5 =
        unnest_tokens(srch5, word, text) %>%
        select(word) %>%
        left_join(get_sentiments("bing")) %>%
        anti_join(custom_stop_words) %>%
        na.omit() %>%
        count(word, sentiment, sort = 1) %>%
        ungroup()
      Word_timeline_sentiment5 %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap( ~ sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()
    })
    
    ########################################
    ####--Sentiment Username WordCloud--####
    ########################################
    output$sentiment_wordcloud_10 =  renderPlot({
      validate(need(
        input$twitterPhraseText != "",
        "Sentiment Wordcloud will output here"
      ))
      custom_stop_words =
        bind_rows(data_frame(word = c("trump","cloud","Ibm"), lexicon = c("custom")), stop_words)
      
      srch9 = get_timeline(paste(input$twitterPhraseText), n = 500)
      Word_timeline_sentiment9 =
        unnest_tokens(srch9, word, text, to_lower = 1) %>%
        select(word) %>%
        left_join(get_sentiments("bing")) %>%
        anti_join(custom_stop_words) %>%
        na.omit() %>%
        count(word, sentiment, sort = 1) %>%
        ungroup()
      data.table(Word_timeline_sentiment9) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("lightcoral", "cyan3"),
                         max.words = 100)
    })
    #########################################
    #########----Freq Plot Phrase----########
    #########################################
    output$tweetFreq_1 = renderPlot({
      validate(need(
        input$twitterPhraseText != "",
        "Frequency Plot will output here"
      ))
      timeline8 = get_timeline(paste(input$twitterPhraseText), n = 500)
      ts_plot(data = timeline8,
              by = "6 hour",
              trim = 0L) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          x = NULL,
          y = NULL,
          title = "Frequency - Twitter posts from past 2 months",
          subtitle = "Tweet counts aggregated using 6 hour intervals",
          caption = paste(input$twitterPhraseText)
       )
    })
    
    ########################################
    #####-Global Map Plot Phrase-#######
    ########################################
    
    output$map_1 =  renderPlot({
      validate(need(input$twitterPhraseText != "", "Global map will output here"))
      phrase = search_tweets("taylor swift", n = 100, retryonratelimit = TRUE)
      tweet_USA3 = lat_lng(phrase)
      par(mar = c(1, 1, 1, 1))
      maps::map("world", lwd = .25)
      with(tweet_USA3, points(
        lng,
        lat,
        pch = 20,
        cex = .75,
        col = rgb(0, .3, .7, .75)
      ))
    })
    
    ########################################
    #########--USA Map Plot Phrase--########
    ########################################
    output$map2_1 =  renderPlot({
      validate(need(input$twitterPhraseText != "", "USA map will output here"))
      timeline7 = get_timeline(paste(input$twitterPhraseText), n = 100)
      tweet_USA4 = lat_lng(timeline7)
      par(mar = c(2, 2, 2, 2))
      maps::map("state", lwd = .25)
      with(tweet_USA4, points(
        lng,
        lat,
        pch = 20,
        cex = .75,
        col = rgb(0, .3, .7, .75)
      ))
    })
    
    ########################################
    ####-NRC sentiment barchart Phrase-#####
    ########################################
    output$nrc_barplot_1 =  renderPlot({
      validate(need(
        input$twitterPhraseText != "",
        "Sentiment Bar Plot will output here"
      ))
      #custom stop words (to remove trump) - complete
      custom_stop_words =
        bind_rows(data_frame(word = c("trump"), lexicon = c("custom")), stop_words)
      srch7 = get_timeline(paste(input$twitterPhraseText), n = 500)
      NRC_sentiment7 =
        unnest_tokens(srch7, word, text) %>%
        select(word) %>%
        left_join(get_sentiments("nrc")) %>%
        anti_join(custom_stop_words) %>%
        na.omit() %>%
        count(sentiment)
      par(mar = c(10, 4, 4, 2) + .1)
      barplot(
        NRC_sentiment7$n,
        #y
        names.arg = c(
          "Anger",
          "Anticipation",
          "Disgust",
          "Fear",
          "Joy",
          "Negative",
          "Positive",
          "Sadness",
          "Surprise",
          "Trust"
        ),
        #x
        col = c(
          "lightcoral",
          "grey",
          "lightcoral",
          "lightcoral",
          "cyan3",
          "lightcoral",
          "cyan3",
          "lightcoral",
          "grey",
          "cyan3"
        ),
        xlab = "",
        #x axis title
        las = 2
      )
      legend(
        "topleft",
        legend = c("Positive", "Neutral", "Negative"),
        fill = c("cyan3", "grey", "lightcoral")
      )
    })
    ########################################
    ######----Nation Trend Search----#######
    ########################################
    output$nationOutput = renderText({
      paste(input$nationOutput)
    })
    
    output$ui = renderUI({
      if (is.null(input$nation))
        return()
      
      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      switch(
        input$nation,
        "No Nation Selected" = wellPanel(selectInput(
          "city",
          "Select city",
          choices = c("No City Selected", country_city_list$City)
        )),
        Argentina = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Argentina"])
        ),
        Australia = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Australia"])
        ),
        Austria = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Austria"])
        ),
        Belgium = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Belgium"])
        ),
        Brazil = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Brazil"])
        ),
        Canada = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Canada"])
        ),
        Chile = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Chile"])
        ),
        Colombia = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Colombia"])
        ),
        Congo = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Congo"])
        ),
        Denmark = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Denmark"])
        ),
        Egypt = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Egypt"])
        ),
        France = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "France"])
        ),
        Germany = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Germany"])
        ),
        Ghana = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Ghana"])
        ),
        Greece = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Greece"])
        ),
        Hungary = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Hungary"])
        ),
        India = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "India"])
        ),
        Indonesia = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Indonesia"])
        ),
        Israel = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Israel"])
        ),
        Italy = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Italy"])
        ),
        Japan = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Japan"])
        ),
        Kuwait = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Kuwait"])
        ),
        Lebanon = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Lebanon"])
        ),
        Malaysia = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Malaysia"])
        ),
        Mexico = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Mexico"])
        ),
        Nigeria = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Nigeria"])
        ),
        Pakistan = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Pakistan"])
        ),
        Peru = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Peru"])
        ),
        Philippines = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Philippines"])
        ),
        Poland = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Poland"])
        ),
        Portugal = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Portugal"])
        ),
        Russia = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Russia"])
        ),
        "Saudi Arabia" = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Saudi Arabia"])
        ),
        Singapore = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Singapore"])
        ),
        "South Africa" = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "South Africa"])
        ),
        "South Korea" = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "South Korea"])
        ),
        Spain = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Spain"])
        ),
        Thailand = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Thailand"])
        ),
        Turkey = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Turkey"])
        ),
        UAE = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "UAE"])
        ),
        UK = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "UK"])
        ),
        USA = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "USA"])
        ),
        Vietnam = wellPanel(
          selectInput("city", "Select city", choices = country_city_list$City[country_city_list$Country ==
                                                                                "Vietnam"])
        )
      )
    })
    
    output$city = renderText({
      input$city
    })
    
    output$city_1 = renderDataTable(data.table({
      city_2 = get_trends(paste(input$city))
      city_3 = city_2[c(1, 2, 5, 6)]
      colnames(city_3) = c("Hashtag Trend", "URL", "Tweet Volume", "Location")
      city_3
    }))
    
    ########################################
    ###-Followers Twitter Username Table-###
    ########################################
    output$followers = DT::renderDataTable(data.table({
      validate(need(
        input$twitterText != "",
        "Twitter User Followers will output here..."
      ))
      twitter_user_follower_ids =
        get_followers(paste(input$twitterText), n = 50)
      twitter_user_follower_info =
        lookup_users(twitter_user_follower_ids$user_id)
      twitter_user_follower_info[, c(2, 3, 4, 5, 8, 17, 13)]
    }))
    ###########selecting followers################
    output$select = renderDataTable(data.table({
      s = input$twitter_user_follower_info_rows_selected
      if (length(s)) {
        twitter_user_follower_ids =
          get_followers(paste(input$twitterText), n = 50)
        twitter_user_follower_info =
          lookup_users(twitter_user_follower_ids$user_id)
        follower_timeline = get_timeline(paste(twitter_user_follower_info[s, 3]))
        follower_timeline
        
      }
    }))
    ########################################
    ########--Reddit Phrase Table---########
    ########################################
    output$redditText = renderText({
      paste(input$redditText)
    })
    
    output$reddit2 = renderDataTable(data.table({
      validate(need(input$redditText != "", "This table may take a few minutes..."))
      data4 = paste(input$redditText)
      ss = gs_title("testing")
      read4 = gs_read_cellfeed(ss, ws = "RedditSearch", range = "D1")
      read4 = data.table(read4)
      anchorXXXX = read4[1, 5]
      log4 = gs_title("testing")
      log4 = log4 %>%
        gs_edit_cells(
          ws = "RedditSearch",
          input = data4,
          byrow = FALSE,
          anchor = paste(anchorXXXX)
        )
      term = paste(input$redditText) # term for which to search --> can also do "Example | Example"
      Reddit_Search_Return = get_reddit(
        search_terms = term,
        cn_threshold = 25,
        page_threshold = 1,
        sort_by = "comments",
        wait_time = .5
      )
      reddit_1 = Reddit_Search_Return[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 16, 17)]
      reddit_1
    }))
    
    ########################################
    #####--Reddit Comment Thread Plot--##### Continue working on this (cant get the row click functionality to work)
    ########################################
    #Graphing a reddit comment thread
    # print the selected indices
    output$reddit_comment_plot = renderPlot({
      s = input$x3_rows_selected
      if (length(s)) {
        x = reddit_1[s, 12]
      }
      url_data = reddit_content(x)
      graph_object = construct_graph(url_data)
    })
    
    
    #  output$reddit_comment_plot = renderPlot({
    #   validate(
    #    need(input$redditText != "", "Select a row from the Reddit Table Output to show a comment thread"))
    
    #   reddit_plot_url = "https://www.reddit.com/r/InternetIsBeautiful/comments/3oeaz1/summarize_articles_editorials_and_essays/"
    
    
    ########################################
    ########----Media URL Search----########
    ########################################
    output$mediaText = renderText({
      paste(input$mediaText)
    })
    
    output$value1 = renderPrint({
      input$num
    })
    
    
    
    output$summary_output = renderTable({
      my_url = paste(input$mediaText)
      
      #read page html
      page = xml2::read_html(my_url)
      #extract text from page html using selector
      page_text = rvest::html_text(rvest::html_nodes(page, "p"))
      
      #perform lexrank for top 3 sentences
      tops = lexRankr::lexRank(
        page_text,
        #only 1 article; repeat same docid for all of input vector
        docId = rep(1, length(page_text)),
        #return 3 sentences to mimick /u/autotldr's output
        n = as.numeric(paste(input$num2)),
        continuous = TRUE
      )
      
      #reorder the top 3 sentences to be in order of appearance in article
      order_of_appearance = order(as.integer(gsub("_", "", tops$sentenceId)))
      #extract sentences in order of appearance
      ordered_tops = tops[order_of_appearance, "sentence"]
      SentimentColumn = as.data.frame(sentiment(ordered_tops))
      ordered_tops_df = as.data.frame(ordered_tops)
      merge_ordered_tops = merge(ordered_tops_df, SentimentColumn, all = TRUE)
      
      #   ordered_tops_df$h = SentimentColumn
      colnames(merge_ordered_tops) = c("Summary",
                                       "Element_ID",
                                       "Sentence_ID",
                                       "#Words",
                                       "Sentiment")
      
      if ((as.numeric((paste(input$num2)))) == 1) {
        merge_ordered_tops[1, c(1, 2, 3, 5)]
      } else if ((as.numeric((paste(input$num2)))) ==
                 2) {
        merge_ordered_tops[c(1, 4), c(1, 2, 3, 5)]
      } else if ((as.numeric((paste(input$num2)))) ==
                 3) {
        merge_ordered_tops[c(1, 5, 9), c(1, 2, 3, 5)]
      } else if ((as.numeric((paste(input$num2)))) ==
                 4) {
        merge_ordered_tops[c(1, 6, 11, 16), c(1, 2, 3, 5)]
      } else if ((as.numeric((paste(input$num2)))) ==
                 5) {
        merge_ordered_tops[c(1, 7, 12, 19, 25), c(1, 2, 3, 5)]
      } else if ((as.numeric((paste(input$num2)))) >
                 5) {
        paste("More than 5 sentences kinda defeats the purpose of a 'Summary' ...")
      }
    })
    
    
    output$media_1 = renderDataTable(data.table({
      validate(need(input$mediaText != "", "Table will output here"))
      data3 = paste(input$mediaText)
      ss = gs_title("testing")
      read3 = gs_read_cellfeed(ss, ws = "MediaLink", range = "D1")
      read3 = data.table(read3)
      anchorXXX = read3[1, 5]
      log3 = gs_title("testing")
      log3 = log3 %>%
        gs_edit_cells(
          ws = "MediaLink",
          input = data3,
          byrow = FALSE,
          anchor = paste(anchorXXX)
        )
      media = get_socialmedia(paste(input$mediaText))
      media_1 = media[c(3, 4, 5, 8, 9, 11, 12)]
      colnames(media_1) = c(
        "FB_Shares",
        "FB_Comments",
        "Reddit_Score",
        "Reddit_Comments",
        "LinkedIn_Shares",
        "Pinterest_Shares",
        "Media_Link"
      )
      media_1
    }))
    
    # Top of page Button
    observeEvent(input$toTop, {
      js$toTop()
      
    })
    observeEvent(input$toTop2, {
      js$toTop2()
      
    })
    observeEvent(input$toTop3, {
      js$toTop3()
      
    })
    observeEvent(input$toTop4, {
      js$toTop4()
      
    })
    observeEvent(input$toTop5, {
      js$toTop5()
      
    })
    observeEvent(input$toTop6, {
      js$toTop6()
      
    })
  })

##########################################################################################
