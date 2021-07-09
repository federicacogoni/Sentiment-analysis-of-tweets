#load packages
library(igraph)
library(tidyverse)
library(rtweet)
library(readtext)
library(readr)
library(tidyr)
library(ggplot2)
library(data.table)
library(stringr)
library(rlang)
library(tidytext)
library(textdata)
library(dplyr)

#setup the apikey (to scrape twitter: developer> apps> pop_democracy > keys and tokens) 
api_key <- "xxx"
api_secret_key <-  "xxx"

#authenticate via web browser
token <- create_token(
  app= "pop-democracy",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = "xxx",
  access_secret= "xxx"
)

#or just run this
get_token()

#search tweets by hashtags (some examples)
debate <- search_tweets("#PresidentialDebate2020", n = 18000, include_rts = FALSE)
elections <- search_tweets("#USA2020", n = 18000, include_rts = FALSE)
elections_a <- search_tweets("#Usa2020",n = 18000, include_rts = FALSE)
elections_b <- search_tweets("#PresidentialElections",n = 18000, include_rts = FALSE)
vote <- search_tweets("#Vote", n = 18000, include_rts=FALSE)
vote_a <- search_tweets("#vote", n = 18000, include_rts = FALSE)


#save in RDS format
debate <- saveRDS(debate,"debate.csv")

#readRDS 
deb <- readRDS("debate.csv",refhook = NULL)



#P A R T  - II - VISUALIZATION#

#clean dataset - this is a simple way to remove weird emojis and characters
setwd("C:/Users/fcogo/Desktop/project/")
c <- readtext("final_classified_id.csv")

list_weird_emojis <-c("@\\S*", "amp", "[\r\n]","[[:punct:]]","ðÿ","â","ð","ï","im","ã",
                      "ÿ","œ", "š","ÿž","te", "ŸŽ")

c$cleaned_text <- gsub("https\\S*", "", c$stripped_text) 
for (variable in list_weird_emojis){
  c$cleaned_text <- gsub(variable, "", c$cleaned_text) 
}

#pre-processing
tweets_words <-  sentiment %>%
  select(cleaned_text) %>%
  unnest_tokens(word, cleaned_text)%>%
  anti_join(stop_words)
#dataframe
words <- tweets_words %>% 
  count(word, sort=TRUE)
words <- words[-1,]

#cloud
wordcloud2(data=words, size=1.6, color='random-dark')
#this one
wordcloud2(data=words, size = 0.7, shape = 'pentagon')

##maybe only republican and democrats and make two different clouds?
#R E P U B L I C A N  - DESCRIPTIONS
#republican wordcloud
rep <- sentiment[which(sentiment$ideologies==1),]
#preprocessing
tweets_rep <-  rep %>%
  select(cleaned_text) %>%
  unnest_tokens(word, cleaned_text)%>%
  anti_join(stop_words)
#df
words_rep <- tweets_rep %>% 
  count(word, sort=TRUE)
words_rep <- words_rep[-1,]
#cloud
wordcloud2(data=words_rep, size = 0.7, shape = 'pentagon')

#D E M O C R A T S  - DESCRIPTIONS
dem <- sentiment[which(sentiment$ideologies==2),]
#preprocessing
tweets_dem <-  dem %>%
  select(cleaned_text) %>%
  unnest_tokens(word, cleaned_text)%>%
  anti_join(stop_words)
#df
words_dem <- tweets_dem %>% 
  count(word, sort=TRUE)
words_dem <- words_dem[-1,]
#cloud
wordcloud2(data=words_dem, size = 0.7, shape = 'pentagon')



## SENTIMENT ANALYSIS ##

#import dictionary
dictionary <-  get_sentiments("bing")
id$hashtag_sep <- NA

#PresidentialDebate2020 - query hashtag
id$detect <- str_detect(id$tweet, "PresidentialDebate2020")
id$tweet <- as.character(id$tweet)
for (i in 1:nrow(id)) {
  if(id$detect[i] == "TRUE") {
    id$hashtag_sep[i] <- "PresidentialDebate2020"
  } else {
    if(id$detect[i] == "FALSE") {
      id$hashtag_sep[i] <- "Other"
    }  
  }
}

## create a dataframe with the dictionary and tweets
d_2 <- id %>%
  unnest_tokens(word, tweet)%>%
  anti_join(stop_words)
#appendo il dizionario 
d_3 <- d_2 %>%
  inner_join(dictionary)
#positive - negative cols
debate_sentiment <- d_3 %>%
  count(screen_name, sentiment, text, lang, description,verified,stripped_text, true_rep, true_dem, ideologies, stripped_tweet, hashtag_sep)%>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive-negative) %>%
  rename(date = text) %>%
  arrange(date)


## visualization of sentiments ##
tweet_score <- readtext("poltweet_score.csv")
#TRUMP - wordcloud
trump <- tweet_score%>%
  subset(trumpists == 1)
#preprocessing
tweets_rep <-  trump %>%
  select(cleaned_tweet) %>%
  unnest_tokens(word, cleaned_tweet)%>%
  anti_join(stop_words)
#dataframe
words_rep <- tweets_rep %>% 
  count(word, sort=TRUE)

#word-cloud
wordcloud2(data=words_rep, size = 0.7, shape = 'pentagon')
rm(tweets_rep)































