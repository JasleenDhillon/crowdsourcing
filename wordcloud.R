# loading packages
installed.packages("twitteR")
installed.packages("ROAuth")
installed.packages("tidyverse")
installed.packages("purrrlyr")
installed.packages("text2vec")
installed.packages("caret")
installed.packages("glmnet")
installed.packages("ggrepel")
installed.packages("dplyr")
library(wordcloud)

library(ggplot2)
library(twitteR)
library(dplyr)
library(ROAuth)
library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)  
library(ggrepel)

download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "cacert.pem")
setup_twitter_oauth('2xctDH4h84IXBLWNuZXNxhZHg', # api key
                    'a2nKa073qM1seql5q1NYL7v6qUHA6h8L162G7cC3P882i1cVOH', # api secret
                    '769939122601467904-mshiDdlJH4RYZuKFEguStgedx6zWbO8', # access token
                    'QLnFqfItj4ip2SmeuXh52S8vZWD4Q5mMmqfiYTyUkQu2v' # access token secret
)

df_tweets <- read_csv('datafinal.csv', col_names = c('id', 'sentiment', 'text','ab')) %>%
#df_tweets <- read_csv('datafinal.csv', col_names = c('id', 'sentiment', 'text','ab')) %>%
 # converting some symbols
  dmap_at('text', conv_fun)

x<-twListToDF(df_tweets$text)  
tweets <- data.frame(x)
iconv(tweets$text, from="UTF-8", to="ASCII", sub="")
tweets$text=str_replace_all(tweets$text,"[^[:graph:]]", " ") 
tweets$text <- gsub("[^[:alnum:]///' ]", "", tweets$text) 
tweets$text <- tolower(tweets$text)
tweets$text <- gsub("rt", "", tweets$text)
tweets$text <- gsub("[[:punct:]]", "", tweets$text)



#Remove tabs            
tweets$text <- gsub("[ |\t]{2,}", "", tweets$text)

#Remove blankspaces at begining            
tweets$text <- gsub("^ ", "", tweets$text)

#Remove blankspaces at the end            
tweets$text <- gsub(" $", "", tweets$text)

#Remove usernames 
tweets$text <- gsub("@\\w+", "", tweets$text)

tweets <- Corpus(VectorSource(tweets))

tweets <- tm_map(tweets, removeWords, stopwords("en"))
wordcloud(tweets,min.freq = 3, scale=c(6,0.5),colors=brewer.pal(8, "Dark2"),random.color= FALSE, random.order = FALSE, max.words = 110)