### fetching tweets ###
library(twitteR)
library(ggplot2)
library(xlsx)
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

#df_tweets <- twListToDF(searchTwitter('#RSS or rss #rss or RSS', n = 500, lang = 'en')) %>%
  # converting some symbols
df_tweets<- read_csv('sheetk.csv', col_names = c('text','hashs')) %>%
  dmap_at('text', conv_fun)

str(df_tweets)

# preprocessing and tokenization
it_tweets <- itoken(df_tweets$text,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    
                    progressbar = TRUE)

# creating vocabulary and document-term matrix
dtm_tweets <- create_dtm(it_tweets, vectorizer)

# transforming data with tf-idf
dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)

# loading classification model
glmnet_classifier <- readRDS('glmnet_classifier.RDS')

# predict probabilities of positiveness
preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]

#preds_tweets
# adding rates to initial dataset
df_tweets$sentiment <- preds_tweets+0.15
str(df_tweets)
df_tweets$sentiment
counta=0
str(df_tweets)
for(val in df_tweets$sentiment)
{
  if(val<0.30)
  {
    #print(val)
    #print(df_tweets$text[counta])
    write_excel_csv(df_tweets[counta,],path="sheetk2.csv",append = TRUE)
  }
  counta=counta+1
}

# color palette
cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")
#cols <- c("#ce472e", "#eec73a", "#ffd73e", "#4ab04a", "#f05336")
#cols <- c("#ce472e", "#ce472e", "#ce472e", "#ce472e", "#ce472e")4ab04a
set.seed(932)
samp_ind <- sample(c(1:nrow(df_tweets)), nrow(df_tweets) * 0.1) # 10% for labeling

# plotting
ggplot(df_tweets, aes(x = created, y = sentiment, color = sentiment)) +
  theme_minimal() +
  scale_color_gradientn(colors = cols, limits = c(0, 1),
                        breaks = seq(0, 1, by = 1/4),
                        labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
                        guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_point(aes(color = sentiment), alpha = 0.8) +
  geom_hline(yintercept = 0.7, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_hline(yintercept = 0.33, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_smooth(size = 1.2, alpha = 0.2) +
  geom_label_repel(data = df_tweets[samp_ind, ],
                   aes(label = round(sentiment, 2)),
                   fontface = 'bold',
                   size = 2.5,
                   max.iter = 100) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
  ggtitle("Tweets Sentiment rate (probability of positiveness)")


