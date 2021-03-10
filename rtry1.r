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

#badiya aadmni ne likhar text2vec

# tweets ke andar ke symbols convert krne ke liye
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

#text contain tweets
#labeled_data file jo use kar rhe hai 
#sentiment 1,2,3 
tweets_classified <- read_csv('labeled_data.csv', col_names = c('id', 'sentiment', 'text')) %>%
  # converting some symbols
  dmap_at('text', conv_fun) %>%
  
 
  mutate(sentiment = ifelse(sentiment == 0,0,1))


# there are some tweets with NA ids jo hum abhi hata denge tabhi yeh kiya hai

tweets_classified_na <- tweets_classified %>%
  filter(is.na(id) == TRUE) %>%
  mutate(id = c(1:n()))
tweets_classified <- tweets_classified %>%
  filter(!is.na(id)) %>%
  rbind(., tweets_classified_na)


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

# vocab banao aur dtm 
##################
stop_words = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")
t1 = Sys.time()
vocab = create_vocabulary(it_train, stopwords = stop_words)
print(difftime(Sys.time(), t1, units = 'sec'))


################
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

# train the model- takes time tfidf ko kara train
t1 <- Sys.time()
#################
has_NA = apply(is.na(dtm_train_tfidf), 1, any) #= 1 if any column in that row is NA
dtm_train_tfidf = dtm_train_tfidf[!has_NA,]
tweets_train =tweets_train[!has_NA,]
#####################

glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf,
                               y = tweets_train[['sentiment']], 
                               family = 'binomial', 
                              
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

plot(glmnet_classifier,xlab=expression(paste("log(",lambda,")")))
#plot(glmnet_classifier)
print(paste("max AUC(area under curve) =", round(max(glmnet_classifier$cvm), 4)))

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
#auc(as.numeric(tweets_test$sentiment), preds)

# save the model for future using
saveRDS(glmnet_classifier, 'glmnet_classifier.RDS')
#######################################################