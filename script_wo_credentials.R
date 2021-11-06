library(jsonlite)
library(tidyverse)
library(rtweet)
library(magrittr)
library(maps)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(lubridate)
library(topicmodels)


df <- fromJSON(txt = "ALM_BLM.json", )
nrow(df)
new_df <- df %>% 
  unnest(cols = c("Corpus", "Tweets")) %>% 
  unnest(cols = annotations) %>% 
  mutate(tweet_id = paste("x", tweet_id, sep = ""))

new_df %<>% 
  separate(col = annotation, into = c("ann1", "ann2", "ann3", "ann4", "ann5", "ann6"), sep = ",")  
# in total, maximum annotions given to any tweet, by annotators is 6. 


# authenticate via web browser
token <- create_token(
  app = "ALM BLM and Morality",
  consumer_key = api_key,
  consumer_secret = api_secret_key, 
  access_token = access_token, 
  access_secret = access_token_secret
  )

(tweet_ids_tbl <- new_df %>% 
  distinct(tweet_id))

(tweets <- lookup_statuses(statuses = tweet_ids_tbl$tweet_id, parse = TRUE, token = token))

#write_csv(x = new_df, file = "TweetAnnotations")
#write_as_csv(x = tweets, file_name = "AllTweets")

tweet_annot <- read_csv("TweetAnnotations")
tweets <- read_csv("AllTweets.csv")

tweet_annot_2 <- tweet_annot
  
#tweet_annot_2 <- tweet_annot %>% 
#  mutate(tweet_id = paste("x", tweet_id, sep = ""))



tweets_with_corpus <- left_join(tweets, select(tweet_annot_2, Corpus, tweet_id), by = c("status_id" = "tweet_id")) 
#tweets_with_corpus %>% 
#  select(status_id, Corpus) %>% 
#  filter(is.na(Corpus))

tweets_clean <- tweets_with_corpus %>% 
  select(user_id:text, Corpus, reply_to_status_id, is_quote:symbols, quoted_status_id:quoted_statuses_count, retweet_status_id:retweet_statuses_count, place_name:bbox_coords, followers_count:favourites_count)

# remove URLs
tweets_clean$text <- gsub("https\\S*","", tweets_clean$text)
# remove "@username" tags
tweets_clean$text <- gsub("@\\w+", "", tweets_clean$text) 
#remove all URLs with t.co. 
tweets_clean$text <- gsub("http://t+","", tweets_clean$text)

tweets_tokens <- tweets_clean %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;")) %>% 
  unnest_tokens(word, text, token = "tweets") %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) #%>%
  #mutate(word = SnowballC::wordStem(word))

#Frequently Occuring words in ALM, BLM Corpus.
t2 <- tweets_tokens %>% 
  filter(!is.na(Corpus)) %>% 
  mutate(Corpus = factor(Corpus)) %>% 
  group_by(Corpus) %>% 
  count(word) %>% 
  arrange(n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>%
  group_by(Corpus) %>% 
  top_n(20) 


ggplot(t2) +
  geom_col(mapping = aes(word, n, fill = Corpus), show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~Corpus, scales = "free") 

tweets_tokens$word
#plot
wordcloud(tweets_tokens$word, min.freq = 100)

#positive and negative sentiments
tweets_sentiments <- tweets_tokens %>% left_join(get_sentiments('afinn')) 

#most negative words used by ALM and BLM posts.
tweets_sentiments %>% 
  filter(value < 0) %>% 
  group_by(Corpus) %>% 
  count(word) %>% 
  top_n(15) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot() +
  geom_col(mapping = aes(word, n, fill = Corpus), show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~Corpus, scales = "free") 

#most positive words used by ALM and BLM posts.
tweets_sentiments %>% 
  filter(value > 0) %>% 
  group_by(Corpus) %>% 
  count(word) %>% 
  top_n(15) %>% 
  mutate(word = factor(word)) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot() +
  geom_col(mapping = aes(word, n, fill = Corpus), show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~Corpus, scales = "free") 


#Timeline of ALM vs BLM Tweets. (filtered out the extreme cases)
tweets_sentiments %>% 
  mutate(day = floor_date(ymd_hms(created_at), unit = "day")) %>% 
  group_by(Corpus, day, status_id) %>% 
  summarise(tot_sent = sum(value, na.rm = TRUE)) %>% 
  filter(abs(tot_sent) < 50) %>% 
  ggplot(aes(day, tot_sent, color = Corpus)) +
  geom_jitter(size = 0.5, height = 1, alpha = 0.2) +
  geom_smooth(size = 0.6) 

#Timeline of ALM vs BLM Tweets. (filtered out the extreme cases)
tweets_sentiments %>% 
  mutate(day = floor_date(ymd_hms(created_at), unit = "day")) %>% 
  group_by(Corpus, day) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(day, value, color = Corpus)) +
  geom_jitter(size = 0.5, height = 1, alpha = 0.2) +
  geom_smooth(size = 0.6) 

tweets_clean <- tweets_tokens %>% 
  select(status_id, Corpus, created_at, word) %>% 
  count(Corpus, word, sort = TRUE) 

total_words <- tweets_clean %>%    
  group_by(Corpus) %>% 
  summarise(total = sum(n))

tweets_clean <- left_join(tweets_clean, total_words)

ggplot(tweets_clean, aes(n/total, fill = Corpus)) +
  geom_histogram(show.legend = FALSE, bins = 100) +
  xlim(NA, 0.001) +
  facet_wrap(~Corpus, scales = "free_y") +
  labs(x = "Term Frequency (N / Total)", y = "Count") + 
  theme_bw()

tweets_clean %>% 
  group_by(Corpus, word) %>% 
  summarise(freq = n/total) %>% 
  arrange(desc(freq))

tweets_clean %>% 
  bind_tf_idf(word, Corpus, n) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Corpus) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = Corpus)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Corpus, scales = "free") +
  coord_flip()

tweets_sentiments <- tweets_tokens %>% left_join(get_sentiments('nrc')) 

nrc <- get_sentiments("nrc")
distinct(nrc, sentiment)

#negative affects related to the posts
tweets_sentiments %>% 
  filter(sentiment %in% c("fear", "negative", "anger", "disgust")) %>% 
  group_by(Corpus, sentiment) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>% 
  mutate(word = factor(word)) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot() +
  geom_col(mapping = aes(word, n, fill = Corpus), show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment + Corpus, scales = "free", ncol = 2) 

#positive affects related to the posts
tweets_sentiments %>% 
  filter(sentiment %in% c("surprise", "positive", "joy")) %>% 
  group_by(Corpus, sentiment) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>% 
  mutate(word = factor(word)) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot() +
  geom_col(mapping = aes(word, n, fill = Corpus), show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment + Corpus, scales = "free", ncol = 2) 

data_dtm <- tweets_clean %>% 
  count(Corpus, word) %>% 
  cast_dtm(document = Corpus, term = word, value = n, weighting = tm::weightTf)

# Fine tuning
#data_dtm_trim <- tm::removeSparseTerms(data_dtm, sparse = .99)
#rowTotals <- apply(data_dtm_trim, 1, sum) #Find the sum of words in each Document
#data_dtm_trim <- data_dtm_trim[rowTotals> 0, ] 
#tweets_lda <- LDA(data_dtm_trim, k = 10, control = list(seed = 1234))

tweets_lda <- LDA(data_dtm, k = 10, control = list(seed = 1234))
tweet_topics <- tidy(tweets_lda, matrix = "beta")

# make dataframe showcasing the 10 words with highest beta per topic
tweet_top_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# plot top words for each topic
tweet_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#Annotations Morality Cleaned
tweet_annot_clean <- tweet_annot %>% 
  select(Corpus:ann1)

tweets_morality <- left_join(tweets, select(tweet_annot, Corpus, tweet_id, annotator, ann1), by = c("status_id" = "tweet_id")) %>% 
  select(status_id, created_at, text, Corpus, ann1, annotator, favorite_count, retweet_count, reply_count) 


