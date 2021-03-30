library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal())

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

bigrams_filtered <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  {.}

bigram_tf_idf <- bigrams_filtered %>%
  drop_na(word1) %>% 
  unite(bigram, word1, word2, sep = " ") %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>% 
  group_by(book) %>% 
  top_n(5, tf_idf) %>% 
  ggplot(aes(x = tf_idf, 
             y = forcats::fct_reorder(bigram, tf_idf),
             fill = book)) +
  geom_col() +
  facet_wrap(~ book, scales = 'free') +
  rcartocolor::scale_fill_carto_d() +
  guides(fill = FALSE) +
  labs(y = NULL)


# Exercise 2

austen_trigrams <- austen_books() %>%
  filter(book == 'Sense & Sensibility') %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) 

my_stop_words <- stop_words$word

austen_trigrams %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") -> check # %>%
  filter(!word1 %in% my_stop_words,
         !word2 %in% my_stop_words,
         !word3 %in% my_stop_words) %>%
  drop_na(word1) %>%
  unite(trigram, word1, word2, word3, sep = " ") %>%
  filter(n > 1) %>% 
  ggplot(aes(x = n, 
           y = forcats::fct_reorder(trigram, n))) +
  geom_col() +
  labs(y = NULL)

  
  