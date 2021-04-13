library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(stringr)
library(scales)
library(tidyr)
theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())

tweets_julia <- read_csv("data/tweets_julia.csv")
tweets_dave <- read_csv("data/tweets_dave.csv")
tweets <- bind_rows(
  tweets_julia %>%
    mutate(person = "Julia"),
  tweets_dave %>%
    mutate(person = "David")
) %>%
  mutate(timestamp = ymd_hms(timestamp))

ggplot(tweets, aes(x = timestamp, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)


remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(
    !word %in% stop_words$word,
    !word %in% str_remove_all(stop_words$word, "'"),
    str_detect(word, "[a-z]")
  ) %>%
  {
    .
  }

# frequency <- tidy_tweets %>%
#   group_by(person) %>%
#   mutate(total = n()) %>%
#   count(person, word, total, sort = TRUE) %>%
#   mutate(freq = n/total)
#
# frequency

frequency <- frequency %>%
  select(person, word, freq) %>%
  pivot_wider(names_from = person, values_from = freq) %>%
  arrange(Julia, David)
frequency

ggplot(frequency, aes(Julia, David)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


tidy_tweets <- tidy_tweets %>%
  filter(
    timestamp >= as.Date("2016-01-01"),
    timestamp < as.Date("2017-01-01")
  )

word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = person, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, ~ (. + 1) / (sum(.) + 1)) %>%
  mutate(logratio = log(David / Julia)) %>%
  arrange(desc(logratio))

## Exercise 1
top_15_words <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 15)

top_15_words %>%
  ggplot(aes(x = n, y = forcats::fct_reorder(word, n))) +
  geom_col() +
  labs(y = NULL, x = NULL)

## Exercise 2

tweet_sent <- tidy_tweets %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  mutate(sum_sentiment = n * (1 - 2 * (sentiment == "negative")))

p <- tweet_sent %>%
  ggplot(aes(
    x = sum_sentiment,
    y = forcats::fct_reorder2(word, sentiment, sum_sentiment),
    fill = sentiment
  )) +
  geom_col() +
  rcartocolor::scale_fill_carto_d() +
  theme(legend.position = "None") +
  labs(x = "Total sentiment", y = NULL)


## Exercise 3
# Make a word cloud of David's tweet
library(wordcloud)

tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  filter(person == "David") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, random.color = T))
?with
