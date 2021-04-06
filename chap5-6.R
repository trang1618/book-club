# library(tm.plugin.webmining)
library(purrr)
library(dplyr)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol  <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", 
             "TWTR", "IBM", "YHOO", "NFLX")
download_articles <- function(symbol) {
  tm.plugin.webmining:::WebCorpus(YahooNewsSource(paste0("NASDAQ:", symbol)))
}
stock_articles <- tibble(company = company,
                         symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))
download_articles('AAPL')
symbol = 'AAPL'
(YahooNewsSource(paste0("NASDAQ:", symbol)))
  
download_article <- function(symbol) {
  WebCorpus(YahooNewsSource(paste0("NASDAQ:", symbol)))
}

## Exercise 1
library(tidytext)
library(tm)
library(ggplot2)
theme_set(theme_minimal())

data("AssociatedPress", package = "topicmodels")

ap_sentiments <- tidy(AssociatedPress) %>%
  inner_join(get_sentiments("afinn"), by = c(term = "word"))

ap_sentiments %>%
  group_by(term) %>% 
  summarise(n = sum(value)) %>% 
  filter(abs(n) >= 200) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = n)) +
  geom_col() +
  labs(x = "Contribution to sentiment", y = NULL) +
  scale_fill_viridis_c(option = 'E')


ap_sentiments %>%
  count(term, value > 0, wt = count) %>%
  filter(abs(n) >= 200) %>%
  mutate(n = ifelse(`value > 0`, n, -n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = n)) +
  geom_col() +
  labs(x = "Contribution to sentiment", y = NULL) +
  scale_fill_viridis_c(option = 'E')

## Exercise 2
# ap_lda <- topicmodels::LDA(AssociatedPress, k = 3, control = list(seed = 1618))

ap_top_terms <- ap_lda %>% 
  tidy(matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic),
         topic = paste('Topic', topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  rcartocolor::scale_fill_carto_d() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
