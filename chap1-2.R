# install.packages('janeaustenr')
library(janeaustenr)
library(dplyr)
library(tidytext)
library(ggplot2)

dat <- austen_books()

# Exercise 1
processed_dat <- dat %>%
  unnest_tokens(char, text, token = 'characters') %>%
  count(char) %>%
  mutate(freq = n / sum(n))

processed_dat %>%
  ggplot(aes(x = freq,
             y = forcats::fct_reorder(char, freq))) +
  geom_col() +
  labs(x = 'Frequency', y = NULL)

# Exercise 2

zipf_dat <- dat %>%
  unnest_tokens(word, text) %>%
  mutate(proc_word = stringr::str_replace(word, '_', '')) %>%
  count(proc_word, sort = TRUE) %>%
  mutate(rank = row_number(),
         freq = n / sum(n))

zipf_dat %>%
  ggplot(aes(rank, freq)) +
  geom_point() +
  theme_minimal() +
  scale_x_log10() +
  scale_y_log10()

# Exercise 3
data(sensesensibility)
get_sentiments("afinn")
library(stringr)
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(stringr::str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# sensesensibility
sensiment <- tidy_books %>%
  filter(book == 'Sense & Sensibility') %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(chapter) %>% 
  summarise(afinn_val = mean(value))

sensiment %>% 
  ggplot(aes(x = chapter, y = afinn_val)) +
  geom_point() +
  geom_line() +
  theme_minimal()
