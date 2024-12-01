library(dplyr)
library(janeaustenr)
library(tidytext)

# count term frequency in each book
book_words = austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book, word, sort = TRUE)

# count number of terms in each book
total_words = book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
#join both
book_words = left_join(book_words, total_words)

ggplot(data = book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")
