
# exercise 1

library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)

austen_books() |> head(n=100) |> datatable()

original_books <- austen_books() %>%
  group_by(book) |> 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books


tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)


tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(
    aes(
      y = word, 
      x = n)
    ) +
  geom_col(
    fill = "steelblue"
  )+
  scale_x_continuous(
    expand = c(0,0,0.05,0)
  )+
  labs(
    title = " Die meist-benutzten Wörter",
    x = "Häufigkeit der Wörter",
    y = ""
  )+
  theme_minimal(base_size = 15)


by_book <- tidy_books |> 
  group_by(book,word) |> 
  summarise(
    n_words = n()
  ) |> 
  top_n(10)

by_book |> 
  ggplot(
    aes(
      x = n_words,
      y = reorder_within(word,n_words,book),
      fill = book
    )
  )+
  geom_col()+
  facet_wrap(
    ~book,
    scales = "free_y"
  )+
  scale_y_reordered()+
  scale_x_continuous(
    expand = c(0,0,0.05,0)
    )+
  scale_fill_brewer(
    palette = "Dark2"
  )+
  labs(
    x ="",
    y = "",
    fill = ""
  )+
  theme_bw()+
  theme(
    legend.position = "bottom"
    )
