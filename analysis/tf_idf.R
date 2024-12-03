library(dplyr)
library(janeaustenr)
library(tidytext)
library(tidyverse)
library(here)
library(patchwork)
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

book_words |> 
  ggplot(
    aes(
      x = n/total,
      fill = book
    )
  )+
  geom_histogram()+
  facet_wrap(
    ~book,
    scales = "free_y"
  )+
  labs(
    fill = ""
  )+
  scale_fill_brewer(
    palette = "Dark2"
  )+
  scale_y_continuous(
    expand = c(0,0,0.05,0)
  )+
  scale_x_continuous(
    limits = c(NA, 0.0009),
    labels = scales::percent
  )+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )


freq_by_rank = book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

freq_by_rank %>% 
  ggplot(
    aes(
      rank, 
      `term frequency`, 
      color = book)
    ) + 
  geom_line(
    size = 1.1, 
    alpha = 0.8, 
    # show.legend = FALSE
    ) + 
  scale_x_log10() +
  scale_y_log10(
    labels = scales::percent
  )+
  scale_color_brewer(
    palette = "Dark2"
  )+
  labs(
    x = "Rang",
    y = "Termhäufigkeit",
    color = ""
  )+
  theme_bw(
    base_size = 15
  )+
  theme(
    legend.position = "bottom"
  )

ggsave(filename = here("img","zipf_austen.png"),width = 8, height = 6)


book_words <- book_words %>%
  bind_tf_idf(word, book, n)




plt_important <- book_words %>%
  arrange(
    desc(tf_idf)
    ) %>%
  mutate(
    word = 
      factor(
        word, 
        levels = 
          rev(
            unique(
              word
              )
            )
        )
    ) %>% 
  group_by(
    book
    ) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(
    aes(
      y = word, 
      x = tf_idf, 
      fill = book
      )
    ) +
  geom_col(
    show.legend = FALSE
    ) +
  labs(
    x = NULL, 
    y = NULL,
    title = "Wichtigkeit"
    ) +
  facet_wrap(~book, ncol = 3, scales = "free_y")+ 
  scale_fill_brewer(
    palette = "Dark2"
  )+
  scale_x_continuous(
    expand = c(0,0,0.05,0),
    labels = scales::percent
  )+
  theme_bw(12)

ggsave(filename = here("img","austen_ImportantWords.png"),width = 8, height = 6)


original_books <- austen_books() %>%
  group_by(book) |> 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()



tidy_books <- original_books %>%
  unnest_tokens(word, text)

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

by_book <- tidy_books |> 
  group_by(book,word) |> 
  summarise(
    n_words = n()
  ) |> 
  top_n(10)

plt_most10 <- by_book |> 
  ggplot(
    aes(
      x = n_words,
      y = reorder_within(word,n_words,book),
      fill = book
    )
  )+
  geom_col(
    show.legend = FALSE
  )+
  facet_wrap(
    ~book,
    ncol = 3,
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
    title = "häufigste Wörter",
    x ="",
    y = "",
    fill = ""
  )+
  theme_bw(12)+
  theme(
    legend.position = "bottom"
  )

plt_cmp <- plt_most10+plt_important

ggsave(plot = plt_cmp, filename = here("img","compare_Austen.png"),width = 15, height = 8)

