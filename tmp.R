
# exercise 1

library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)

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
