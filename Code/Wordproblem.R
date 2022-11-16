library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)

words <- read_fwf("words.txt")
View(words)

m_words <- words %>%
  dplyr::filter(str_detect(X1, "^[Mm]"))


m8words <- m_words %>%
  filter(str_length(X1) == 8)

m8wordssplit <- m8words %>%
  separate(X1, c("first", "second"), sep= 4, remove = FALSE)

move_words <- m8wordssplit %>%
  separate(col = second, c("first3let", "lastlet"), sep = 3) %>%
  mutate(new_word = paste0(lastlet, first3let))
View(move_words)

words_to_check <- c("ling", "scat", "soil")
words_to_check %in% pull(words, "X1")

is_word <- function(words_to_check, real_word_list = words){
  words_to_check %in% pull(words, "X1")
}

is_word(words_to_check = c("ling", "scat", "soil"))

final <- move_words %>%
  filter(is_word(first) & is_word(new_word))%>%
  unite("combo", c("first", "new_word"), sep = " ") %>%
  select(combo)

head(final)

