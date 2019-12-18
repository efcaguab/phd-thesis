raw_biblio <- readLines("data/biblio-text.txt")

library(magrittr)

token_count <- raw_biblio %>%
  tokenizers::tokenize_words() %>%
  unlist() %>%
  table() %>%
  as.data.frame()

token_count %>%
  dplyr::arrange(desc(Freq))


# jane memmot
# marcelo aizen
# ana traveset
# paulo guimaraes

