
#-------------------------------------------------------------------------------
library(magrittr, include.only = '%>%')


art_wos_scopus <- read.csv2('art_wos_scoous.csv')


art_wos_scopus <- art_wos_scopus %>%
  dplyr::rename('referencia' = X,
                'resumo' = AB) %>%
  dplyr::mutate(novo = resumo)


art_wos_scopus$novo <- art_wos_scopus$novo %>%
  stringr::str_to_lower() %>%                            # Caixa baixa.
  stringr::str_replace_all(" *-+ *", "") %>%             # Remove hífen.
  stringr::str_replace_all("[[:punct:]]", " ") %>%       # Pontuação por espaço.
  tm::removeNumbers() %>%                                # Remove números.
  trimws() %>%                                           # Remove espaços nas bordas.
  tm::removeWords(words =
                    c(tm::stopwords(kind = 'en')))       # Remove stopwords.

sentimento <- tidytext::get_sentiments(lexicon = 'nrc')

base <- art_wos_scopus %>%
  tidytext::unnest_tokens(output = 'palavras', input = novo) %>%
  dplyr::left_join(sentimento,
                    by = c('palavras'='word'))






#-------------------------------------------------------------------------------
library(jsonlite)
library(tm)
library(tidytext)
library(tidyverse)
library(DT)
library(wordcloud)

# Dicionários léxicos em PT.
library(lexiconPT)
ls("package:lexiconPT")

url <- paste0("https://github.com/leg-ufpr/hackathon/blob/master",
              "/opinioes.json?raw=true")

txt <- fromJSON(url)
str(txt)

colnames(txt) <- c("id", "title", "model", "owner", "condition", "good",
                   "bad", "defect", "general", "ts")
tt <- as_tibble(txt)
glimpse(tt)

tt$product <- tt$model %>%
  str_extract("^([[:alpha:]]+ +[[:alpha:]]+)") %>%
  str_to_upper()

View(tt)

# Tipos únicos.
# tt$product %>% unique() %>% dput()
tt %>%
  count(product, sort = TRUE)








