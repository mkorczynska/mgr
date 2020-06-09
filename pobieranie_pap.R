###POBIERANIE ARTYKULOW---------------------------------------------------------------
###BIBLIOTEKI-------------------------------------------------------------------------
install.packages("rvest")
install.packages("tidyverse")
install.packages("stringr")
install.packages("lubridate")
install.packages("tibble")
install.packages("tm")
install.packages("textreg")
install.packages("stopwords")
install.packages("readr")
install.packages("tidytext")
install.packages("knitr")
install.packages("progress")
install.packages("DT")
install.packages("ldatuning")
install.packages("topicmodels")
install.packages("tidyr")
install.packages("progress")

library(rvest)
library(tidyverse)
library(stringr)
library(lubridate)
library(tibble)
library(tm)
library(textreg)
library(stopwords)
library(readr)
library(tidytext)
library(knitr)
library(progress)
library(DT)
library(ldatuning)
library(topicmodels)
library(tidyr)
library(progress)

path<-getwd()
setwd(path)
####################################POBRANIE ARTYKULOW######################################
url_pap_1 <-"https://www.pap.pl/aktualnosci/index%2C1%2C%2Cwybory-parlamentarne.html?page="

pages_pap <- 24

get_list_pap <- function(page_num){
  url_pap <- paste0(url_pap_1, page_num)
  page <- read_html(url_pap)
  links <- page %>%
    html_node("body")%>%
    html_nodes("div.m8")%>%
    html_nodes("div.textWrapper")%>%
    html_nodes("h3")
  temp <- data_frame(link = links %>% html_node("a") %>% html_attr("href"))
  return(temp)
}

pap_links <- tibble()

for(i in 8:pages_pap) {
  pap_links_temp <- get_list_pap(i)
  pap_links <- bind_rows(pap_links, pap_links_temp)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
}
rm(pap_links_temp, i)

get_article_pap <- function(article_url) {
  page <- read_html(article_url)
  date <- page %>% html_node("div.moreInfo") %>% html_text() %>% trimws()
  title <- page %>%html_node("h1") %>% html_text() %>% trimws()
  lead <- page %>% html_node("div.field--name-field-lead") %>% html_text() %>% trimws()
  body <- page %>% html_node("div.field--name-body") %>% html_text() %>% trimws()
  article <- data_frame(title = title,
                        lead = lead,
                        body = body,
                        date = date,
                        url = article_url)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
  return(article)
}

pap_links<-pap_links %>%
  drop_na()

pap_links<-pap_links %>%
  select(link) %>%
  mutate(full_link = paste0("https://www.pap.pl", link))

articles_pap <- pap_links %>%
  rowwise() %>%
  do(get_article_pap(.$full_link)) %>%
  bind_rows() %>% 
  ungroup()

saveRDS(articles_pap, file = "articles_pap.RDS")

##############################UTWORZENIE KORPUSU##############################################
articles_pap <- readRDS("articles_pap.RDS")
datatable(articles_pap)

articles_pap<-articles_pap%>%
  mutate(date = sub(" aktualizacja:.*", "", date))%>%
  filter(date>="2019-08-09")%>%
  filter(date<="2019-10-12")

articles_pap <- articles_pap %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         hour = hour(date))

articles_pap %>%
  count(year, month, day) %>%
  ggplot() +
  geom_col(aes(make_date(year, month, day), n), fill="lightblue", color = "gray50") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))

corpus_pap<-cbind(articles_pap$title, articles_pap$lead, articles_pap$body)
corpus_pap<-as.data.frame(corpus_pap)
colnames(corpus_pap)<-c("title", "lead", "body")
corpus_pap<-unite(corpus_pap, "text", c("title", "lead", "body"), sep=" ")

corpus_pap<-tibble(corpus_pap)
corpus_pap<-unlist(corpus_pap)
corpus_pap<-VCorpus(VectorSource(corpus_pap))

save.corpus.to.files(corpus_pap, filename = "corpus_pap")

#######################OCZYSZCZANIE KORPUSU###########################################
#----------------------FUNKCJE-----------------------------------------------#
#funkcja do usuwania konkretnych slow, wyrazen
delete_pattern<-content_transformer(function(x, pattern){
  gsub(pattern, "", x)
})

##funkcja do zastepowania slow ich formami podstawowymi
stem_word <- function(word_to_stem) {
  stem_dictionary %>% filter(word == word_to_stem) %>% .$stem %>% .[1]
}
#----------------------------------------------------------------------------#
load(file="corpus_pap.rda")
stoplista<-stopwords("pl", source = "stopwords-iso")
stoplista<-as.data.frame(stoplista)

stem_dictionary <- read_csv2("polimorfologik-2.1.txt", col_names = c("stem", "word", "info"))
stem_dictionary<-add_row(stem_dictionary, stem="Kidawa-Błońska", word="KidawaBłońska")
stem_dictionary<-add_row(stem_dictionary, stem="Kidawa-Błońska", word="KidawyBłońskiej")
stem_dictionary<-add_row(stem_dictionary, stem="Kidawa-Błońska", word="KidawieBłońskiej")
stem_dictionary<-add_row(stem_dictionary, stem="Kidawa-Błońska", word="KidawęBłońską")
stem_dictionary<-add_row(stem_dictionary, stem="Kidawa-Błońska", word="KidawąBłońską")

stem_dictionary<-add_row(stem_dictionary, stem="Kosiniak-Kamysz", word="KosiniakKamysz")
stem_dictionary<-add_row(stem_dictionary, stem="Kosiniak-Kamysz", word="KosiniakaKamysza")
stem_dictionary<-add_row(stem_dictionary, stem="Kosiniak-Kamysz", word="KosiniakowiKamyszowi")
stem_dictionary<-add_row(stem_dictionary, stem="Kosiniak-Kamysz", word="KosiniakiemKamyszem")
stem_dictionary<-add_row(stem_dictionary, stem="Kosiniak-Kamysz", word="KosiniakuKamyszu")

stem_dictionary<-add_row(stem_dictionary, stem="Korwin-Mikke", word="KorwinMikke")
stem_dictionary<-add_row(stem_dictionary, stem="Korwin-Mikke", word="KorwinMikkego")
stem_dictionary<-add_row(stem_dictionary, stem="Korwin-Mikke", word="KorwinMikkemu")
stem_dictionary<-add_row(stem_dictionary, stem="Korwin-Mikke", word="KorwinMikkem")

stem_dictionary<-add_row(stem_dictionary, stem="Liroy-Marzec", word="LiroyMarzec")
stem_dictionary<-add_row(stem_dictionary, stem="Liroy-Marzec", word="LiroyaMarca")
stem_dictionary<-add_row(stem_dictionary, stem="Liroy-Marzec", word="LiroyowiMarcowi")
stem_dictionary<-add_row(stem_dictionary, stem="Liroy-Marzec", word="LiroyemMarcem")
stem_dictionary<-add_row(stem_dictionary, stem="Liroy-Marzec", word="LiroyuMarcu")

stem_dictionary<-add_row(stem_dictionary, stem="KO", word="KO")
stem_dictionary<-add_row(stem_dictionary, stem="PSL", word="PSL")
stem_dictionary<-add_row(stem_dictionary, stem="PiS", word="PiS")


bigcorp = tm_map(bigcorp, content_transformer(tolower))
bigcorp = tm_map(bigcorp, removeNumbers)
bigcorp = tm_map(bigcorp, removePunctuation, preserve_intra_word_dashes=TRUE)
bigcorp = tm_map(bigcorp, removeWords, stopwords("pl", source = "stopwords-iso"))
bigcorp = tm_map(bigcorp, stripWhitespace)
bigcorp = tm_map(bigcorp, delete_pattern, "pap autor.*")

#zamiana liter w slowniku na male
stem_dictionary <- stem_dictionary %>%
  mutate(stem = str_to_lower(stem),
         word = str_to_lower(word)) %>%
  distinct()

#przeksztalcenie korpusu do podstaci ramki danych
corp_data<-data.frame(text = sapply(bigcorp, as.character), stringsAsFactors = FALSE)
datatable(corp_data)

#przeksztalcanie wyrazow na formy podstawowe
articles<-data.frame()
pb<-progress_bar$new(format = "[:bar] :percent eta: :eta elap. :elapsedfull",
                     total = nrow(corp_data))
for(i in 1:nrow(corp_data)){
  pb$tick()
  words_in_article<-tibble(corp_data[i,1])
  words<-words_in_article%>%
    unnest_tokens(word, corp_data[i,1])
  
  for(j in 1:nrow(words)){
    words[j, 1]<-stem_word(as.character(words[j,1]))
  }
  
  w<-words[1,1]
  for(k in 2:nrow(words)){
    w<-combine_words(c(w, words[k, 1]), and=" ")
  }
  
  articles[i,1]<-w
  Sys.sleep(1 / nrow(corp_data))
}

#przeksztalcenie do postaci korpusu
data_tib_v<-unlist(articles)
corp<-VCorpus(VectorSource(data_tib_v))

dtm = DocumentTermMatrix(corp)
inspect(dtm)
dtm = removeSparseTerms(dtm, 0.9)

freq <- colSums(as.matrix(dtm))
ord <- order(freq)   

#czestosc slow
freq <- colSums(as.matrix(dtm))
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)

#ramka ze slowami i ich frekwencja
word_freq <- data.frame(freq=freq)

########################################LDA#############################################
#wybor liczby tematow w lda
results_100 <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Griffiths2004"),
  method = "Gibbs",
  mc.cores = 4L,
  verbose = TRUE
)

#wykres pozwalajacy wybrac liczbe tematow
FindTopicsNumber_plot(results_100)

datatable(results_100)

#lda
lda_20_2 <- LDA(dtm, k = 6)
#zapisanie wspolczynnikow beta dla kazdego slowa i tematu
topics_20 <- tidy(lda_20_2, matrix = "beta")

#wykres slow dla poszczegolnych tematow
topics_20 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(beta) %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  ggplot() +
  geom_col(aes(term, beta, fill = factor(topic)), color = "gray50", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free_y") +
  coord_flip()

#topowe slowa w kazdym z tematow
ap_top_terms <- topics_20 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#wykres topowych slow ze wspolczynnikami
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#beta spread
beta_spread <- topics_20 %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#wykres dla beta spread
beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(15, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

########################################ANLIZA SENTYMENTU###############################
articles<-data.frame(text = sapply(bigcorp, as.character), stringsAsFactors = FALSE)

lead_words <- articles_pap %>%
  unnest_tokens(word, lead, token = "words")

pl_words_sentiment <- read_csv("pl_words.csv")
#pl_words_sentiment <- pl_words_sentiment[, 2:8]

text_words_sentiment <- inner_join(lead_words %>%
                                     select(word, year, month, day),
                                   pl_words_sentiment,
                                   by = c("word" = "word"))

text_words_sentiment %>%
  count(year, month, day, category) %>%
  ungroup() %>%
  group_by(year, month, day) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(!category %in% c("N", "U")) %>%
  mutate(category = case_when(.$category == "A" ~ "Anger",
                              .$category == "H" ~ "Happiness",
                              .$category == "S" ~ "Sadness",
                              .$category == "D" ~ "Disgust",
                              .$category == "F" ~ "Fear")) %>%
  ggplot() +
  geom_col(aes(make_date(year, month, day), p, fill=category), show.legend = FALSE) +
  facet_wrap(~category, ncol=1)

text_words_sentiment %>%
  select(-word, -category) %>%
  gather(key = sent_category, value = score,
         mean.Happiness, mean.Anger, mean.Sadness, mean.Fear, mean.Disgust) %>%
  ggplot() +
  geom_smooth(aes(make_date(year, month, day), score, color = sent_category),
              show.legend = FALSE) +
  facet_wrap(~sent_category, scale = "free_y", ncol=1)

# lista artykułów z wybranymi słowami w lidzie
art_list <- lead_words %>%
  filter(word %in% c("pis", "platforma", "nowoczesna",
                     "kaczyński", "tusk", "duda", "szydło")) %>%
  select(url, word) %>%
  distinct()

# sentyment dla słow z treści
# poprzenio nie było kolumny url - stąd jeszcze raz łączymy słowa ze słownikiem sentymentu
text_words_sentiment <- inner_join(lead_words %>%
                                     select(word, year, month, day, url),
                                   pl_words_sentiment,
                                   by = c("word" = "word")) %>%
  # łączymy dane o sentymencie z listą artykułów i słowami z leadu
  left_join(art_list, by = c("url" = "url")) %>%
  filter(!is.na(word)) %>%
  # pivot tabelki
  gather(key = sent_category, value = score,
         mean.Happiness, mean.Anger, mean.Sadness, mean.Fear, mean.Disgust)