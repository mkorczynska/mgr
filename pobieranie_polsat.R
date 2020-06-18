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
###-----------------------------------------------------------------------------------
url_polsat_1 <- "https://www.polsatnews.pl/raport/module35471/page"
url_polsat_2 <- "/wybory-parlamentarne-2019_1494891/"

pages_polsat <- 26

get_list_polsat <- function(page_num){
  url_polsat<-paste0(url_polsat_1, page_num, url_polsat_2)
  page <- read_html(url_polsat)
  links <- page %>%
    html_node("body")%>%
    html_node("div.news-list")%>%
    html_nodes("article")
  temp <- data_frame(link = links %>% html_node("a") %>% html_attr("href"))
  return(temp)
}

polsat_links <- tibble()

for(i in 9:pages_polsat) {
  polsat_links_temp <- get_list_polsat(i)
  polsat_links <- bind_rows(polsat_links, polsat_links_temp)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
}

rm(polsat_links_temp, i)

get_article <- function(article_url) {
  page <- read_html(article_url)
  date <- page %>% html_node("time") %>% html_text() %>% trimws()
  title <- page %>%html_node("h1") %>% html_text() %>% trimws()
  lead <- page %>% html_node("div.news__preview") %>% html_text() %>% trimws()
  body <- page %>% html_node("div.news__description") %>% html_text() %>% trimws()
  article <- data_frame(title = title,
                        lead = lead,
                        body = body,
                        date = date,
                        url = article_url)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
  return(article)
}

polsat_links<-polsat_links %>%
  drop_na()

articles_polsat <- polsat_links %>%
  rowwise() %>%
  do(get_article(.$link)) %>%
  bind_rows() %>% 
  ungroup()

saveRDS(articles_polsat, file = "articles_polsat.RDS")

##############################UTWORZENIE KORPUSU##############################################
articles_polsat <- readRDS("articles_polsat.RDS")
datatable(articles_polsat)

articles_polsat<-articles_polsat%>%
  mutate(date = gsub(",.*", "", date))%>%
  mutate(date = as.Date(date, "%d.%m.%Y"))

articles_polsat<-articles_polsat%>%
  filter(date>="2019-08-09")%>%
  filter(date<"2019-10-12")

articles_polsat<-articles_polsat%>%
  mutate(body = gsub("Twoja przeglądarka nie wspiera.*videoPlayer", "", body))%>%
  mutate(body = gsub("WIDEO.*'", "", body))

articles_polsat <- articles_polsat %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         hour = hour(date))

articles_polsat %>%
  count(year, month, day) %>%
  ggplot() +
  geom_col(aes(make_date(year, month, day), n), fill="lightblue", color = "gray50") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))

corpus_polsat<-cbind(articles_polsat$title, articles_polsat$lead, articles_polsat$body)
corpus_polsat<-as.data.frame(corpus_polsat)
colnames(corpus_polsat)<-c("title", "lead", "body")
corpus_polsat<-unite(corpus_polsat, "text", c("title", "lead", "body"), sep=" ")

#nazwiska
corpus_polsat<-corpus_polsat%>%
  mutate(text = gsub("Kidawa-Błońska", "kidawabłońska", text))%>%
  mutate(text = gsub("Kidawy-Błońskiej", "kidawabłońska", text))%>%
  mutate(text = gsub("Kidawie-Błońskiej", "kidawabłońska", text))%>%
  mutate(text = gsub("Kidawę-Błońską", "kidawabłońska", text))%>%
  mutate(text = gsub("Kidawą-Błońską", "kidawabłońska", text))%>%
  mutate(text = gsub("Kosiniak-Kamysz", "kosiniakkamysz", text))%>%
  mutate(text = gsub("Kosiniaka-Kamysza", "kosiniakkamysz", text))%>%
  mutate(text = gsub("Kosiniakowi-Kamyszowi", "kosiniakkamysz", text))%>%
  mutate(text = gsub("Kosiniakiem-Kamyszem", "kosiniakkamysz", text))%>%
  mutate(text = gsub("Kosiniaku-Kamyszu", "kosiniakkamysz", text))%>%
  mutate(text = gsub("Korwin-Mikke", "korwinmikke", text))%>%
  mutate(text = gsub("Korwin-Mikkego", "korwinmikke", text))%>%
  mutate(text = gsub("Korwin-Mikkemu", "korwinmikke", text))%>%
  mutate(text = gsub("Korwin-Mikkem", "korwinmikke", text))%>%
  mutate(text = gsub("Liroy-Marzec", "liroymarzec", text))%>%
  mutate(text = gsub("Liroya-Marca", "liroymarzec", text))%>%
  mutate(text = gsub("Liroyowi-Marcowi", "liroymarzec", text))%>%
  mutate(text = gsub("Liroyem-Marcem", "liroymarzec", text))%>%
  mutate(text = gsub("Liroyu-Marcu", "liroymarzec", text))

#partie
corpus_polsat<-corpus_polsat%>%
  mutate(text = gsub("Prawo i Sprawiedliwość", "pis", text))%>%
  mutate(text = gsub("Prawa i Sprawiedliwości", "pis", text))%>%
  mutate(text = gsub("Prawu i Sprawiedliwości", "pis", text))%>%
  mutate(text = gsub("Prawem i Sprawiedliwością", "pis", text))%>%
  mutate(text = gsub("Prawie i Sprawiedliwości", "pis", text))%>%
  mutate(text = gsub("Prawo i Sprawiedliwości", "pis", text))%>%
  mutate(text = gsub("Koalicja Obywatelska", "ko", text))%>%
  mutate(text = gsub("Koalicji Obywatelskiej", "ko", text))%>%
  mutate(text = gsub("Koalicję Obywatelską", "ko", text))%>%
  mutate(text = gsub("Koalicją Obywatelską", "ko", text))%>%
  mutate(text = gsub("Koalicjo Obywatelska", "ko", text))%>%
  mutate(text = gsub("Sojusz Lewicy Demokratycznej", "sld", text))%>%
  mutate(text = gsub("Sojuszu Lewicy Demokratyczne", "sld", text))%>%
  mutate(text = gsub("Sojuszowi Lewicy Demokratycznej", "sld", text))%>%
  mutate(text = gsub("Sojuszem Lewicy Demokratycznej", "sld", text))%>%
  mutate(text = gsub("Sojuszu Lewicy Demokratycznej", "sld", text))%>%
  mutate(text = gsub("Polskie Stronnictwo Ludowe", "psl", text))%>%
  mutate(text = gsub("Polskiego Stronnictwa Ludowego", "psl", text))%>%
  mutate(text = gsub("Polskiemu Stronnictwu Ludowemu", "psl", text))%>%
  mutate(text = gsub("Polskim Stronnictwem Ludowym", "psl", text))%>%
  mutate(text = gsub("Polskim Stronnictwie Ludowym", "psl", text))

corpus_polsat<-tibble(corpus_polsat)
corpus_polsat<-unlist(corpus_polsat)
corpus_polsat<-VCorpus(VectorSource(corpus_polsat))

save.corpus.to.files(corpus_polsat, filename = "corpus_polsat")

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

load(file="corpus_polsat.rda")
bigcorp<-corpus_polsat

stoplista<-stopwords("pl", source = "stopwords-iso")
stoplista<-as.data.frame(stoplista)

stem_dictionary <- read_csv2("polimorfologik-2.1.txt", col_names = c("stem", "word", "info"))
stem_dictionary<-add_row(stem_dictionary, stem="kidawabłońska", word="kidawabłońska")
stem_dictionary<-add_row(stem_dictionary, stem="kosiniakkamysz", word="kosiniakkamysz")
stem_dictionary<-add_row(stem_dictionary, stem="korwinmikke", word="korwinmikke")
stem_dictionary<-add_row(stem_dictionary, stem="liroymarzec", word="liroymarzec")
stem_dictionary<-add_row(stem_dictionary, stem="ko", word="ko")

bigcorp = tm_map(bigcorp, content_transformer(tolower))
bigcorp = tm_map(bigcorp, content_transformer(gsub), pattern = "proc.", replacement = "procent ")
bigcorp = tm_map(bigcorp, removeNumbers)
bigcorp = tm_map(bigcorp, removePunctuation)
bigcorp = tm_map(bigcorp, removeWords, stopwords("pl", source = "stopwords-iso"))
bigcorp = tm_map(bigcorp, stripWhitespace)

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
                     total = nrow(corp_data)+1)
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

data_tib_v<-unlist(articles)
corpus_polsat<-VCorpus(VectorSource(data_tib_v))

corpus_polsat = tm_map(corpus_polsat, content_transformer(tolower))
corpus_polsat = tm_map(corpus_polsat, removeWords, stopwords("pl", source = "stopwords-iso"))
corpus_polsat = tm_map(corpus_polsat, stripWhitespace)

save.corpus.to.files(corpus_polsat, filename = "corpus_polsat_c_s")
#-----------------------------------------------------------------
#wczytanie korpusu po oczyszczeniu
load(file="corpus_polsat_c_s.rda")
corpus_polsat<-bigcorp

#macierz dokument-term
dtm = DocumentTermMatrix(corpus_polsat)
inspect(dtm)
dtm = removeSparseTerms(dtm, 0.9)

freq <- colSums(as.matrix(dtm))
ord <- order(freq)   

#czestosc slow
freq <- colSums(as.matrix(dtm))
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)

#ramka ze slowami i ich frekwencja
word_freq <- data.frame(freq=freq)
datatable(word_freq)
########################################LDA#############################################
#wybor liczby tematow w lda
results_1 <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014"),
  method = "Gibbs",
  mc.cores = 4L,
  verbose = TRUE
)

results_2 <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009"),
  method = "Gibbs",
  mc.cores = 4L,
  verbose = TRUE
)

#wykres pozwalajacy wybrac liczbe tematow
FindTopicsNumber_plot(results_1)
FindTopicsNumber_plot(results_2)

datatable(results_100)

#lda
lda_20_2 <- LDA(dtm, k = 15)
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