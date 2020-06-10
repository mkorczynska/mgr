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
url_interia_1 <-"https://fakty.interia.pl/raporty/raport-wybory-parlamentarne-2019/aktualnosci,nPack,"

pages_interia <- 84

get_list_interia <- function(page_num){
  url_interia<-paste0(url_interia_1, page_num)
  page <- read_html(url_interia)
  links <- page %>%
    html_node("body")%>%
    html_nodes("li.brief-list-item")%>%
    html_node("h2")
  temp <- data_frame(link = links %>% html_node("a") %>% html_attr("href"))
  return(temp)
}

interia_links <- tibble()

for(i in 28:pages_interia) {
  interia_links_temp <- get_list_interia(i)
  interia_links <- bind_rows(interia_links, interia_links_temp)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
}

rm(interia_links_temp, i)
get_article <- function(article_url) {
  page <- read_html(article_url)
  date <- page %>% html_node("a.article-date") %>% html_text() %>% trimws()
  title <- page %>%html_node("h1") %>% html_text() %>% trimws()
  lead <- page %>% html_node("p.article-lead") %>% html_text() %>% trimws()
  body <- page %>% html_node("div.article-body") %>% html_text() %>% trimws()
  article <- data_frame(title = title,
                        lead = lead,
                        body = body,
                        date = date,
                        url = article_url)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
  return(article)
}

interia_links<-interia_links %>%
  drop_na()

interia_links<-interia_links %>%
  select(link) %>%
  mutate(full_link = paste0("https://fakty.interia.pl", link))

articles_interia <- interia_links %>%
  rowwise() %>%
  do(get_article(.$full_link)) %>%
  bind_rows() %>% 
  ungroup()

articles_interia<-articles_interia %>%
  drop_na()

saveRDS(articles_interia, file = "articles_interia.RDS")

##############################UTWORZENIE KORPUSU##############################################
articles_interia <- readRDS("articles_interia.RDS")
datatable(articles_interia)

articles_interia<-articles_interia%>%
  mutate(date = gsub(" października ", "-10-", date))%>%
  mutate(date = gsub(" września ", "-09-", date))%>%
  mutate(date = gsub(" sierpnia ", "-08-", date))%>%
  mutate(date = gsub(".*,", "", date))%>%
  mutate(date = gsub("2019.*", "2019", date))%>%
  mutate(date = as.Date(date, "%d-%m-%Y"))

articles_interia<-articles_interia%>%
  filter(date>="2019-08-09")%>%
  filter(date<"2019-10-12")

articles_interia<-articles_interia%>%
  mutate(body = gsub("var Criteo.*Reklama", "", body))%>%
  mutate(body = gsub("CDATA.*block", "", body ))%>%
  mutate(body = gsub("Zdjęcie.*Reporter", "", body))%>%
  mutate(body = gsub("Zdjęcie.*East News", "", body))%>%
  mutate(body = gsub("Zdjęcie.*PAP", "", body))%>%
  mutate(body = gsub("Wideo.*srcsign", "", body))

articles_interia <- articles_interia %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         hour = hour(date))

articles_interia %>%
  count(year, month, day) %>%
  ggplot() +
  geom_col(aes(make_date(year, month, day), n), fill="lightblue", color = "gray50") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))

corpus_interia<-cbind(articles_interia$title, articles_interia$lead, articles_interia$body)
corpus_interia<-as.data.frame(corpus_interia)
colnames(corpus_interia)<-c("title", "lead", "body")
corpus_interia<-unite(corpus_interia, "text", c("title", "lead", "body"), sep=" ")

corpus_interia<-tibble(corpus_interia)
corpus_interia<-unlist(corpus_interia)
corpus_interia<-VCorpus(VectorSource(corpus_interia))

save.corpus.to.files(corpus_interia, filename = "corpus_interia")

#######################OCZYSZCZANIE KORPUSU###########################################
#funkcja do usuwania konkretnych slow, wyrazen
delete_pattern<-content_transformer(function(x, pattern){
  gsub(pattern, "", x)
})

##funkcja do zastepowania slow ich formami podstawowymi
stem_word <- function(word_to_stem) {
  stem_dictionary %>% filter(word == word_to_stem) %>% .$stem %>% .[1]
}
#----------------------------------------------------------------------------#

load(file="corpus_interia.rda")

stoplista<-stopwords("pl", source = "stopwords-iso")
stoplista<-as.data.frame(stoplista)
stem_dictionary <- read_csv2("polimorfologik-2.1.txt", col_names = c("stem", "word", "info"))

bigcorp = tm_map(bigcorp, content_transformer(tolower))
bigcorp = tm_map(bigcorp, removeNumbers)
bigcorp = tm_map(bigcorp, removePunctuation, preserve_intra_word_dashes=TRUE)
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

install.packages("progress")
library(progress)

#przeksztalcanie wyrazow na formy podstawowe
articles<-data.frame()
pb<-progress_bar$new(total = 100)
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
