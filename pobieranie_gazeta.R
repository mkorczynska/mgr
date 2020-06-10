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
url_gazeta_1 <-"https://wiadomosci.gazeta.pl/wiadomosci/0,143907.html?str="
url_gazeta_2 <- "_19845770"

pages_gazeta <- 8

get_list_gazeta <- function(page_num){
  url_gazeta <- paste0(url_gazeta_1, page_num, url_gazeta_2)
  page <- read_html(url_gazeta)
  links <- page %>%
    html_node("article")%>%
    html_node("section")%>%
    html_nodes("ul")%>%
    html_nodes("header")%>%
    html_nodes("h2")
  temp <- data_frame(link = links %>% html_node("a") %>% html_attr("href"))
  return(temp)
}

gazeta_links <- tibble()

for(i in 5:pages_gazeta) {
  gazeta_links_temp <- get_list_gazeta(i)
  gazeta_links <- bind_rows(gazeta_links, gazeta_links_temp)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
}

rm(gazeta_links_temp, i)

get_article <- function(article_url) {
  page <- read_html(article_url, encoding = "ISO_8859-2")
  date <- page %>% html_node("span.article_date") %>% html_text() %>% trimws() %>%
    str_replace_all("[\t\n ]", "") %>% dmy_hm()
  title <- page %>%html_node("h1#article_title") %>% html_text() %>% trimws()
  lead <- page %>% html_node("div#gazeta_article_lead") %>% html_text() %>% trimws()
  body <- page %>% html_node("section.art_content") %>% html_text() %>% trimws()
  article <- data_frame(title = title,
                        lead = lead,
                        body = body,
                        date = date,
                        url_gazeta = article_url)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
  return(article)
}

articles_gazeta <- gazeta_links %>%
  rowwise() %>%
  do(get_article(.$link)) %>%
  bind_rows() %>% 
  ungroup()

saveRDS(articles_gazeta, file = "articles_gazeta.RDS")

##############################UTWORZENIE KORPUSU##############################################
articles_gazeta <- readRDS("articles_gazeta.RDS")
datatable(articles_gazeta)

articles_gazeta<-articles_gazeta%>%
  filter(date>="2019-08-09")%>%
  filter(date<="2019-10-12")

articles_gazeta <- articles_gazeta %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         hour = hour(date))

articles_gazeta %>%
  count(year, month, day) %>%
  ggplot() +
  geom_col(aes(make_date(year, month, day), n), fill="lightblue", color = "gray50") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))

articles_gazeta<-articles_gazeta%>%
  mutate(body = gsub("REKLAMA.*;});}","", body))%>%
  mutate(body = gsub("Zobacz wideo", "", body))

articles_gazeta<-articles_gazeta%>%
  mutate(body = gsub("\n    if.*});\n    }\n", "", body))%>%
  mutate(body = gsub("\n    if.*});\n    }", "", body))

corpus_gazeta<-cbind(articles_gazeta$title, articles_gazeta$lead, articles_gazeta$body)
corpus_gazeta<-as.data.frame(corpus_gazeta)
colnames(corpus_gazeta)<-c("title", "lead", "body")
corpus_gazeta<-unite(corpus_gazeta, "text", c("title", "lead", "body"), sep=" ")

#nazwiska
corpus_gazeta<-corpus_gazeta%>%
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
corpus_gazeta<-corpus_gazeta%>%
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
  

corpus_gazeta<-tibble(corpus_gazeta)
corpus_gazeta<-unlist(corpus_gazeta)
corpus_gazeta<-VCorpus(VectorSource(corpus_gazeta))

save.corpus.to.files(corpus_gazeta, filename = "corpus_gazeta")

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

load(file="corpus_gazeta.rda")
bigcorp<-corpus_gazeta

stoplista<-stopwords("pl", source = "stopwords-iso")
stoplista<-as.data.frame(stoplista)

stem_dictionary <- read_csv2("polimorfologik-2.1.txt", col_names = c("stem", "word", "info"))
stem_dictionary<-add_row(stem_dictionary, stem="kidawabłońska", word="kidawabłońska")
stem_dictionary<-add_row(stem_dictionary, stem="kosiniakkamysz", word="kosiniakkamysz")
stem_dictionary<-add_row(stem_dictionary, stem="korwinmikke", word="korwinmikke")
stem_dictionary<-add_row(stem_dictionary, stem="liroymarzec", word="liroymarzec")

stem_dictionary<-add_row(stem_dictionary, stem="KO", word="KO")

bigcorp = tm_map(bigcorp, content_transformer(tolower))
bigcorp = tm_map(bigcorp, content_transformer(gsub), pattern = "proc.", replacement = "procent ")
bigcorp = tm_map(bigcorp, removeNumbers)
bigcorp = tm_map(bigcorp, removePunctuation)
bigcorp = tm_map(bigcorp, removeWords, stopwords("pl", source = "stopwords-iso"))
bigcorp = tm_map(bigcorp, stripWhitespace)

###tm_combine

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
corpus_gazeta<-VCorpus(VectorSource(data_tib_v))

corpus_gazeta = tm_map(corpus_gazeta, content_transformer(tolower))
corpus_gazeta = tm_map(corpus_gazeta, removeWords, stopwords("pl", source = "stopwords-iso"))
corpus_gazeta = tm_map(corpus_gazeta, stripWhitespace)

save.corpus.to.files(corpus_gazeta, filename = "corpus_gazeta_c")

#wczytanie korpusu po oczyszczeniu
load(file="corpus_gazeta_c.rda")
corpus_gazeta<-bigcorp

#macierz dokument-term
dtm = DocumentTermMatrix(corpus_gazeta)
inspect(dtm)
dtm = removeSparseTerms(dtm, 0.99)

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
