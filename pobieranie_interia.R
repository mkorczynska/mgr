######################################################################################
#--BIBLIOTEKI-------------------------------------------------------------------------
######################################################################################
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
install.packages("corpus")
install.packages("textclean")

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
library(corpus)
library(textclean)

path<-getwd()
setwd(path)

######################################################################################
#--ZAPISYWANIE TEKSTOW----------------------------------------------------------------
######################################################################################
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

######################################################################################
#--UTWORZENIE KORPUSU-----------------------------------------------------------------
######################################################################################
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

articles_interia <- articles_interia %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         hour = hour(date))

articles_interia<-articles_interia%>%
  mutate(body = gsub("var Criteo.*Reklama", "", body))%>%
  mutate(body = gsub("CDATA.*block", "", body ))%>%
  mutate(body = gsub("Zdjęcie.*Reporter", "", body))%>%
  mutate(body = gsub("Zdjęcie.*East News", "", body))%>%
  mutate(body = gsub("Zdjęcie.*PAP", "", body))%>%
  mutate(body = gsub("Wideo.*srcsign", "", body))

articles_interia %>%
  count(year, month, day) %>%
  ggplot(aes(make_date(year, month, day), n)) +
  geom_bar(stat="identity") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)

corpus_interia<-cbind(articles_interia$title, articles_interia$lead, articles_interia$body)
corpus_interia<-as.data.frame(corpus_interia)
colnames(corpus_interia)<-c("title", "lead", "body")
corpus_interia<-unite(corpus_interia, "text", c("title", "lead", "body"), sep=" ")

datatable(corpus_interia)

#wczytanie listy zarejestrowanych komitetow
komitety<-read.csv2("komitety_sejm_senat.csv", header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
komitety_interia<-komitety

for(i in 1:nrow(komitety_interia)){
  if(grepl(komitety_interia[i, 1], corpus_interia)==TRUE){
    komitety_interia[i, 3]=TRUE
  }else{
    komitety_interia[i, 3]=FALSE
  }
}

summary(komitety_interia)
komitety_interia_pelne<-komitety_interia%>%slice_head(n=86)
kipf<-komitety_interia_pelne%>%filter(V3=="TRUE")

komitety_interia_skroty<-komitety_interia%>%slice_tail(n=86)
kisf<-komitety_interia_skroty%>%filter(V3=="TRUE")

komitety_interia_ogol<-inner_join(kipf, kisf, by="Skrót")

#zastapienie nazw komitetow i odmienionych nazw partii akronimami
corpus_interia <- mgsub(corpus_interia, komitety$X.U.FEFF.Nazwa, komitety$Skrót, safe = TRUE)
corpus_interia<-corpus_interia%>%
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
  mutate(text = gsub("Polskim Stronnictwie Ludowym", "psl", text))%>%
  mutate(text = gsub("Konfederacja", "konf", text))%>%
  mutate(text = gsub("Konfederacji", "konf", text))%>%
  mutate(text = gsub("Konfederację", "konf", text))%>%
  mutate(text = gsub("Konfederacją", "konf", text))%>%
  mutate(text = gsub("Konfederacjo", "konf", text))

datatable(corpus_interia)

corpus_interia<-tibble(corpus_interia)
corpus_interia<-unlist(corpus_interia)
corpus_interia<-VCorpus(VectorSource(corpus_interia))

save.corpus.to.files(corpus_interia, filename = "new_corpus_interia")

######################################################################################
#--OCZYSZCZANIE KORPUSU---------------------------------------------------------------
######################################################################################

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

load(file="new_corpus_interia.rda")

stoplista<-stopwords("pl", source = "stopwords-iso")
stoplista<-as.data.frame(stoplista)

stem_dictionary <- read_csv2("polimorfologik-2.1.txt", col_names = c("stem", "word", "info"))

stem<-as.data.frame(komitety$Skrót)
word<-as.data.frame(komitety$Skrót)
info<-matrix(nrow=172, ncol=1)
info<-as.data.frame(info)

skroty<-cbind(stem, word, info)
colnames(skroty)<-c("stem", "word", "info")

stem_dictionary<-rbind(stem_dictionary, skroty)

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
corpus_interia<-VCorpus(VectorSource(data_tib_v))

corpus_interia = tm_map(corpus_interia, content_transformer(tolower))
corpus_interia = tm_map(corpus_interia, removeWords, stopwords("pl", source = "stopwords-iso"))
corpus_interia = tm_map(corpus_interia, stripWhitespace)

save.corpus.to.files(corpus_interia, filename = "new_corpus_interia_c_s")

######################################################################################
#--DTM, LISTA FREKWENCYJNA------------------------------------------------------------
######################################################################################
#wczytanie korpusu po oczyszczeniu
load(file="new_corpus_interia_c_s.rda")
corpus_interia<-bigcorp

#macierz dokument-term
dtm_interia = DocumentTermMatrix(corpus_interia, control=list(wordLengths=c(1,Inf)))
inspect(dtm_interia)

#czestosc slow
freq <- colSums(as.matrix(dtm_interia))
freq <- sort(colSums(as.matrix(dtm_interia)), decreasing=TRUE)

#ramka ze slowami i ich frekwencja
word_freq <- data.frame(word=names(freq), freq=freq)
datatable(word_freq)

#wykres frekwencji
top_n(word_freq, n=10, freq) %>%
  ggplot(., aes(x=reorder(word, -freq), y=freq))+
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)

#redukcja
dtm_interia = removeSparseTerms(dtm_interia, 0.99)
inspect(dtm_interia)

######################################################################################
#--WYSTEPOWANIE NAZW PARTII-----------------------------------------------------------
######################################################################################
#korpus jako ramka danych
corpus_interia_df<-data.frame(text = sapply(corpus_interia, as.character), stringsAsFactors = FALSE)

date_cols<-articles_interia%>%
  dplyr::select(year, month, day, url)

corpus_interia_df<-cbind(corpus_interia_df, date_cols)

body_words <- corpus_interia_df %>%
  unnest_tokens(word_s, text, token = "words")

articles_per_day <- articles_interia %>%
  count(year, month, day) %>%
  ungroup() %>%
  rename(n_arts = n)

body_words %>%
  filter(word_s %in% c("pis", "ko", "lewica", "psl")) %>%
  count(year, month, day, word_s) %>%
  ungroup() %>%
  rename(n_words = n) %>%
  left_join(articles_per_day, by = c("year" = "year", "month" = "month", "day"="day")) %>%
  # przeskalowanie danych o liczbie słów
  mutate(n_words_plot = n_words) %>%
  mutate(date = make_date(year, month, day)) %>%
  ggplot() +
  # bar = liczba tesktów
  geom_bar(data = articles_per_day, aes(make_date(year, month, day), n_arts),
           stat="identity",
           fill = "gray80") +
  # line = liczba słów
  geom_point(aes(date, n_words_plot, color = word_s), size = 2) +
  theme(legend.position = "bottom")

######################################################################################
#--LDA--------------------------------------------------------------------------------
######################################################################################
#wybor liczby tematow w lda
results_1_interia <- FindTopicsNumber(
  dtm_interia,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "CaoJuan2009"),
  method = "VEM",
  mc.cores = 4L,
  verbose = TRUE
)

results_2_interia <- FindTopicsNumber(
  dtm_interia,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "Griffiths2004", "CaoJuan2009"),
  method = "Gibbs",
  mc.cores = 4L,
  verbose = TRUE
)

#wykres pozwalajacy wybrac liczbe tematow
FindTopicsNumber_plot(results_1_interia)
FindTopicsNumber_plot(results_2_interia)

#lda
lda_interia <- LDA(dtm_interia, k=10, method = "VEM", control=list(seed=1234))

#zapisanie beta i gamma
topics_words_interia <- tidy(lda_interia, matrix = "beta")
topics_docs_interia <- tidy(lda_interia, matrix = "gamma")

#klasyfikacja kazdego dokumentu
doc_classes_interia <- topics_docs_interia %>%
  group_by(document) %>%
  top_n(1) %>%
  ungroup()

#liczba dokumentow w temacie
doc_classes_interia%>% count(topic)

#wykres slow dla poszczegolnych tematow (ogolne skale)
topics_words_interia %>%
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
ap_top_terms_interia <- topics_words_interia %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#wykres topowych slow ze wspolczynnikami (kazdy ma skale beta)
ap_top_terms_interia %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

assignments_interia<-augment(lda_interia, dtm_interia)

######################################################################################
#--ANALIZA SENTYMENTU-----------------------------------------------------------------
######################################################################################

pl_words_sentiment <- read_csv("pl_words.csv")
pl_words_sentiment <- read_csv("nawl-analysis.csv")

as_tidy_interia <- tidy(dtm_interia)

text_words_sentiment_interia <- inner_join(as_tidy_interia %>%
                                            dplyr::select(document, term),
                                          pl_words_sentiment,
                                          by = c("term" = "word"))

emotions_interia<-text_words_sentiment_interia %>%
  count(document, category) %>%
  ungroup() %>%
  group_by(document) %>%
  ungroup() %>%
  filter(!category %in% c("U", "N")) %>%
  mutate(category = case_when(.$category == "A" ~ "Złość",
                              .$category == "H" ~ "Szczęście",
                              .$category == "S" ~ "Smutek",
                              .$category == "D" ~ "Wstręt",
                              .$category == "F" ~ "Strach"))


all_emotions_interia<-emotions_interia%>%
  group_by(category)%>%
  summarise(sum=sum(n))

all_emotions_interia$zrodlo<-rep("Interia", 5)


nr_interia<-as.data.frame(seq(1:nrow(articles_interia)))
colnames(nr_interia)<-c("nr")
new_articles_interia<-cbind(nr_interia, articles_interia)
new_articles_interia$nr<-as.character(new_articles_interia$nr)

articles_emotions_interia <- inner_join(new_articles_interia %>%
                                         dplyr::select(nr, year, month, day),
                                       emotions_interia,
                                       by = c("nr" = "document"))

articles_emotions_interia<-articles_emotions_interia%>%
  mutate(date=make_date(year, month, day))

colnames(articles_emotions_interia)<-c("Dokument", "Rok", "Miesiac", "Dzien", "Emocje", "Liczba", "Data" )
grouped_emotions_interia<-articles_emotions_interia%>%
  group_by(Emocje, Data)%>%
  summarise(Liczba = sum(Liczba))

ggplot(grouped_emotions_interia, aes(fill=Emocje, y=Liczba, x=Data)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#3d538f", "#BAA898", "#848586", "#C2847A", "#0d0f06"))+
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  #geom_text(data=subset(grouped_emotions_interia, Emocje!=""), aes(label = Liczba), position = position_stack(vjust = 0.5), colour = "white")+
  theme(legend.position = "bottom")


only_parties<-as_tidy_interia%>%
  filter(term %in% c("pis", "ko", "sld", "psl", "konf"))

articles_parties_interia<-inner_join(only_parties%>%select(document, term),
                                    emotions_interia,
                                    by = c("document" = "document"))

emotions_parties_interia<-articles_parties_interia%>%
  group_by(term, category)%>%
  summarise(Liczba = sum(n))

ggplot(emotions_parties_interia, aes(fill=category, y=Liczba, x=term)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#3d538f", "#BAA898", "#848586", "#C2847A", "#0d0f06"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(data=subset(emotions_parties_interia, category!=""), aes(label = Liczba), position = position_stack(vjust = 0.5), colour = "white")+
  theme(legend.position = "bottom")

par(mfrow=c(2,3))

#---
ko_interia<-emotions_parties_interia%>%filter(term=="ko")

ko_interia<-ko_interia%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_ko_interia <- as.data.frame(t(matrix(ko_interia$procent)))
colnames(radar_ko_interia) <- ko_interia$category

radar_ko_interia <- rbind(rep(100, 5) , rep(0, 5) , radar_ko_interia)

radarchart(radar_ko_interia, axistype=1 , 
           
           #custom polygon
           pcol="#0a122a" , pfcol="#0a122aCC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="ko",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
pis_interia<-emotions_parties_interia%>%filter(term=="pis")

pis_interia<-pis_interia%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_pis_interia <- as.data.frame(t(matrix(pis_interia$procent)))
colnames(radar_pis_interia) <- pis_interia$category

radar_pis_interia <- rbind(rep(100, 5) , rep(0, 5) , radar_pis_interia)

radarchart(radar_pis_interia, axistype=1 , 
           
           #custom polygon
           pcol="#574ae2" , pfcol="#574ae2CC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="pis",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
sld_interia<-emotions_parties_interia%>%filter(term=="sld")

sld_interia<-sld_interia%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_sld_interia <- as.data.frame(t(matrix(sld_interia$procent)))
colnames(radar_sld_interia) <- sld_interia$category

radar_sld_interia <- rbind(rep(100, 5) , rep(0, 5) , radar_sld_interia)

radarchart(radar_sld_interia, axistype=1 , 
           
           #custom polygon
           pcol="#f21b3f" , pfcol="#f21b3fCC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="sld",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
konf_interia<-emotions_parties_interia%>%filter(term=="konf")

konf_interia<-konf_interia%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_konf_interia <- as.data.frame(t(matrix(konf_interia$procent)))
colnames(radar_konf_interia) <- konf_interia$category

radar_konf_interia <- rbind(rep(100, 5) , rep(0, 5) , radar_konf_interia)

radarchart(radar_konf_interia, axistype=1 , 
           
           #custom polygon
           pcol="#f0a202" , pfcol="#f0a202CC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="konf",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
psl_interia<-emotions_parties_interia%>%filter(term=="psl")

psl_interia<-psl_interia%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_psl_interia <- as.data.frame(t(matrix(psl_interia$procent)))
colnames(radar_psl_interia) <- psl_interia$category

radar_psl_interia <- rbind(rep(100, 5) , rep(0, 5) , radar_psl_interia)

radarchart(radar_psl_interia, axistype=1 , 
           
           #custom polygon
           pcol="#6da34d" , pfcol="#6da34dCC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="psl",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

######################################################################################
#--DODATKI-----------------------------------------------------------------
######################################################################################