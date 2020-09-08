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
install.packages("treemap")
install.packages("RColorBrewer")
install.packages("fmsb")

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
library(treemap)
library(RColorBrewer)
library(fmsb)

path<-getwd()
setwd(path)

######################################################################################
#--ZAPISYWANIE TEKSTOW----------------------------------------------------------------
######################################################################################
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

######################################################################################
#--UTWORZENIE KORPUSU-----------------------------------------------------------------
######################################################################################
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

#wykres czestosci publikowania
articles_pap %>%
  count(year, month, day) %>%
  ggplot(aes(make_date(year, month, day), n)) +
  geom_bar(stat="identity") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)

full_articles_pap<-cbind(articles_pap$title, articles_pap$lead, articles_pap$body)
full_articles_pap<-as.data.frame(full_articles_pap)
colnames(full_articles_pap)<-c("title", "lead", "body")
full_articles_pap<-unite(full_articles_pap, "text", c("title", "lead", "body"), sep=" ")

datatable(full_articles_pap)

#wczytanie listy zarejestrowanych komitetow
komitety<-read.csv2("komitety_sejm_senat.csv", header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
komitety_pap<-komitety

for(i in 1:nrow(komitety_pap)){
  if(grepl(komitety_pap[i, 1], full_articles_pap)==TRUE){
    komitety_pap[i, 3]=TRUE
  }else{
    komitety_pap[i, 3]=FALSE
  }
}

summary(komitety_pap)
komitety_pap_pelne<-komitety_pap%>%slice_head(n=86)
kppf<-komitety_pap_pelne%>%filter(V3=="TRUE")

komitety_pap_skroty<-komitety_pap%>%slice_tail(n=86)
kpsf<-komitety_pap_skroty%>%filter(V3=="TRUE")

komitety_pap_ogol<-inner_join(kppf, kpsf, by="Skrót")

#zastapienie nazw komitetow i odmienionych nazw partii akronimami
full_articles_pap <- mgsub(full_articles_pap, komitety$X.U.FEFF.Nazwa, komitety$Skrót, safe = TRUE)
full_articles_pap<-full_articles_pap%>%
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

datatable(full_articles_pap)

corpus_pap<-tibble(full_articles_pap)
corpus_pap<-unlist(corpus_pap)
corpus_pap<-VCorpus(VectorSource(corpus_pap))

save.corpus.to.files(corpus_pap, filename = "new_corpus_pap")

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
load(file="new_corpus_pap.rda")
bigcorp<-corpus_pap

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
bigcorp = tm_map(bigcorp, removePunctuation)
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
corpus_pap<-VCorpus(VectorSource(data_tib_v))

corpus_pap = tm_map(corpus_pap, content_transformer(tolower))
corpus_pap = tm_map(corpus_pap, removeWords, stopwords("pl", source = "stopwords-iso"))
corpus_pap = tm_map(corpus_pap, stripWhitespace)

save.corpus.to.files(corpus_gazeta, filename = "new_corpus_pap_c_s")

######################################################################################
#--DTM, LISTA FREKWENCYJNA------------------------------------------------------------
######################################################################################
#wczytanie korpusu po oczyszczeniu
load(file="new_corpus_pap_c_s.rda")
corpus_pap<-bigcorp

#macierz dokument-term
dtm_pap=DocumentTermMatrix(corpus_pap, control=list(wordLengths=c(1,Inf)))
inspect(dtm_pap)

#czestosc slow
freq <- colSums(as.matrix(dtm_pap))
freq <- sort(colSums(as.matrix(dtm_pap)), decreasing=TRUE)

#ramka ze slowami i ich frekwencja
word_freq <- data.frame(word=names(freq), freq=freq)
datatable(word_freq)

#wykres frekwencji
top_n(word_freq, n=10, freq) %>%
  ggplot(., aes(x=reorder(word, -freq), y=freq))+
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)

#redukcja
dtm_pap = removeSparseTerms(dtm_pap, 0.99)
inspect(dtm_pap)

######################################################################################
#--WYSTEPOWANIE NAZW PARTII-----------------------------------------------------------
######################################################################################
#korpus jako ramka danych
corpus_pap_df<-data.frame(text = sapply(corpus_pap, as.character), stringsAsFactors = FALSE)

date_cols<-articles_pap%>%
  dplyr::select(year, month, day, url)

corpus_pap_df<-cbind(corpus_pap_df, date_cols)

body_words <- corpus_pap_df %>%
  unnest_tokens(word_s, text, token = "words")

articles_per_day <- articles_pap %>%
  count(year, month, day) %>%
  ungroup() %>%
  rename(n_arts = n)

body_words %>%
  filter(word_s %in% c("pis", "ko", "psl", "sld", "konf")) %>%
  count(year, month, day, word_s) %>%
  ungroup() %>%
  rename(n_words = n) %>%
  left_join(articles_per_day, by = c("year" = "year", "month" = "month", "day"="day")) %>%
  mutate(n_words_plot = n_words) %>%
  mutate(date = make_date(year, month, day)) %>%
  ggplot() +
  geom_bar(data = articles_per_day, aes(make_date(year, month, day), n_arts),
           stat="identity") +
  geom_point(aes(date, n_words_plot, color = word_s, shape=word_s), size = 4) +
  theme(legend.position = "bottom")

######################################################################################
#--LDA--------------------------------------------------------------------------------
######################################################################################
#wybor liczby tematow w lda
results_1_pap <- FindTopicsNumber(
  dtm_pap,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "CaoJuan2009"),
  method = "VEM",
  mc.cores = 4L,
  verbose = TRUE
)

results_2_pap <- FindTopicsNumber(
  dtm_pap,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "Griffiths2004", "CaoJuan2009"),
  method = "Gibbs",
  mc.cores = 4L,
  verbose = TRUE
)

#wykres pozwalajacy wybrac liczbe tematow
FindTopicsNumber_plot(results_1_pap)
FindTopicsNumber_plot(results_2_pap)

#lda
lda_pap <- LDA(dtm_pap, k = 8, method = "VEM", control=list(seed=1234))
lda_pap <- LDA(dtm_pap, k = 8, method = "Gibbs", control=list(seed=1234))

#zapisanie beta i gamma
topics_words_pap <- tidy(lda_pap, matrix = "beta")
topics_docs_pap <- tidy(lda_pap, matrix = "gamma")

#klasyfikacja kazdego dokumentu
doc_classes_pap <- topics_docs_pap %>%
  group_by(document) %>%
  top_n(1) %>%
  ungroup()

#liczba dokumentow w temacie
doc_classes_pap%>% count(topic)

#wykres slow dla poszczegolnych tematow (ogolne skale)
topics_words_pap %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(beta) %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  ggplot() +
  geom_col(aes(term, beta, fill = factor(topic)), color = "gray50", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free_y", ncol = 4) +
  coord_flip()

#topowe slowa w kazdym z tematow
ap_top_terms_pap <- topics_words_pap %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#wykres topowych slow ze wspolczynnikami (kazdy ma skale beta)
ap_top_terms_pap %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y", ncol = 4) +
  coord_flip()

assignments_pap<-augment(lda_pap, dtm_pap)

######################################################################################
#--ANALIZA SENTYMENTU-----------------------------------------------------------------
######################################################################################

#pl_words_sentiment <- read_csv("pl_words.csv")
pl_words_sentiment <- read_csv("nawl-analysis.csv")

as_tidy_pap <- tidy(dtm_pap)

text_words_sentiment_pap <- inner_join(as_tidy_pap %>%
                                         dplyr::select(document, term),
                                       pl_words_sentiment,
                                       by = c("term" = "word"))

emotions_pap<-text_words_sentiment_pap %>%
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

all_emotions_pap<-emotions_pap%>%
  group_by(category)%>%
  summarise(sum=sum(n))

all_emotions_pap$zrodlo<-rep("PAP", 5)

nr_pap<-as.data.frame(seq(1:nrow(articles_pap)))
colnames(nr_pap)<-c("nr")
new_articles_pap<-cbind(nr_pap, articles_pap)
new_articles_pap$nr<-as.character(new_articles_pap$nr)

articles_emotions_pap <- inner_join(new_articles_pap %>%
                           dplyr::select(nr, year, month, day),
                         emotions_pap,
                         by = c("nr" = "document"))

articles_emotions_pap<-articles_emotions_pap%>%
  mutate(date=make_date(year, month, day))

colnames(articles_emotions_pap)<-c("Dokument", "Rok", "Miesiac", "Dzien", "Emocje", "Liczba", "Data" )
grouped_emotions_pap<-articles_emotions_pap%>%
  group_by(Emocje, Data)%>%
  summarise(Liczba = sum(Liczba))%>%
  group_by(Data)%>%
  mutate(p = 100*Liczba/sum(Liczba))

ile_kiedy<-grouped_emotions_pap%>%
  group_by(Data)%>%
  summarise(c=sum(Liczba))

ggplot(grouped_emotions_pap, aes(fill=Emocje, y=p, x=Data)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#3d538f", "#BAA898", "#848586", "#C2847A", "#0d0f06"))+
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  theme(legend.position = "bottom")

only_parties<-as_tidy_pap%>%
  filter(term %in% c("pis", "ko", "sld", "psl", "konf"))

articles_parties_pap<-inner_join(only_parties%>%select(document, term),
                   emotions_pap,
                   by = c("document" = "document"))

emotions_parties_pap<-articles_parties_pap%>%
  group_by(term, category)%>%
  summarise(Liczba = sum(n))

ggplot(emotions_parties_pap, aes(fill=category, y=Liczba, x=term)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#222E50", "#BAA898", "#848586", "#C2847A", "#280003"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(data=subset(emotions_parties_pap, category!=""), aes(label = Liczba), position = position_stack(vjust = 0.5), colour = "white")+
  theme(legend.position = "bottom")

par(mfrow=c(2,3))
#---RADARY
ko_pap<-emotions_parties_pap%>%filter(term=="ko")

ko_pap<-ko_pap%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_ko_pap <- as.data.frame(t(matrix(ko_pap$procent)))
colnames(radar_ko_pap) <- ko_pap$category

radar_ko_pap <- rbind(rep(100, 5) , rep(0, 5) , radar_ko_pap)

radarchart(radar_ko_pap, axistype=1 , 
           pcol="#0a122a" , pfcol="#0a122aCC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="ko",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
pis_pap<-emotions_parties_pap%>%filter(term=="pis")

pis_pap<-pis_pap%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_pis_pap <- as.data.frame(t(matrix(pis_pap$procent)))
colnames(radar_pis_pap) <- pis_pap$category

radar_pis_pap <- rbind(rep(100, 5) , rep(0, 5) , radar_pis_pap)

radarchart(radar_pis_pap, axistype=1 , 
           pcol="#574ae2" , pfcol="#574ae2CC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="pis",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
sld_pap<-emotions_parties_pap%>%filter(term=="sld")

sld_pap<-sld_pap%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_sld_pap <- as.data.frame(t(matrix(sld_pap$procent)))
colnames(radar_sld_pap) <- sld_pap$category

radar_sld_pap <- rbind(rep(100, 5) , rep(0, 5) , radar_sld_pap)

radarchart(radar_sld_pap, axistype=1 , 
           pcol="#f21b3f" , pfcol="#f21b3fCC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="sld",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
konf_pap<-emotions_parties_pap%>%filter(term=="konf")

konf_pap<-konf_pap%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_konf_pap <- as.data.frame(t(matrix(konf_pap$procent)))
colnames(radar_konf_pap) <- konf_pap$category

radar_konf_pap <- rbind(rep(100, 5) , rep(0, 5) , radar_konf_pap)

radarchart(radar_konf_pap, axistype=1 , 
           pcol="#f0a202" , pfcol="#f0a202CC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="konf",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
psl_pap<-emotions_parties_pap%>%filter(term=="psl")

psl_pap<-psl_pap%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_psl_pap <- as.data.frame(t(matrix(psl_pap$procent)))
colnames(radar_psl_pap) <- psl_pap$category

radar_psl_pap <- rbind(rep(100, 5) , rep(0, 5) , radar_psl_pap)

radarchart(radar_psl_pap, axistype=1 , 
           pcol="#6da34d" , pfcol="#6da34dCC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="psl",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

######################################################################################
#--DODATKI-----------------------------------------------------------------
######################################################################################
all_sources<-rbind(all_emotions_pap, all_emotions_gazeta, all_emotions_tvn, all_emotions_polsat, all_emotions_interia)
colnames(all_sources)<-c("Emocje", "Liczba", "Źródło")

ggplot(all_sources, aes(fill=Emocje, y=Liczba, x=Źródło)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("#3d538f", "#BAA898", "#848586", "#C2847A", "#0d0f06"))+
  theme(legend.position = "bottom")+
  geom_text(aes(label=Liczba), position=position_dodge(width=0.9), vjust=-0.25)

install.packages("Compositional")
install.packages("MCMCpack")

library(Compositional)
library(MCMCpack)

draws_1 <- rdirichlet(100, c(0.1, 0.1, 0.1))
draws_2 <- rdirichlet(200, c(0.5, 0.5, 0.5))
draws_3 <- rdirichlet(200, c(1, 1, 1))
draws_4 <- rdirichlet(200, c(5, 5, 5))
par(mfrow=c(2,2))
bivt.contour(draws_1)
legend("topleft", legend=expression(paste(alpha, "=(0.1, 0.1, 0.1)")),
       cex=1.5, bg="transparent", box.lty=0)
bivt.contour(draws_2)
legend("topleft", legend=expression(paste(alpha, "=(0.5, 0.5, 0.5)")),
       cex=1.5, bg="transparent", box.lty=0)
bivt.contour(draws_3)
legend("topleft", legend=expression(paste(alpha, "=(1, 1, 1)")),
       cex=1.5, bg="transparent", box.lty=0)
bivt.contour(draws_4)
legend("topleft", legend=expression(paste(alpha, "=(5, 5, 5)")),
      cex=1.5, bg="transparent", box.lty=0)

ramka<-data.frame("Źródło" = rep(c("PAP", "gazeta.pl", "TVN24", "Polsat News", "Interia"), each=3), 
                  "Nazwa" = rep(c("Pełna", "Skrót", "Obie"), 5), 
                  "Liczba"=c(0,38,4,1,4,4,1,64,10,0,21,2,1,60,14))

ggplot(ramka, aes(fill=Nazwa, y=Liczba, x=Źródło)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_grey(start=0.6, end=0.1)+
  theme(legend.position = "bottom")+
  geom_text(data=subset(ramka, Liczba != 0), aes(label = Liczba), position = position_stack(vjust = 0.5), colour = "white")
