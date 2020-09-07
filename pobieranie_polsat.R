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

######################################################################################
#--UTWORZENIE KORPUSU-----------------------------------------------------------------
######################################################################################
articles_polsat <- readRDS("articles_polsat.RDS")
datatable(articles_polsat)

articles_polsat<-articles_polsat%>%
  mutate(date = gsub(",.*", "", date))%>%
  mutate(date = as.Date(date, "%d.%m.%Y"))

articles_polsat<-articles_polsat%>%
  filter(date>="2019-08-09")%>%
  filter(date<"2019-10-12")

articles_polsat <- articles_polsat %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         hour = hour(date))

articles_polsat<-articles_polsat%>%
  mutate(body = gsub("Twoja przeglądarka nie wspiera.*videoPlayer", "", body))%>%
  mutate(body = gsub("WIDEO.*'", "", body))

articles_polsat %>%
  count(year, month, day) %>%
  ggplot(aes(make_date(year, month, day), n)) +
  geom_bar(stat="identity") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)

corpus_polsat<-cbind(articles_polsat$title, articles_polsat$lead, articles_polsat$body)
corpus_polsat<-as.data.frame(corpus_polsat)
colnames(corpus_polsat)<-c("title", "lead", "body")
corpus_polsat<-unite(corpus_polsat, "text", c("title", "lead", "body"), sep=" ")

datatable(corpus_polsat)

#wczytanie listy zarejestrowanych komitetow
komitety<-read.csv2("komitety_sejm_senat.csv", header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
komitety_polsat<-komitety

for(i in 1:nrow(komitety_polsat)){
  if(grepl(komitety_polsat[i, 1], corpus_polsat)==TRUE){
    komitety_polsat[i, 3]=TRUE
  }else{
    komitety_polsat[i, 3]=FALSE
  }
}

summary(komitety_polsat)
komitety_polsat_pelne<-komitety_polsat%>%slice_head(n=86)
kpopf<-komitety_polsat_pelne%>%filter(V3=="TRUE")

komitety_polsat_skroty<-komitety_polsat%>%slice_tail(n=86)
kposf<-komitety_polsat_skroty%>%filter(V3=="TRUE")

komitety_polsat_ogol<-inner_join(kpopf, kposf, by="Skrót")

#zastapienie nazw komitetow i odmienionych nazw partii akronimami
corpus_polsat <- mgsub(corpus_polsat, komitety$X.U.FEFF.Nazwa, komitety$Skrót, safe = TRUE)
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
  mutate(text = gsub("Polskim Stronnictwie Ludowym", "psl", text))%>%
  mutate(text = gsub("Konfederacja", "konf", text))%>%
  mutate(text = gsub("Konfederacji", "konf", text))%>%
  mutate(text = gsub("Konfederację", "konf", text))%>%
  mutate(text = gsub("Konfederacją", "konf", text))%>%
  mutate(text = gsub("Konfederacjo", "konf", text))

datatable(corpus_polsat)

corpus_polsat<-tibble(corpus_polsat)
corpus_polsat<-unlist(corpus_polsat)
corpus_polsat<-VCorpus(VectorSource(corpus_polsat))

save.corpus.to.files(corpus_polsat, filename = "new_corpus_polsat")

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

load(file="new_corpus_polsat.rda")
bigcorp<-corpus_polsat

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

save.corpus.to.files(corpus_polsat, filename = "new_corpus_polsat_c_s")

######################################################################################
#--DTM, LISTA FREKWENCYJNA------------------------------------------------------------
######################################################################################
#wczytanie korpusu po oczyszczeniu
load(file="new_corpus_polsat_c_s.rda")
corpus_polsat<-bigcorp

#macierz dokument-term
dtm_polsat = DocumentTermMatrix(corpus_polsat, control=list(wordLengths=c(1,Inf)))
inspect(dtm_polsat)

#czestosc slow
freq <- colSums(as.matrix(dtm_polsat))
freq <- sort(colSums(as.matrix(dtm_polsat)), decreasing=TRUE)

#ramka ze slowami i ich frekwencja
word_freq <- data.frame(word=names(freq), freq=freq)
datatable(word_freq)

#wykres frekwencji
top_n(word_freq, n=10, freq) %>%
  ggplot(., aes(x=reorder(word, -freq), y=freq))+
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)

#redukcja
dtm_polsat = removeSparseTerms(dtm_polsat, 0.99)
inspect(dtm_polsat)

######################################################################################
#--WYSTEPOWANIE NAZW PARTII-----------------------------------------------------------
######################################################################################
#korpus jako ramka danych
corpus_polsat_df<-data.frame(text = sapply(corpus_polsat, as.character), stringsAsFactors = FALSE)

date_cols<-articles_polsat%>%
  dplyr::select(year, month, day, url)

corpus_polsat_df<-cbind(corpus_polsat_df, date_cols)

body_words <- corpus_polsat_df %>%
  unnest_tokens(word_s, text, token = "words")

articles_per_day <- articles_polsat %>%
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
results_1_polsat <- FindTopicsNumber(
  dtm_polsat,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "CaoJuan2009"),
  method = "VEM",
  mc.cores = 4L,
  verbose = TRUE
)

results_2_polsat <- FindTopicsNumber(
  dtm_polsat,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "Griffiths2004", "CaoJuan2009"),
  method = "Gibbs",
  mc.cores = 4L,
  verbose = TRUE
)

#wykres pozwalajacy wybrac liczbe tematow
FindTopicsNumber_plot(results_1_polsat)
FindTopicsNumber_plot(results_2_polsat)

#lda
lda_polsat <- LDA(dtm_polsat, k=10, method = "VEM", control=list(seed=1234))

#zapisanie beta i gamma
topics_words_polsat <- tidy(lda_polsat, matrix = "beta")
topics_docs_polsat <- tidy(lda_polsat, matrix = "gamma")

#klasyfikacja kazdego dokumentu
doc_classes_polsat <- topics_docs_polsat %>%
  group_by(document) %>%
  top_n(1) %>%
  ungroup()

#liczba dokumentow w temacie
doc_classes_polsat%>% count(topic)

#wykres slow dla poszczegolnych tematow (ogolne skale)
topics_words_polsat %>%
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
ap_top_terms_polsat <- topics_words_polsat %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#wykres topowych slow ze wspolczynnikami (kazdy ma skale beta)
ap_top_terms_polsat %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol=4) +
  coord_flip()

assignments_polsat<-augment(lda_polsat, dtm_polsat)

######################################################################################
#--ANALIZA SENTYMENTU-----------------------------------------------------------------
######################################################################################

pl_words_sentiment <- read_csv("pl_words.csv")
pl_words_sentiment <- read_csv("nawl-analysis.csv")

as_tidy_polsat <- tidy(dtm_polsat)

text_words_sentiment_polsat <- inner_join(as_tidy_polsat %>%
                                         dplyr::select(document, term),
                                       pl_words_sentiment,
                                       by = c("term" = "word"))

emotions_polsat<-text_words_sentiment_polsat %>%
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


all_emotions_polsat<-emotions_polsat%>%
  group_by(category)%>%
  summarise(sum=sum(n))

all_emotions_polsat$zrodlo<-rep("Polsat News", 5)


nr_polsat<-as.data.frame(seq(1:nrow(articles_polsat)))
colnames(nr_polsat)<-c("nr")
new_articles_polsat<-cbind(nr_polsat, articles_polsat)
new_articles_polsat$nr<-as.character(new_articles_polsat$nr)

articles_emotions_polsat <- inner_join(new_articles_polsat %>%
                                      dplyr::select(nr, year, month, day),
                                    emotions_polsat,
                                    by = c("nr" = "document"))

articles_emotions_polsat<-articles_emotions_polsat%>%
  mutate(date=make_date(year, month, day))

colnames(articles_emotions_polsat)<-c("Dokument", "Rok", "Miesiac", "Dzien", "Emocje", "Liczba", "Data" )
grouped_emotions_polsat<-articles_emotions_polsat%>%
  group_by(Emocje, Data)%>%
  summarise(Liczba = sum(Liczba))

ggplot(grouped_emotions_polsat, aes(fill=Emocje, y=Liczba, x=Data)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#3d538f", "#BAA898", "#848586", "#C2847A", "#0d0f06"))+
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  #geom_text(data=subset(grouped_emotions_polsat, Emocje!=""), aes(label = Liczba), position = position_stack(vjust = 0.5), colour = "white")+
  theme(legend.position = "bottom")


only_parties<-as_tidy_polsat%>%
  filter(term %in% c("pis", "ko", "sld", "psl", "konf"))

articles_parties_polsat<-inner_join(only_parties%>%select(document, term),
                                 emotions_polsat,
                                 by = c("document" = "document"))

emotions_parties_polsat<-articles_parties_polsat%>%
  group_by(term, category)%>%
  summarise(Liczba = sum(n))

ggplot(emotions_parties_polsat, aes(fill=category, y=Liczba, x=term)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#222E50", "#BAA898", "#848586", "#C2847A", "#280003"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(data=subset(emotions_parties_polsat, category!=""), aes(label = Liczba), position = position_stack(vjust = 0.5), colour = "white")+
  theme(legend.position = "bottom")

par(mfrow=c(2,3))

#---
ko_polsat<-emotions_parties_polsat%>%filter(term=="ko")

ko_polsat<-ko_polsat%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_ko_polsat <- as.data.frame(t(matrix(ko_polsat$procent)))
colnames(radar_ko_polsat) <- ko_polsat$category

radar_ko_polsat <- rbind(rep(100, 5) , rep(0, 5) , radar_ko_polsat)

radarchart(radar_ko_polsat, axistype=1 , 
           
           #custom polygon
           pcol="#0a122a" , pfcol="#0a122aCC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="ko",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
pis_polsat<-emotions_parties_polsat%>%filter(term=="pis")

pis_polsat<-pis_polsat%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_pis_polsat <- as.data.frame(t(matrix(pis_polsat$procent)))
colnames(radar_pis_polsat) <- pis_polsat$category

radar_pis_polsat <- rbind(rep(100, 5) , rep(0, 5) , radar_pis_polsat)

radarchart(radar_pis_polsat, axistype=1 , 
           
           #custom polygon
           pcol="#574ae2" , pfcol="#574ae2CC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="pis",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
sld_polsat<-emotions_parties_polsat%>%filter(term=="sld")

sld_polsat<-sld_polsat%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_sld_polsat <- as.data.frame(t(matrix(sld_polsat$procent)))
colnames(radar_sld_polsat) <- sld_polsat$category

radar_sld_polsat <- rbind(rep(100, 5) , rep(0, 5) , radar_sld_polsat)

radarchart(radar_sld_polsat, axistype=1 , 
           
           #custom polygon
           pcol="#f21b3f" , pfcol="#f21b3fCC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="sld",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
konf_polsat<-emotions_parties_polsat%>%filter(term=="konf")

konf_polsat<-konf_polsat%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_konf_polsat <- as.data.frame(t(matrix(konf_polsat$procent)))
colnames(radar_konf_polsat) <- konf_polsat$category

radar_konf_polsat <- rbind(rep(100, 5) , rep(0, 5) , radar_konf_polsat)

radarchart(radar_konf_polsat, axistype=1 , 
           
           #custom polygon
           pcol="#f0a202" , pfcol="#f0a202CC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="konf",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
psl_polsat<-emotions_parties_polsat%>%filter(term=="psl")

psl_polsat<-psl_polsat%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_psl_polsat <- as.data.frame(t(matrix(psl_polsat$procent)))
colnames(radar_psl_polsat) <- psl_polsat$category

radar_psl_polsat <- rbind(rep(100, 5) , rep(0, 5) , radar_psl_polsat)

radarchart(radar_psl_polsat, axistype=1 , 
           
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