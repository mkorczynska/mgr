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
url_tvn_1 <-"https://tvn24.pl/wybory-parlamentarne-2019,5225,t/"

pages_tvn <- 28

get_list_tvn <- function(page_num){
  url_tvn<-paste0(url_tvn_1, page_num)
  page <- read_html(url_tvn)
  links <- page %>%
    html_node("body")%>%
    html_node("div.mainLeftColumn")%>%
    html_nodes("article")%>%
    html_nodes("h1")
  temp <- data_frame(link = links %>% html_node("a") %>% html_attr("href"))
  return(temp)
  
}

tvn_links <- tibble()

for(i in 13:pages_tvn) {
  tvn_links_temp <- get_list_tvn(i)
  tvn_links <- bind_rows(tvn_links, tvn_links_temp)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
}

rm(tvn_links_temp, i)

get_article <- function(article_url) {
  page <- read_html(article_url)
  date <- page %>% html_node("time") %>% html_text() %>% trimws()
  title <- page %>%html_node("h1") %>% html_text() %>% trimws()
  lead <- page %>% html_node("p.lead-text") %>% html_text() %>% trimws()
  body <- page %>% html_node("div.article-story-content__elements") %>% html_text() %>% trimws()
  article <- data_frame(title = title,
                        lead = lead,
                        body = body,
                        date = date,
                        url = article_url)
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
  return(article)
}

tvn_links<-tvn_links %>%
  drop_na()

tvn_links<-tvn_links %>%
  select(link) %>%
  filter(!str_detect(link, "https"))%>%
  mutate(full_link = paste0("https://tvn24.pl", link))

articles_tvn <- tvn_links %>%
  rowwise() %>%
  do(get_article(.$full_link)) %>%
  bind_rows() %>% 
  ungroup()

articles_tvn<-articles_tvn%>%
  drop_na()

saveRDS(articles_tvn, file = "articles_tvn.RDS")

######################################################################################
#--UTWORZENIE KORPUSU-----------------------------------------------------------------
######################################################################################
articles_tvn <- readRDS("articles_tvn.RDS")
datatable(articles_tvn)

articles_tvn<-articles_tvn%>%
  mutate(date = gsub(" października ", "-10-", date))%>%
  mutate(date = gsub(" września ", "-09-", date))%>%
  mutate(date = gsub(" sierpnia ", "-08-", date))%>%
  mutate(date = gsub("2019.*", "2019", date))%>%
  mutate(date = as.Date(date, "%d-%m-%Y"))

articles_tvn<-articles_tvn%>%
  filter(date>="2019-08-09")%>%
  filter(date<"2019-10-12")

articles_tvn <- articles_tvn %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         hour = hour(date))

articles_tvn<-articles_tvn%>%
  mutate(body = gsub("Autor.*", "", body))%>%
  mutate(body = gsub("CZYTAJ WIĘCEJ", "", body))

articles_tvn %>%
  count(year, month, day) %>%
  ggplot(aes(make_date(year, month, day), n)) +
  geom_bar(stat="identity") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)

corpus_tvn<-cbind(articles_tvn$title, articles_tvn$lead, articles_tvn$body)
corpus_tvn<-as.data.frame(corpus_tvn)
colnames(corpus_tvn)<-c("title", "lead", "body")
corpus_tvn<-unite(corpus_tvn, "text", c("title", "lead", "body"), sep=" ")

datatable(corpus_tvn)

#wczytanie listy zarejestrowanych komitetow
komitety<-read.csv2("komitety_sejm_senat.csv", header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
komitety_tvn<-komitety

for(i in 1:nrow(komitety_tvn)){
  if(grepl(komitety_tvn[i, 1], corpus_tvn)==TRUE){
    komitety_tvn[i, 3]=TRUE
  }else{
    komitety_tvn[i, 3]=FALSE
  }
}

summary(komitety_tvn)
komitety_tvn_pelne<-komitety_tvn%>%slice_head(n=86)
ktpf<-komitety_tvn_pelne%>%filter(V3=="TRUE")

komitety_tvn_skroty<-komitety_tvn%>%slice_tail(n=86)
ktsf<-komitety_tvn_skroty%>%filter(V3=="TRUE")

komitety_tvn_ogol<-inner_join(ktpf, ktsf, by="Skrót")

#zastapienie nazw komitetow i odmienionych nazw partii akronimami
corpus_tvn <- mgsub(corpus_tvn, komitety$X.U.FEFF.Nazwa, komitety$Skrót, safe = TRUE)
corpus_tvn<-corpus_tvn%>%
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

datatable(corpus_tvn)

corpus_tvn<-tibble(corpus_tvn)
corpus_tvn<-unlist(corpus_tvn)
corpus_tvn<-VCorpus(VectorSource(corpus_tvn))

save.corpus.to.files(corpus_tvn, filename = "new_corpus_tvn")

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

load(file="new_corpus_tvn.rda")
bigcorp<-corpus_tvn

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
corpus_tvn<-VCorpus(VectorSource(data_tib_v))

corpus_tvn = tm_map(corpus_tvn, content_transformer(tolower))
corpus_tvn = tm_map(corpus_tvn, removeWords, stopwords("pl", source = "stopwords-iso"))
corpus_tvn = tm_map(corpus_tvn, stripWhitespace)

save.corpus.to.files(corpus_tvn, filename = "new_corpus_tvn_c_s")

######################################################################################
#--DTM, LISTA FREKWENCYJNA------------------------------------------------------------
######################################################################################
#wczytanie korpusu po oczyszczeniu
load(file="new_corpus_tvn_c_s.rda")
corpus_tvn<-bigcorp

#macierz dokument-term
dtm_tvn = DocumentTermMatrix(corpus_tvn, control=list(wordLengths=c(1,Inf)))
inspect(dtm_tvn)

#czestosc slow
freq <- colSums(as.matrix(dtm_tvn))
freq <- sort(colSums(as.matrix(dtm_tvn)), decreasing=TRUE)

#ramka ze slowami i ich frekwencja
word_freq <- data.frame(word=names(freq), freq=freq)
datatable(word_freq)

#wykres frekwencji
top_n(word_freq, n=10, freq) %>%
  ggplot(., aes(x=reorder(word, -freq), y=freq))+
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)

#redukcja
dtm_tvn = removeSparseTerms(dtm_tvn, 0.99)
inspect(dtm_tvn)

######################################################################################
#--WYSTEPOWANIE NAZW PARTII-----------------------------------------------------------
######################################################################################
#korpus jako ramka danych
corpus_tvn_df<-data.frame(text = sapply(corpus_tvn, as.character), stringsAsFactors = FALSE)

date_cols<-articles_tvn%>%
  dplyr::select(year, month, day, url)

corpus_tvn_df<-cbind(corpus_tvn_df, date_cols)

body_words <- corpus_tvn_df %>%
  unnest_tokens(word_s, text, token = "words")

articles_per_day <- articles_tvn %>%
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
results_1_tvn <- FindTopicsNumber(
  dtm_tvn,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "CaoJuan2009"),
  method = "VEM",
  mc.cores = 4L,
  verbose = TRUE
)

results_2_tvn <- FindTopicsNumber(
  dtm_tvn,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "Griffiths2004", "CaoJuan2009"),
  method = "Gibbs",
  mc.cores = 4L,
  verbose = TRUE
)

#wykres pozwalajacy wybrac liczbe tematow
FindTopicsNumber_plot(results_1_tvn)
FindTopicsNumber_plot(results_2_tvn)

#lda
lda_tvn <- LDA(dtm_tvn, k=10, method = "VEM", control=list(seed=1234))

#zapisanie beta i gamma
topics_words_tvn <- tidy(lda_tvn, matrix = "beta")
topics_docs_tvn <- tidy(lda_tvn, matrix = "gamma")

#klasyfikacja kazdego dokumentu
doc_classes_tvn <- topics_docs_tvn %>%
  group_by(document) %>%
  top_n(1) %>%
  ungroup()

#liczba dokumentow w temacie
doc_classes_tvn%>% count(topic)

#wykres slow dla poszczegolnych tematow (ogolne skale)
topics_words_tvn %>%
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
ap_top_terms_tvn <- topics_words_tvn %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#wykres topowych slow ze wspolczynnikami (kazdy ma skale beta)
ap_top_terms_tvn %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  coord_flip()

assignments_tvn<-augment(lda_tvn, dtm_tvn)

######################################################################################
#--ANALIZA SENTYMENTU-----------------------------------------------------------------
######################################################################################

pl_words_sentiment <- read_csv("pl_words.csv")
pl_words_sentiment <- read_csv("nawl-analysis.csv")

as_tidy_tvn <- tidy(dtm_tvn)

text_words_sentiment_tvn <- inner_join(as_tidy_tvn %>%
                                            dplyr::select(document, term),
                                          pl_words_sentiment,
                                          by = c("term" = "word"))

emotions_tvn<-text_words_sentiment_tvn %>%
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



all_emotions_tvn<-emotions_tvn%>%
  group_by(category)%>%
  summarise(sum=sum(n))

all_emotions_tvn$zrodlo<-rep("TVN24", 5)


nr_tvn<-as.data.frame(seq(1:nrow(articles_tvn)))
colnames(nr_tvn)<-c("nr")
new_articles_tvn<-cbind(nr_tvn, articles_tvn)
new_articles_tvn$nr<-as.character(new_articles_tvn$nr)

articles_emotions_tvn <- inner_join(new_articles_tvn %>%
                                         dplyr::select(nr, year, month, day),
                                       emotions_tvn,
                                       by = c("nr" = "document"))

articles_emotions_tvn<-articles_emotions_tvn%>%
  mutate(date=make_date(year, month, day))

colnames(articles_emotions_tvn)<-c("Dokument", "Rok", "Miesiac", "Dzien", "Emocje", "Liczba", "Data" )
grouped_emotions_tvn<-articles_emotions_tvn%>%
  group_by(Emocje, Data)%>%
  summarise(Liczba = sum(Liczba))

ggplot(grouped_emotions_tvn, aes(fill=Emocje, y=Liczba, x=Data)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#3d538f", "#BAA898", "#848586", "#C2847A", "#0d0f06"))+
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  #geom_text(data=subset(grouped_emotions_tvn, Emocje!=""), aes(label = Liczba), position = position_stack(vjust = 0.5), colour = "white")+
  theme(legend.position = "bottom")


only_parties<-as_tidy_tvn%>%
  filter(term %in% c("pis", "ko", "sld", "psl", "konf"))

articles_parties_tvn<-inner_join(only_parties%>%select(document, term),
                                    emotions_tvn,
                                    by = c("document" = "document"))

emotions_parties_tvn<-articles_parties_tvn%>%
  group_by(term, category)%>%
  summarise(Liczba = sum(n))

ggplot(emotions_parties_tvn, aes(fill=category, y=Liczba, x=term)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#222E50", "#BAA898", "#848586", "#C2847A", "#280003"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(data=subset(emotions_parties_tvn, category!=""), aes(label = Liczba), position = position_stack(vjust = 0.5), colour = "white")+
  theme(legend.position = "bottom")

par(mfrow=c(2,3))

#---
ko_tvn<-emotions_parties_tvn%>%filter(term=="ko")

ko_tvn<-ko_tvn%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_ko_tvn <- as.data.frame(t(matrix(ko_tvn$procent)))
colnames(radar_ko_tvn) <- ko_tvn$category

radar_ko_tvn <- rbind(rep(100, 5) , rep(0, 5) , radar_ko_tvn)

radarchart(radar_ko_tvn, axistype=1 , 
           
           #custom polygon
           pcol="#0a122a" , pfcol="#0a122aCC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="ko",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
pis_tvn<-emotions_parties_tvn%>%filter(term=="pis")

pis_tvn<-pis_tvn%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_pis_tvn <- as.data.frame(t(matrix(pis_tvn$procent)))
colnames(radar_pis_tvn) <- pis_tvn$category

radar_pis_tvn <- rbind(rep(100, 5) , rep(0, 5) , radar_pis_tvn)

radarchart(radar_pis_tvn, axistype=1 , 
           
           #custom polygon
           pcol="#574ae2" , pfcol="#574ae2CC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="pis",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
sld_tvn<-emotions_parties_tvn%>%filter(term=="sld")

sld_tvn<-sld_tvn%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_sld_tvn <- as.data.frame(t(matrix(sld_tvn$procent)))
colnames(radar_sld_tvn) <- sld_tvn$category

radar_sld_tvn <- rbind(rep(100, 5) , rep(0, 5) , radar_sld_tvn)

radarchart(radar_sld_tvn, axistype=1 , 
           
           #custom polygon
           pcol="#f21b3f" , pfcol="#f21b3fCC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="sld",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
konf_tvn<-emotions_parties_tvn%>%filter(term=="konf")

konf_tvn<-konf_tvn%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_konf_tvn <- as.data.frame(t(matrix(konf_tvn$procent)))
colnames(radar_konf_tvn) <- konf_tvn$category

radar_konf_tvn <- rbind(rep(100, 5) , rep(0, 5) , radar_konf_tvn)

radarchart(radar_konf_tvn, axistype=1 , 
           
           #custom polygon
           pcol="#f0a202" , pfcol="#f0a202CC" , plwd=4 , 
           
           #custom the grid
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           
           #custom labels
           vlcex=1.2)
legend("bottom", legend="konf",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
psl_tvn<-emotions_parties_tvn%>%filter(term=="psl")

psl_tvn<-psl_tvn%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_psl_tvn <- as.data.frame(t(matrix(psl_tvn$procent)))
colnames(radar_psl_tvn) <- psl_tvn$category

radar_psl_tvn <- rbind(rep(100, 5) , rep(0, 5) , radar_psl_tvn)

radarchart(radar_psl_tvn, axistype=1 , 
           
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