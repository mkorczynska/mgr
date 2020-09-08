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

######################################################################################
#--UTWORZENIE KORPUSU-----------------------------------------------------------------
######################################################################################
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

articles_gazeta<-articles_gazeta%>%
  mutate(body = gsub("REKLAMA.*;});}","", body))%>%
  mutate(body = gsub("Zobacz wideo", "", body))

articles_gazeta<-articles_gazeta%>%
  mutate(body = gsub("\n    if.*});\n    }\n", "", body))%>%
  mutate(body = gsub("\n    if.*});\n    }", "", body))

articles_gazeta %>%
  count(year, month, day) %>%
  ggplot(aes(make_date(year, month, day), n)) +
  geom_bar(stat="identity") +
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)

full_articles_gazeta<-cbind(articles_gazeta$title, articles_gazeta$lead, articles_gazeta$body)
full_articles_gazeta<-as.data.frame(full_articles_gazeta)
colnames(full_articles_gazeta)<-c("title", "lead", "body")
full_articles_gazeta<-unite(full_articles_gazeta, "text", c("title", "lead", "body"), sep=" ")

datatable(full_articles_gazeta)

#wczytanie listy zarejestrowanych komitetow
komitety<-read.csv2("komitety_sejm_senat.csv", header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
komitety_gazeta<-komitety

for(i in 1:nrow(komitety_gazeta)){
  if(grepl(komitety_gazeta[i, 1], full_articles_gazeta)==TRUE){
    komitety_gazeta[i, 3]=TRUE
  }else{
    komitety_gazeta[i, 3]=FALSE
  }
}

summary(komitety_gazeta)
komitety_gazeta_pelne<-komitety_gazeta%>%slice_head(n=86)
kgpf<-komitety_gazeta_pelne%>%filter(V3=="TRUE")

komitety_gazeta_skroty<-komitety_gazeta%>%slice_tail(n=86)
kgsf<-komitety_gazeta_skroty%>%filter(V3=="TRUE")

komitety_gazeta_ogol<-inner_join(kgpf, kgsf, by="Skrót")

#zastapienie nazw komitetow i odmienionych nazw partii akronimami
corpus_gazeta <- mgsub(corpus_gazeta, komitety$X.U.FEFF.Nazwa, komitety$Skrót, safe = TRUE)
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
  mutate(text = gsub("Polskim Stronnictwie Ludowym", "psl", text))%>%
  mutate(text = gsub("Konfederacja", "konf", text))%>%
  mutate(text = gsub("Konfederacji", "konf", text))%>%
  mutate(text = gsub("Konfederację", "konf", text))%>%
  mutate(text = gsub("Konfederacją", "konf", text))%>%
  mutate(text = gsub("Konfederacjo", "konf", text))

datatable(corpus_gazeta)

corpus_gazeta<-tibble(corpus_gazeta)
corpus_gazeta<-unlist(corpus_gazeta)
corpus_gazeta<-VCorpus(VectorSource(corpus_gazeta))

stats_corp_gazeta<-text_stats(corpus_gazeta)

save.corpus.to.files(corpus_gazeta, filename = "corpus_gazeta")

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
load(file="corpus_gazeta.rda")
bigcorp<-corpus_gazeta

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

save.corpus.to.files(corpus_gazeta, filename = "new_corpus_gazeta_c_s")

######################################################################################
#--DTM, LISTA FREKWENCYJNA------------------------------------------------------------
######################################################################################
#wczytanie korpusu po oczyszczeniu
load(file="new_corpus_gazeta_c_s.rda")
corpus_gazeta<-bigcorp

#macierz dokument-term
dtm_gazeta = DocumentTermMatrix(corpus_gazeta, control=list(wordLengths=c(1,Inf)))
inspect(dtm_gazeta)

#czestosc slow
freq <- colSums(as.matrix(dtm_gazeta))
freq <- sort(colSums(as.matrix(dtm_gazeta)), decreasing=TRUE)

#ramka ze slowami i ich frekwencja
word_freq <- data.frame(word=names(freq), freq=freq)
datatable(word_freq)

#wykres frekwencji
top_n(word_freq, n=10, freq) %>%
  ggplot(., aes(x=reorder(word, -freq), y=freq))+
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)

#redukcja
dtm_gazeta = removeSparseTerms(dtm_gazeta, 0.99)
inspect(dtm_gazeta)

######################################################################################
#--WYSTEPOWANIE NAZW PARTII-----------------------------------------------------------
######################################################################################
#korpus jako ramka danych
corpus_gazeta_df<-data.frame(text = sapply(corpus_gazeta, as.character), stringsAsFactors = FALSE)

date_cols<-articles_gazeta%>%
  select(year, month, day, url_gazeta)

corpus_gazeta_df<-cbind(corpus_gazeta_df, date_cols)

body_words <- corpus_gazeta_df %>%
  unnest_tokens(word_s, text, token = "words")

articles_per_day <- articles_gazeta %>%
  count(year, month, day) %>%
  ungroup() %>%
  rename(n_arts = n)

body_words %>%
  filter(word_s %in% c("pis", "ko", "lewica", "psl", "sld", "konfederacja")) %>%
  count(year, month, day, word_s) %>%
  ungroup() %>%
  rename(n_words = n) %>%
  left_join(articles_per_day, by = c("year" = "year", "month" = "month", "day"="day")) %>%
  mutate(n_words_plot = n_words) %>%
  mutate(date = make_date(year, month, day)) %>%
  ggplot() +
  geom_bar(data = articles_per_day, aes(make_date(year, month, day), n_arts),
           stat="identity",
           fill = "gray80") +
  geom_point(aes(date, n_words_plot, color = word_s, shape=word_s), size = 4) +
  theme(legend.position = "bottom")

######################################################################################
#--LDA--------------------------------------------------------------------------------
######################################################################################
#wybor liczby tematow w lda
results_1_gazeta <- FindTopicsNumber(
  dtm_gazeta,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "CaoJuan2009"),
  method = "VEM",
  mc.cores = 4L,
  verbose = TRUE
)

results_2_gazeta <- FindTopicsNumber(
  dtm_gazeta,
  topics = seq(from = 2, to = 10, by = 2),
  metrics = c("Arun2010", "Deveaud2014", "Griffiths2004", "CaoJuan2009"),
  method = "Gibbs",
  mc.cores = 4L,
  verbose = TRUE
)

#wykres pozwalajacy wybrac liczbe tematow
FindTopicsNumber_plot(results_1_gazeta)
FindTopicsNumber_plot(results_2_gazeta)

#lda
lda_gazeta <- LDA(dtm_gazeta, k=10, method = "VEM", control=list(seed=1234))

#zapisanie beta i gamma
topics_words_gazeta <- tidy(lda_gazeta, matrix = "beta")
topics_docs_gazeta <- tidy(lda_gazeta, matrix = "gamma")

#klasyfikacja kazdego dokumentu
doc_classes_gazeta <- topics_docs_gazeta %>%
  group_by(document) %>%
  top_n(1) %>%
  ungroup()

#liczba dokumentow w temacie
doc_classes_gazeta%>% count(topic)

#wykres slow dla poszczegolnych tematow (ogolne skale)
topics_words_gazeta %>%
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
ap_top_terms_gazeta <- topics_words_gazeta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#wykres topowych slow ze wspolczynnikami (kazdy ma skale beta)
ap_top_terms_gazeta %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol=4) +
  coord_flip()

assignments_gazeta<-augment(lda_gazeta, dtm_gazeta)

######################################################################################
#--ANALIZA SENTYMENTU-----------------------------------------------------------------
######################################################################################

#pl_words_sentiment <- read_csv("pl_words.csv")
pl_words_sentiment <- read_csv("nawl-analysis.csv")

as_tidy_gazeta <- tidy(dtm_gazeta)

text_words_sentiment_gazeta <- inner_join(as_tidy_gazeta %>%
                                         dplyr::select(document, term),
                                       pl_words_sentiment,
                                       by = c("term" = "word"))

emotions_gazeta<-text_words_sentiment_gazeta %>%
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

all_emotions_gazeta<-emotions_gazeta%>%
  group_by(category)%>%
  summarise(sum=sum(n))

all_emotions_gazeta$zrodlo<-rep("gazeta.pl", 5)

nr_gazeta<-as.data.frame(seq(1:nrow(articles_gazeta)))
colnames(nr_gazeta)<-c("nr")
new_articles_gazeta<-cbind(nr_gazeta, articles_gazeta)
new_articles_gazeta$nr<-as.character(new_articles_gazeta$nr)

articles_emotions_gazeta <- inner_join(new_articles_gazeta %>%
                                      dplyr::select(nr, year, month, day),
                                    emotions_gazeta,
                                    by = c("nr" = "document"))

articles_emotions_gazeta<-articles_emotions_gazeta%>%
  mutate(date=make_date(year, month, day))

colnames(articles_emotions_gazeta)<-c("Dokument", "Rok", "Miesiac", "Dzien", "Emocje", "Liczba", "Data" )
grouped_emotions_gazeta<-articles_emotions_gazeta%>%
  group_by(Emocje, Data)%>%
  summarise(Liczba = sum(Liczba))

ggplot(grouped_emotions_gazeta, aes(fill=Emocje, y=Liczba, x=Data)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#3d538f", "#BAA898", "#848586", "#C2847A", "#0d0f06"))+
  scale_x_date(date_breaks = "5 days", date_labels = "%d.%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  theme(legend.position = "bottom")

ile_kiedy_gazeta<-grouped_emotions_gazeta%>%
  group_by(Data)%>%
  summarise(c=sum(Liczba))

only_parties<-as_tidy_gazeta%>%
  filter(term %in% c("pis", "ko", "sld", "psl", "konf"))

articles_parties_gazeta<-inner_join(only_parties%>%select(document, term),
                                 emotions_gazeta,
                                 by = c("document" = "document"))

emotions_parties_gazeta<-articles_parties_gazeta%>%
  group_by(term, category)%>%
  summarise(Liczba = sum(n))

ggplot(emotions_parties_gazeta, aes(fill=category, y=Liczba, x=term)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("#222E50", "#BAA898", "#848586", "#C2847A", "#280003"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(data=subset(emotions_parties_gazeta, category!=""), aes(label = Liczba), position = position_stack(vjust = 0.5), colour = "white")+
  theme(legend.position = "bottom")

par(mfrow=c(2,3))

#---RADARY
ko_gazeta<-emotions_parties_gazeta%>%filter(term=="ko")

ko_gazeta<-ko_gazeta%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_ko_gazeta <- as.data.frame(t(matrix(ko_gazeta$procent)))
colnames(radar_ko_gazeta) <- ko_gazeta$category

radar_ko_gazeta <- rbind(rep(100, 5) , rep(0, 5) , radar_ko_gazeta)

radarchart(radar_ko_gazeta, axistype=1 , 
           pcol="#0a122a" , pfcol="#0a122aCC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="ko",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
pis_gazeta<-emotions_parties_gazeta%>%filter(term=="pis")

pis_gazeta<-pis_gazeta%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_pis_gazeta <- as.data.frame(t(matrix(pis_gazeta$procent)))
colnames(radar_pis_gazeta) <- pis_gazeta$category

radar_pis_gazeta <- rbind(rep(100, 5) , rep(0, 5) , radar_pis_gazeta)

radarchart(radar_pis_gazeta, axistype=1 , 
           pcol="#574ae2" , pfcol="#574ae2CC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="pis",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
sld_gazeta<-emotions_parties_gazeta%>%filter(term=="sld")

sld_gazeta<-sld_gazeta%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_sld_gazeta <- as.data.frame(t(matrix(sld_gazeta$procent)))
colnames(radar_sld_gazeta) <- sld_gazeta$category

radar_sld_gazeta <- rbind(rep(100, 5) , rep(0, 5) , radar_sld_gazeta)

radarchart(radar_sld_gazeta, axistype=1 , 
           pcol="#f21b3f" , pfcol="#f21b3fCC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="sld",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
konf_gazeta<-emotions_parties_gazeta%>%filter(term=="konf")

konf_gazeta<-konf_gazeta%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_konf_gazeta <- as.data.frame(t(matrix(konf_gazeta$procent)))
colnames(radar_konf_gazeta) <- konf_gazeta$category

radar_konf_gazeta <- rbind(rep(100, 5) , rep(0, 5) , radar_konf_gazeta)

radarchart(radar_konf_gazeta, axistype=1 , 
           pcol="#f0a202" , pfcol="#f0a202CC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="konf",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)

#---
psl_gazeta<-emotions_parties_gazeta%>%filter(term=="psl")

psl_gazeta<-psl_gazeta%>%
  mutate(procent=Liczba/sum(Liczba)*100)

radar_psl_gazeta <- as.data.frame(t(matrix(psl_gazeta$procent)))
colnames(radar_psl_gazeta) <- psl_gazeta$category

radar_psl_gazeta <- rbind(rep(100, 5) , rep(0, 5) , radar_psl_gazeta)

radarchart(radar_psl_gazeta, axistype=1 , 
           pcol="#6da34d" , pfcol="#6da34dCC" , plwd=4 , 
           cglcol="black", cglty=1, axislabcol="#280003", caxislabels=paste(seq(0,100,25), "%"), cglwd=0.8,
           vlcex=1.2)
legend("bottom", legend="psl",
       cex=1.2, bg="transparent", box.lty=0, text.font=2)