###POBIERANIE ARTYKULOW---------------------------------------------------------------
###BIBLIOTEKI-------------------------------------------------------------------------
install.packages("rvest")
install.packages("tidyverse")
install.packages("stringr")
install.packages("lubridate")

library(rvest)
library(tidyverse)
library(stringr)
library(lubridate)
###-----------------------------------------------------------------------------------
#ustawienie list na naglowki, leady i teksty
headers<-list()
dates<-list()
leads<-list()
texts<-list()
#tags<-list()

#li.size-1x1:nth-child(1) > div:nth-child(1) > div:nth-child(2) > a:nth-child(1) > strong:nth-child(1) > span:nth-child(1)

url_wprost_1 <-"https://www.wprost.pl/wybory-parlamentarne-2019"

pages_wprost <- 8

get_list_wprost <- function(page_num){
  page <- read_html(url_wprost_1)
  links <- page %>%
    html_node("body")%>%
    html_nodes("div.left-column")%>%
    html_nodes("ul.box-list")%>%
    html_nodes("li.box-list-item")%>%
    html_nodes("div.news-data")%>%
    html_nodes("div.news-titlelead-wrapper")
  
  temp <- data_frame(link = links %>% html_node("a") %>% html_attr("href"),
                     title =  links %>% html_node("a") %>% html_text())
  
  return(temp)
  
}

wprost_links <- tibble()

for(i in 1:pages_wprost) {
  wprost_links_temp <- get_list_wprost(i)
  
  wprost_links <- bind_rows(wprost_links, wprost_links_temp)
  
  # czekamy, żeby być grzecznym dla serwerów
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
}

rm(wprost_links_temp, i)

# DO POPRAWIENIA
get_article <- function(article_url) {
  page <- read_html(article_url, encoding = "ISO_8859-2")
  
  # autor tekstu
  author <- page %>% html_node("div#gazeta_article_author") %>% html_text() %>% trimws()
  
  # data publikacji
  date <- page %>% html_node("div#gazeta_article_date") %>% html_text() %>% trimws() %>%
    str_replace_all("[\t\n ]", "") %>% dmy_hm()
  
  # tytuł tekstu
  title <- page %>%html_node("h1") %>% html_text() %>% trimws()
  
  # lead
  lead <- page %>% html_node("div#gazeta_article_lead") %>% html_text() %>% trimws()
  
  # pełna treść artykułu
  body <- page %>% html_node("div#gazeta_article_body") %>% html_text() %>% trimws()
  
  # wszystkie dane pakujemy razem
  article <- data_frame(title = title,
                        lead = lead,
                        body = body,

                        date = date,
                        url = article_url)
  
  # czekamy, żeby być grzecznym dla serwera
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
  
  return(article)
}
