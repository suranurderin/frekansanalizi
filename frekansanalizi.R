#KUTUPHANELER
library(tuber) 
library(httr) 
library(purrr) 
library(tidyverse) 
library(httpuv) 
library(ROAuth) 
library(writexl) 
library(xlsx) 
library(readxl)
library(tm)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(stopwords)


#R'i Youtube'ye baglama
client_id <- "...."
client_secret_id <- "...." 
yt_oauth(client_id, client_secret_id, token = '') 

#Yorumlari excele cekme
get_all_comments(video_id = "..")
comments <- get_all_comments(video_id = "..")
write_xlsx(comments, "..")

#Excel dosyasini R'a aktarma islemi
yorumlar <- read_xlsx(file.choose())

#Yorumlari temizleme islemi
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "http[^[:space:]]*", " ") 
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "#//S+", " ") 
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "@//S+", " ") 
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "[[:punct:][:blank:]]+"," ") 
yorumlar$textOriginal <- str_to_lower(yorumlar$textOriginal, "tr")
yorumlar$textOriginal <- removeNumbers(yorumlar$textOriginal)
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "[<].*[>]", " ") 
yorumlar$textOriginal <- gsub("\uFFFD", "", yorumlar$textOriginal, fixed = TRUE)
yorumlar$textOriginal <- gsub("\n", "", yorumlar$textOriginal, fixed = TRUE)
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "[^[:alnum:]]", " ")

#T�rkce kelimelerin bozulmamasi icin asagidaki kodu kullaniyoruz
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

veri <- yorumlar %>% select(textOriginal)%>%
  mutate(linenumber=row_number())%>% unnest_tokens(word,textOriginal)

#Gereksiz kelimeleri temizleme
stopwords::stopwords("tr",source = "stopwords-iso")
liste <- stopwords::stopwords("tr",source = "stopwords-iso")

#Gereksiz kelimeleri manuel temizleme
liste <- c("")
yorumlar$textOriginal <- removeWords(yorumlar$textOriginal, liste)

kelimeler <- yorumlar %>% select(textOriginal)  %>%   mutate(linenumber = row_number()) %>% unnest_tokens(word, textOriginal)
head(kelimeler) # Ilk 6 kelimenin konsola getirilmesi

kelimeler %>% # Yorumlarda 50'den daha fazla kullanilan kelimelerin listelenmesi
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  filter(!is.na(word)) %>%  # NA olanlari filtrele
  filter(n > 0) %>%         # Frekansi 0 olanlari filtrele
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Paris Olimpiyatlarina Giden Yolda Filenin Sultanları Üzerine Frekans Analizi")

wordcloud(kelimeler$word, min.freq = 1, max.words = 200, scale = c(2,1,10), 
          colors = brewer.pal(8, "Dark2"),random.color = T, random.order = F)









