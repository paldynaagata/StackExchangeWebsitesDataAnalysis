library(dplyr)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(readr)# w razie gdyby read_lines nie działało
library(stringi)
# Najczęstsze słowa w postach i jak to sie ma do tagów? Czy użytkownicy dobrze tagują swoje posty?

#------eng_stop_words ---dokument txt gdzies są przetrzymywane tzw stop_wrods->wersja angielska
eng_stop_words <-read_lines("C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/PADR/eng_stop_words.txt")
eng_stop_words <-read_lines("C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/eng_stop_words.txt")

#------------scieżki u Mariki

fitness<-"C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/serwisy_csv/fitness_stackexchange_com"
interpersonal<-"C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/serwisy_csv/interpersonal_stackexchange_com"
worldbuilding<-"C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/serwisy_csv/worldbuilding_stackexchange_com"

#------------scieżki u Agaty

fitness<-"C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/serwisy_csv/fitness_stackexchange_com"
interpersonal<-"C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/serwisy_csv/interpersonal_stackexchange_com"
worldbuilding<-"C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/serwisy_csv/worldbuilding_stackexchange_com"

#----wczytanie Tags
TagsF<-read.csv(paste(fitness, "/Tags.csv", sep=""), header=TRUE)
TagsI<-read.csv(paste(interpersonal, "/Tags.csv", sep=""), header=TRUE)
TagsW<-read.csv(paste(worldbuilding, "/Tags.csv", sep=""), header=TRUE)

#---naczęstsze tagi 
TopTagsF<-TagsF%>%arrange(desc(Count))%>%select(TagName, Count)%>%
  as.data.frame()

write.csv(TopTagsF, file = "C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/dane_posrednie/q1/TopTagsF.csv", row.names = FALSE)

TopTagsI<-TagsI%>%arrange(desc(Count))%>%select(TagName, Count)%>%
  as.data.frame() 

write.csv(TopTagsI, file = "C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/dane_posrednie/q1/TopTagsI.csv", row.names = FALSE)

TopTagsW<-TagsW%>%arrange(desc(Count))%>%select(TagName, Count)%>%
  as.data.frame() 

write.csv(TopTagsW, file = "C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/dane_posrednie/q1/TopTagsW.csv", row.names = FALSE)

#-----wczytanie Posts

PostsF<-read.csv(paste(fitness, "/Posts.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
PostsI<-read.csv(paste(interpersonal, "/Posts.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
PostsW<-read.csv(paste(worldbuilding, "/Posts.csv", sep=""), header=TRUE,stringsAsFactors = FALSE)

#----wykres słupkowy i chmura słów z tagami

#--Fitness

TopTagsF%>%top_n(20, Count)%>%mutate(TagName=reorder(TagName, Count))%>%
  ggplot()+ aes(x=TagName, y=Count) + geom_bar(stat="identity", fill="magenta4") + coord_flip() +
  labs(title = "Najpopularniejsze tagi",
       y = "Liczba", x = "")



wordcloud(TopTagsF$TagName, TopTagsF$Count,
          max.words = 100, random.order = FALSE,
          scale = c(2.8, 0.8),
          colors = RColorBrewer::brewer.pal(9, "BuPu")[5:9])


#---Interpersonal

TopTagsI%>%top_n(20, Count)%>%mutate(TagName=reorder(TagName, Count))%>%
  ggplot()+ aes(x=TagName, y=Count) + geom_bar(stat="identity", fill="seagreen4") + coord_flip() +
  labs(title = "Najpopularniejsze tagi",
       y = "Liczba", x = "")

wordcloud(TopTagsI$TagName, TopTagsI$Count,
          max.words = 100,
          scale = c(2.8, 0.8),random.order = FALSE,
          colors = RColorBrewer::brewer.pal(9, "YlGn")[5:9])


#-----Worldbuilding

TopTagsW%>%top_n(20, Count)%>%mutate(TagName=reorder(TagName, Count))%>%
  ggplot()+ aes(x=TagName, y=Count) + geom_bar(stat="identity", fill="turquoise4") + coord_flip() +
  labs(title = "Najpopularniejsze tagi",
       y = "Liczba", x = "")

wordcloud(TopTagsW$TagName, TopTagsW$Count,
            max.words = 50,min.freq = 115,random.order = FALSE,
          scale = c(2.8, .8),
          colors = RColorBrewer::brewer.pal(9, "PuBu")[5:9])


# Teraz wyszukujemy najczęstsze słowa

# -------------Fitness
# musimy wyczyscic tresci postów z niepotrzebnych kropek, przecinków itp,
#tak zebysmy potem mieli tekst z pojedynczych słów
BodyF <- PostsF %>%
  mutate(post_content_stripped = Body %>%
           # usuwamy kod z postów - wszystko pomiędzy "<pre" a "</pre>" usuwamy
           stri_replace_all_regex("<pre(.|\\n|\\r)*?<\\/pre>", "") %>%
           # usuwamy znaczniki html z treści
           stri_replace_all_regex("<.*?>", "") %>%
           # zamieniamy kropki i przecinki na spacje
           stri_replace_all_regex("(\\.|\\,)", " ") %>%
           #usuwamy znacznik końca wiersza
           stri_replace_all_regex("\\n", " "))

# podział tekstów na słowa
WordsF <- BodyF %>%
  select(Title, post_content_stripped) %>%
  unnest_tokens(WordsF, post_content_stripped) %>%
  count(Title, WordsF) %>%
  # usuwamy stop words
  filter(!WordsF %in% eng_stop_words) %>%
  # usuwamy liczby
  filter(is.na(as.numeric(WordsF))) %>%
  # zostawiamy słowa dłuższe niz 2 znaki
  filter(nchar(WordsF) > 2)
 

# zliczamy te słowa
Words_cntF <- WordsF %>%
  group_by(WordsF) %>%
  summarise(n = sum(n)) %>%
  ungroup()

write.csv(Words_cntF %>%
            top_n(50, n), file = "C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/dane_posrednie/q1/Words_cntF.csv", row.names = FALSE)
 
# Wykresy-bar plot i chmurka

Words_cntF %>%
  top_n(50, n) %>%
  mutate(WordsF = reorder(WordsF, n)) %>%
  ggplot() + aes(x=WordsF, y=n) +
  geom_bar(stat="identity", fill="magenta4") +
  coord_flip() +
  labs(title = "Najpopularniejsze s�owa u�yte w postach",
       y = "Liczba wyst�pie�", x = "")


wordcloud(Words_cntF$WordsF, Words_cntF$n,
          max.words = 100,random.order = FALSE,
          scale = c(2.8, 0.8),
          colors = RColorBrewer::brewer.pal(9, "BuPu")[5:9])

# -------Interpersonal


BodyI <- PostsI %>%
  mutate(post_content_stripped = Body %>%
           # usuwamy kod z postów - wszystko pomiędzy "<pre" a "</pre>" usuwamy
           stri_replace_all_regex("<pre(.|\\n|\\r)*?<\\/pre>", "") %>%
           # usuwamy znaczniki html z treści
           stri_replace_all_regex("<.*?>", "") %>%
           # zamienamy kropki i przecinki na spacje
           stri_replace_all_regex("(\\.|\\,)", " ") %>%
           #usuwamy znacznik końca wiersza
           stri_replace_all_regex("\\n", " "))

# podział tekstów na słowa
WordsI <- BodyI %>%
  select(Title, post_content_stripped) %>%
  unnest_tokens(WordsI, post_content_stripped) %>%
  count(Title, WordsI) %>%
  # usuwamy stop words
  filter(!WordsI %in% eng_stop_words) %>%
  # usuwamy liczby
  filter(is.na(as.numeric(WordsI))) %>%
  # zostawiamy słowa dłuższe niz 2 znaki
  filter(nchar(WordsI) > 2)


# zliczamy te słowa
Words_cntI <- WordsI %>%
  group_by(WordsI) %>%
  summarise(n = sum(n)) %>%
  ungroup()

write.csv(Words_cntI %>%
            top_n(50, n), file = "C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/dane_posrednie/q1/Words_cntI.csv", row.names = FALSE)

# Wykresy-bar plot i chmurka

Words_cntI %>%
  top_n(50, n) %>%
  mutate(WordsI = reorder(WordsI, n)) %>%
  ggplot() + aes(x=WordsI, y=n) +
  geom_bar(stat="identity", fill="seagreen4") +
  coord_flip() +
  labs(title = "Najpopularniejsze s�owa u�yte w postach",
       y = "Liczba wyst�pie�", x = "")


wordcloud(Words_cntI$WordsI, Words_cntI$n,
          max.words = 100,random.order = FALSE,
          scale = c(2.8, 0.8),
          colors = RColorBrewer::brewer.pal(9, "YlGn")[5:9])

# WorldBuilding- UWAGA to sie bardzo długo mieli, moj komp nie dał rady, ale kod jest dobry, 
#musisz wywołac u siebie

BodyW <- PostsW %>%
  mutate(post_content_stripped = Body %>%
           # usuwamy kod z postów - wszystko pomiędzy "<pre" a "</pre>" usuwamy
           stri_replace_all_regex("<pre(.|\\n|\\r)*?<\\/pre>", "") %>%
           # usuwamy znaczniki html z treści
           stri_replace_all_regex("<.*?>", "") %>%
           # zamienamy kropki i przecinki na spacje
           stri_replace_all_regex("(\\.|\\,)", " ") %>%
           #usuwamy znacznik końca wiersza
           stri_replace_all_regex("\\n", " "))

# podział tekstów na słowa
WordsW <- BodyW %>%
  select(Title, post_content_stripped) %>%
  unnest_tokens(WordsW, post_content_stripped) %>%
  count(Title, WordsW) %>%
  # usuwamy stop words
  filter(!WordsW %in% eng_stop_words) %>%
  # usuwamy liczby
  filter(is.na(as.numeric(WordsW))) %>%
  # zostawiamy słowa dłuższe niz 2 znaki
  filter(nchar(WordsW) > 2)


# zliczamy te słowa
Words_cntW <- WordsW %>%
  group_by(WordsW) %>%
  summarise(n = sum(n)) %>%
  ungroup()

write.csv(Words_cntW %>%
            top_n(50, n), file = "C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/dane_posrednie/q1/Words_cntW.csv", row.names = FALSE)

# Wykresy-bar plot i chmurka

Words_cntW %>%
  top_n(50, n) %>%
  mutate(WordsW = reorder(WordsW, n)) %>%
  ggplot() + aes(x=WordsW, y=n) +
  geom_bar(stat="identity", fill="turquoise4") +
  coord_flip() +
  labs(title = "Najpopularniejsze s�owa u�yte w postach",
       y = "Liczba wyst�pie�", x = "")


wordcloud(Words_cntW$WordsW, Words_cntW$n,
          max.words = 100,random.order = FALSE,
          scale = c(2.8, 0.8),
          colors = RColorBrewer::brewer.pal(9, "PuBu")[5:9])

