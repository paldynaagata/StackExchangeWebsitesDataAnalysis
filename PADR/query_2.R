

#--------------------------------scieĹĽki - Marika ----------------
fitness<-"C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/serwisy_csv/fitness_stackexchange_com"
interpersonal<-"C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/serwisy_csv/interpersonal_stackexchange_com"
worldbuilding<-"C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/serwisy_csv/worldbuilding_stackexchange_com"

#------------scieżki - Agata

fitness<-"C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/serwisy_csv/fitness_stackexchange_com"
interpersonal<-"C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/serwisy_csv/interpersonal_stackexchange_com"
worldbuilding<-"C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/serwisy_csv/worldbuilding_stackexchange_com"

#---------Posts-------------------------------

PostsF<-read.csv(paste(fitness, "/Posts.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
PostsI<-read.csv(paste(interpersonal, "/Posts.csv", sep=""), header=TRUE)
PostsW<-read.csv(paste(worldbuilding, "/Posts.csv", sep=""), header=TRUE)


#-------------Posty ktĂłry miaĹy‚ najwiecej wyswietlen w Fitnessie(hehe)


TopViewsPostF<-PostsF%>%arrange(desc(ViewCount))%>%
  select(Id, PostTypeId, AcceptedAnswerId, CreationDate, ViewCount, Body, Title, CommentCount, OwnerUserId)

TopViewsPostF%>%
  top_n(15, ViewCount) %>%
  mutate(Title = reorder(Title, ViewCount)) %>%
  ggplot() + aes(x=Title, y=ViewCount) +
  geom_bar(stat="identity", fill="magenta4") +
  coord_flip() +
  labs(title = "Najpopularniejsze posty według liczby odsłon",
       y = "Liczba odsłon", x = "")

#-------------Post ktĂłry miaĹ‚ najwiecej wyswietlen w Interpersonal

TopViewsPostI<-PostsI%>%arrange(desc(ViewCount))%>%
  select(Id, PostTypeId, AcceptedAnswerId, CreationDate, ViewCount, Body, Title, CommentCount, Tags)

TopViewsPostI%>%
  top_n(15, ViewCount) %>%
  mutate(Title = reorder(Title, ViewCount)) %>%
  ggplot() + aes(x=Title, y=ViewCount) +
  geom_bar(stat="identity", fill="turquoise4") +
  coord_flip() +
  labs(title = "Najpopularniejsze posty według liczby odsłon",
       y = "Liczba odsłon", x = "")


#--------------Posty które miały njawiecej wyswietlen w Worldbuilding
TopViewsPostW<-PostsW%>%arrange(desc(ViewCount))%>%
  select(Id, PostTypeId, AcceptedAnswerId, CreationDate, ViewCount, Body, Title, CommentCount, Tags)

TopViewsPostW%>%
  top_n(15, ViewCount) %>%
  mutate(Title = reorder(Title, ViewCount)) %>%
  ggplot() + aes(x=Title, y=ViewCount) +
  geom_bar(stat="identity", fill="seagreen4") +
  coord_flip() +
  labs(title = "Najpopularniejsze posty według liczby odsłon",
       y = "Liczba odsłon", x = "")
