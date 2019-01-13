#Pytanie: Czy liczba wyswietlen idzie w parze z liczbą komentarzy pod pytaniem?

# Fitness
yF<-PostsF%>%filter(PostTypeId==1)%>%select(Id, ParentId, CommentCount)# Pytania
xF<-PostsF%>%filter(PostTypeId==2)%>%select(Id, CommentCount, ParentId)#Odpowiedzi
zF<-inner_join(y,x, by=c("Id"="ParentId"))
uF<-z%>%group_by(Id)%>%summarise(TotalComCnt=sum(CommentCount.y), n=length(Id))# 

QuestionsComCntF<-inner_join(u, z, by="Id")%>%mutate(TotalCommentCnt=TotalComCnt+CommentCount.x)%>%
  select(Id, TotalCommentCnt, n)%>%distinct()%>%mutate(TotalComment=TotalCommentCnt+n)#ile kazde pytanie miało w sumie komentarzy+odpowiedzi
QuestionsCommViewF<-inner_join(QuestionsComCnt, PostsF, by="Id")%>%
  select(Id, TotalComment, ViewCount)# Pytanie, dyskusja, liczba odsłon


plot <- QuestionsCommViewF %>%
  ggplot() +
  geom_smooth(aes( ViewCount,TotalComment), se=FALSE) +
  geom_point(aes(ViewCount,TotalComment), size = 0.5)+labs(title = "Liczba odsłon a liczba komentarzy",
                                                           x = "Liczba odsłon (ViewCount)",
                                                           y = "Liczba komentarzy (TotalComment)")

maxF<-QuestionsCommViewF%>%filter(TotalComment==max(TotalComment))
PostsF%>%filter(Id==maxF$Id)%>%select(Title, Body, ViewCount)

#Interpersonal------------------------------

yI<-PostsI%>%filter(PostTypeId==1)%>%select(Id, ParentId, CommentCount)# Pytania
xI<-PostsI%>%filter(PostTypeId==2)%>%select(Id, CommentCount, ParentId)#Odpowiedzi
zI<-inner_join(yI,xI, by=c("Id"="ParentId"))
uI<-zI%>%group_by(Id)%>%summarise(TotalComCnt=sum(CommentCount.y), n=length(Id))# 
head(QuestionsComCntI)
head(QuestionsComCntF)
QuestionsComCntI<-inner_join(uI, zI, by="Id")%>%mutate(TotalCommentCnt=TotalComCnt+CommentCount.x)%>%
  select(Id, TotalCommentCnt, n)%>%distinct()%>%mutate(TotalComment=TotalCommentCnt+n)
#ile kazde pytanie miało w sumie komentarzy+odpowiedzi
QuestionsCommViewI<-inner_join(QuestionsComCntI, PostsI, by="Id")%>%
select(Id, TotalComment, ViewCount)# Pytanie, dyskusja, liczba odsłon


plot <- QuestionsCommViewI %>%
  ggplot() +
  geom_smooth(aes( ViewCount,TotalComment), se=FALSE) +
  geom_point(aes(ViewCount,TotalComment), size = 0.5)+labs(title = "Liczba odsłon a liczba komentarzy",
                                                           x = "Liczba odsłon (ViewCount)",
                                                           y = "Liczba komentarzy (TotalComment)")

maxI<-QuestionsCommViewI%>%filter(TotalComment==max(TotalComment))
PostsI%>%filter(Id==max$Id)%>%select(Title, Body, ViewCount)

#Worldbuilding

yW<-PostsW%>%filter(PostTypeId==1)%>%select(Id, ParentId, CommentCount)# Pytania
xW<-PostsW%>%filter(PostTypeId==2)%>%select(Id, CommentCount, ParentId)#Odpowiedzi
zW<-inner_join(yW,xW, by=c("Id"="ParentId"))
uW<-zW%>%group_by(Id)%>%summarise(TotalComCnt=sum(CommentCount.y), n=length(Id))# 
head(QuestionsComCntI)
head(QuestionsComCntF)
QuestionsComCntW<-inner_join(uI, zI, by="Id")%>%mutate(TotalCommentCnt=TotalComCnt+CommentCount.x)%>%
  select(Id, TotalCommentCnt, n)%>%distinct()%>%mutate(TotalComment=TotalCommentCnt+n)
#ile kazde pytanie miało w sumie komentarzy+odpowiedzi
QuestionsCommViewW<-inner_join(QuestionsComCntW, PostsW, by="Id")%>%
  select(Id, TotalComment, ViewCount)# Pytanie, dyskusja, liczba odsłon


plot <- QuestionsCommViewW %>%
  ggplot() +
  geom_smooth(aes( ViewCount,TotalComment), se=FALSE) +
  geom_point(aes(ViewCount,TotalComment), size = 0.5)+labs(title = "Liczba odsłon a liczba komentarzy",
                                                           x = "Liczba odsłon (ViewCount)",
                                                           y = "Liczba komentarzy (TotalComment)")

maxW<-QuestionsCommViewW%>%filter(TotalComment==max(TotalComment))
PostsW%>%filter(Id==maxW$Id)%>%select(Title, Body, ViewCount)


