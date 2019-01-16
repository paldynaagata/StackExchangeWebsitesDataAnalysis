#Pytanie: Czy liczba wyswietlen idzie w parze z liczbÄ… komentarzy pod pytaniem?

# Fitness
yF<-PostsF%>%filter(PostTypeId==1)%>%select(Id, ParentId, CommentCount)# Pytania
xF<-PostsF%>%filter(PostTypeId==2)%>%select(Id, CommentCount, ParentId)#Odpowiedzi
zF<-inner_join(yF,xF, by=c("Id"="ParentId"))
uF<-zF%>%group_by(Id)%>%summarise(TotalComCnt=sum(CommentCount.y), n=length(Id))# 

QuestionsComCntF<-inner_join(uF, zF, by="Id")%>%mutate(TotalCommentCnt=TotalComCnt+CommentCount.x)%>%
  select(Id, TotalCommentCnt, n)%>%distinct()%>%mutate(TotalComment=TotalCommentCnt+n)#ile kazde pytanie miaÅ‚o w sumie komentarzy+odpowiedzi

QuestionsCommViewF<-inner_join(QuestionsComCntF, PostsF, by="Id")%>%
  select(Id, TotalComment, ViewCount)# Pytanie, dyskusja, liczba odsÅ‚on

write.csv(QuestionsCommViewF, file = "C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/dane_posrednie/q3/QuestionsCommViewF.csv", row.names = FALSE)

QuestionsCommViewF %>%
  ggplot() + 
  geom_smooth(aes( ViewCount,TotalComment), se=FALSE) +
  geom_point(aes(ViewCount,TotalComment), size = 0.5)+labs(title = "Liczba ods³on a liczba komentarzy",
                                                           x = "Liczba ods³on (ViewCount)",
                                                           y = "Liczba komentarzy (TotalComment)") +
  theme(axis.title = element_text(size = 16))

# plot <- QuestionsCommViewF %>%
#   ggplot() + 
#   geom_smooth(aes( ViewCount,TotalComment), se=FALSE) +
#   geom_point(aes(ViewCount,TotalComment), size = 0.5)+labs(title = "Liczba ods³on a liczba komentarzy",
#                                                            x = "Liczba ods³on (ViewCount)",
#                                                            y = "Liczba komentarzy (TotalComment)")
# 
# maxF<-QuestionsCommViewF%>%filter(TotalComment==max(TotalComment))
# PostsF%>%filter(Id==maxF$Id)%>%select(Title, Body, ViewCount)

#Interpersonal------------------------------

yI<-PostsI%>%filter(PostTypeId==1)%>%select(Id, ParentId, CommentCount)# Pytania
xI<-PostsI%>%filter(PostTypeId==2)%>%select(Id, CommentCount, ParentId)#Odpowiedzi
zI<-inner_join(yI,xI, by=c("Id"="ParentId"))
uI<-zI%>%group_by(Id)%>%summarise(TotalComCnt=sum(CommentCount.y), n=length(Id))# 
head(QuestionsComCntI)
head(QuestionsComCntF)
QuestionsComCntI<-inner_join(uI, zI, by="Id")%>%mutate(TotalCommentCnt=TotalComCnt+CommentCount.x)%>%
  select(Id, TotalCommentCnt, n)%>%distinct()%>%mutate(TotalComment=TotalCommentCnt+n)
#ile kazde pytanie miaÅ‚o w sumie komentarzy+odpowiedzi
QuestionsCommViewI<-inner_join(QuestionsComCntI, PostsI, by="Id")%>%
select(Id, TotalComment, ViewCount)# Pytanie, dyskusja, liczba odsÅ‚on

write.csv(QuestionsCommViewI, file = "C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/dane_posrednie/q3/QuestionsCommViewI.csv", row.names = FALSE)

QuestionsCommViewI %>%
  ggplot() +
  geom_smooth(aes( ViewCount,TotalComment), se=FALSE) +
  geom_point(aes(ViewCount,TotalComment), size = 0.5)+labs(title = "Liczba ods³on a liczba komentarzy",
                                                           x = "Liczba ods³on (ViewCount)",
                                                           y = "Liczba komentarzy (TotalComment)") +
  theme(axis.title = element_text(size = 16))

# plot <- QuestionsCommViewI %>%
#   ggplot() +
#   geom_smooth(aes( ViewCount,TotalComment), se=FALSE) +
#   geom_point(aes(ViewCount,TotalComment), size = 0.5)+labs(title = "Liczba ods³on a liczba komentarzy",
#                                                            x = "Liczba ods³on (ViewCount)",
#                                                            y = "Liczba komentarzy (TotalComment)")
# 
# maxI<-QuestionsCommViewI%>%filter(TotalComment==max(TotalComment))
# PostsI%>%filter(Id==maxI$Id)%>%select(Title, Body, ViewCount)

#Worldbuilding

yW<-PostsW%>%filter(PostTypeId==1)%>%select(Id, ParentId, CommentCount)# Pytania
xW<-PostsW%>%filter(PostTypeId==2)%>%select(Id, CommentCount, ParentId)#Odpowiedzi
zW<-inner_join(yW,xW, by=c("Id"="ParentId"))
uW<-zW%>%group_by(Id)%>%summarise(TotalComCnt=sum(CommentCount.y), n=length(Id))# 
head(QuestionsComCntI)
head(QuestionsComCntF)
QuestionsComCntW<-inner_join(uI, zI, by="Id")%>%mutate(TotalCommentCnt=TotalComCnt+CommentCount.x)%>%
  select(Id, TotalCommentCnt, n)%>%distinct()%>%mutate(TotalComment=TotalCommentCnt+n)
#ile kazde pytanie miaÅ‚o w sumie komentarzy+odpowiedzi
QuestionsCommViewW<-inner_join(QuestionsComCntW, PostsW, by="Id")%>%
  select(Id, TotalComment, ViewCount)# Pytanie, dyskusja, liczba odsÅ‚on

write.csv(QuestionsCommViewW, file = "C:/Users/aga71/OneDrive/studia/SEMESTRY/MGR/I/inne/pd3_PADR_PADPy/PADR/dane_posrednie/q3/QuestionsCommViewW.csv", row.names = FALSE)

QuestionsCommViewW %>%
  ggplot() +
  geom_smooth(aes( ViewCount,TotalComment), se=FALSE) +
  geom_point(aes(ViewCount,TotalComment), size = 0.5)+labs(title = "Liczba ods³on a liczba komentarzy",
                                                           x = "Liczba ods³on (ViewCount)",
                                                           y = "Liczba komentarzy (TotalComment)") +
  theme(axis.title = element_text(size = 16))

# plot <- QuestionsCommViewW %>%
#   ggplot() +
#   geom_smooth(aes( ViewCount,TotalComment), se=FALSE) +
#   geom_point(aes(ViewCount,TotalComment), size = 0.5)+labs(title = "Liczba ods³on a liczba komentarzy",
#                                                            x = "Liczba ods³on (ViewCount)",
#                                                            y = "Liczba komentarzy (TotalComment)")
# 
# maxW<-QuestionsCommViewW%>%filter(TotalComment==max(TotalComment))
# PostsW%>%filter(Id==maxW$Id)%>%select(Title, Body, ViewCount)


