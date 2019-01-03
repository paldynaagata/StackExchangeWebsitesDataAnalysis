install.packages("dplyr")
library(dplyr)
options(stringsAsFactors=TRUE)

#--------------------------------scieżki - Marika ----------------
fitness<-"C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/serwisy_csv/fitness_stackexchange_com"
interpersonal<-"C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/serwisy_csv/interpersonal_stackexchange_com"
worldbuilding<-"C:/Users/hp/Desktop/STUDIA/PAD-R/pd3_PADR_PADPy/serwisy_csv/worldbuilding_stackexchange_com"

###---------------------Proste informacje z Tags---------------------------
TagsF<-read.csv(paste(fitness, "/Tags.csv", sep=""), header=TRUE)
TagsI<-read.csv(paste(interpersonal, "/Tags.csv", sep=""), header=TRUE)
TagsW<-read.csv(paste(worldbuilding, "/Tags.csv", sep=""), header=TRUE)

#------------------------------ 10 najczestszych Tagów w fitnessie---------------
TopTenTagsF<-TagsF%>%arrange(desc(Count))%>%select(TagName, Count)%>%head(10)%>%
                                                  as.data.frame()
                                                                
#-------------------------------10 najczestszych Tagów w Interpersonal------

TopTenTagsI<-TagsI%>%arrange(desc(Count))%>%select(TagName, Count)%>%
                              head(10)%>%as.data.frame() 
                                                        
#------------------------------10 najczestszych Tagów w worldbuilding-------------

TopTenTagsW<-TagsW%>%arrange(desc(Count))%>%select(TagName, Count)%>%
                                  head(10)%>%as.data.frame() 


#-------- Badges... tutaj są srednio ciekawe rzeczy :/-------------                                                           
BadgesF<-read.csv(paste(fitness, "/Badges.csv", sep=""), header=TRUE)

#-----------Comments--------------------------------------------------

CommentsF<-read.csv(paste(fitness, "/Comments.csv", sep=""), header=TRUE)
CommentsI<-read.csv(paste(interpersonal, "/Comments.csv", sep=""), header=TRUE)
CommentsW<-read.csv(paste(worldbuilding, "/Comments.csv", sep=""), header=TRUE)

#---------Posts-------------------------------

PostsF<-read.csv(paste(fitness, "/Posts.csv", sep=""), header=TRUE)
PostsI<-read.csv(paste(interpersonal, "/Posts.csv", sep=""), header=TRUE)
PostsW<-read.csv(paste(worldbuilding, "/Posts.csv", sep=""), header=TRUE)
#-------------Post który miał najwiecej wyswietlen w Fitnessie(hehe)

TopViewsPostF<-PostsF%>%arrange(desc(ViewCount))%>%
  select(Id, PostTypeId, AcceptedAnswerId, CreationDate, ViewCount, Body, Title, CommentCount)%>%
  head(5)
TopViewsPostF$Title
#------tresc pytania------

print(TopViewsPostF$Body)
PostsF%>%filter(Id==TopViewsPostF$AcceptedAnswerId)%>%select(Body) # tresc odpowiedzi, kt�r� wybra� autor pytania

# wszystkie odpowiedzi do tego postu
x<-PostsF%>%filter(ParentId==TopViewsPostF$Id)

#-------------Post który miał najwiecej wyswietlen w Interpersonal

TopViewsPostI<-PostsI%>%arrange(desc(ViewCount))%>%
  select(Id, PostTypeId, AcceptedAnswerId, CreationDate, ViewCount, Body, Title, CommentCount, Tags)%>%
  head(5)

TopViewsPostI$Tags



CommentsF%>%arrange(CreationDate)
head(CommentsF)

names(PostsF)
