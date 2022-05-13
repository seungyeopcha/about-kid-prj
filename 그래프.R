setwd("C:/학대아동")

hj_18<-read.csv("cp_18_수정.csv")

hj_18

v<-hj_18[,as.numeric(c(1,2,22,43,41,16,12,44))]


for(i in c(3:7)){
  v[i]=v[i]/v$pop*1000
}

v<-v[,-2]

v$level<-as.factor(v$level)

v_1<-v[v$level==1,]
v_2<-v[v$level==2,]

#write.csv(v, file = "v1.csv")

v_1<-v_1[,-7]
v_2<-v_2[,-7]

library('ggplot2')

ggplot(v, aes(name, ef9, fill=level))+
  geom_bar(stat='identity', position = 'dodge')+
  theme(axis.text.x=element_text(angle=45))+
  scale_fill_manual(values = c('grey','orange'),
                    labels = c("일반","위험"))


#g
gr<-read.csv("그래프.csv", header = T,sep=",")
gr


ggplot(gr, aes(name, ef13, fill=level))+
  geom_bar(stat='identity', position = 'dodge')+
  theme(axis.text.x=element_text(angle=45))+
  scale_fill_manual(values = c('grey','lightgreen'),
                    labels = c("일반","위험"))



gr$level<-as.factor(gr$level)


#####중앙값
med<-read.csv("med.csv",sep=",",header = T)
med
class(med$경찰소방서)
barplot(med)


mfrow=par
ggplot(med, aes(x=level,y=단독주택, fill=level))+
  geom_bar(stat='identity', position = 'dodge')+
  theme(axis.text.x=element_text(angle=45))+
  scale_fill_manual(values = c('lightgreen','grey'),
                    labels = c("일반","위험"))+
  ggtitle("단독주택")+
  theme(title = element_text(size=15))
  

#####표준화
ff<-cbind(cp_18_pop$ef7,cp_18_pop$ef9,cp_18_pop$ef13,cp_18_pop$ef19,cp_18_pop$ef38,cp_18_pop$wf,cp_18_pop$nn_pop)
ff<-data.frame(ff)
ff

ff$level<-ifelse(ff$V7<3.510030,1,2)
ff
ff<-ff[,-7]
ff

ff_1<-ff[ff$level==1,]
ff_1
ff_2<-ff[ff$level==2,]
ff_2


a<-c(ff_1 %>%summarize(ef7 = median(ef7))
,ff_1 %>%summarize(ef9 = median(ef9))
,ff_1 %>%summarize(ef13 = median(ef13))
,ff_1 %>%summarize(ef19= median(ef19))
,ff_1 %>%summarize(ef38= median(ef38))
,ff_1 %>%summarize(wf= median(wf))
)
a
b<-c(ff_2 %>%summarize(ef7 = median(ef7))
     ,ff_2 %>%summarize(ef9 = median(ef9))
     ,ff_2 %>%summarize(ef13 = median(ef13))
     ,ff_2 %>%summarize(ef19= median(ef19))
     ,ff_2 %>%summarize(ef38= median(ef38))
     ,ff_2 %>%summarize(wf= median(wf))
)
b



fd<-as.data.frame(rbind(a,b))
level<-c(1,2)
fd<-cbind(fd,level)
class(fd)
write.csv(fd, file = "fd.csv")


c<-c("ef7","ef9","ef13","ef19","ef38","wf","level")
fd<-rbind(c,fd)
fd
fs<-t(fd)
fs
fs<-fs[-7,]
fs

ggplot(fs, aes(1,ef7, fill=level))+
  geom_bar(stat='identity', position = 'dodge')+
  theme(axis.text.x=element_text(angle=45))+
  scale_fill_manual(values = c('grey','lightgreen'),
                    labels = c("일반","위험"))
