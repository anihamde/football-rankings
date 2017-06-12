library(plyr)
library(xgboost)

setwd("/Users/Anirudh/RWD/")
#set working directory to relevant file path

#This file is an analysis and ranking for defensive linemen

college<-read.csv(file="collegeCFFdata.csv")
college<-college[
  college$pff_GAMEPOSITION=="DE"|
    college$pff_GAMEPOSITION=="DL"|
    college$pff_GAMEPOSITION=="DLE"|
    college$pff_GAMEPOSITION=="DRE"|
    college$pff_GAMEPOSITION=="DT"|
    college$pff_GAMEPOSITION=="LE"|
    college$pff_GAMEPOSITION=="NT"|
    college$pff_GAMEPOSITION=="RE",]

pro<-read.csv(file="proPFFdata.csv")
pro<-pro[
  pro$pff_GAMEPOSITION=="DE"|
    pro$pff_GAMEPOSITION=="DL"|
    pro$pff_GAMEPOSITION=="DLE"|
    pro$pff_GAMEPOSITION=="DRE"|
    pro$pff_GAMEPOSITION=="DT"|
    pro$pff_GAMEPOSITION=="LE"|
    pro$pff_GAMEPOSITION=="NT"|
    pro$pff_GAMEPOSITION=="RE",]

collegestats05<-read.csv(file="player-game-statistics_2005.csv")
collegestats06<-read.csv(file="player-game-statistics_2006.csv")
collegestats07<-read.csv(file="player-game-statistics_2007.csv")
collegestats08<-read.csv(file="player-game-statistics_2008.csv")
collegestats09<-read.csv(file="player-game-statistics_2009.csv")
collegestats10<-read.csv(file="player-game-statistics_2010.csv")
collegestats11<-read.csv(file="player-game-statistics_2011.csv")
collegestats12<-read.csv(file="player-game-statistics_2012.csv")
collegestats13<-read.csv(file="player-game-statistics_2013.csv")

for(i in 5:13){ 
  
  if(i<10){
    nam <- paste("allstats_0", i, sep = "")
    nam_d <- paste(("collegestats0"),i,sep="")
  } else {
    nam <- paste("allstats_", i, sep = "")
    nam_d <- paste(("collegestats"),i,sep="")
  }
  
  assign(nam, aggregate(cbind(Rush.Att,Rush.Yard,Rush.TD,   
                              Pass.Att,Pass.Comp,Pass.Yard,Pass.TD,Pass.Int,Pass.Conv,
                              Rec,Rec.Yards,Rec.TD,
                              Kickoff.Ret,Kickoff.Ret.Yard,Kickoff.Ret.TD,
                              Punt.Ret,Punt.Ret.Yard,Punt.Ret.TD,
                              Fum.Ret,Fum.Ret.Yard,Fum.Ret.TD,
                              Int.Ret,Int.Ret.Yard,Int.Ret.TD,
                              Misc.Ret,Misc.Ret.Yard,Misc.Ret.TD,
                              Field.Goal.Att,Field.Goal.Made,
                              Off.XP.Kick.Att,Off.XP.Kick.Made,Off.2XP.Att,Off.2XP.Made,Def.2XP.Made,
                              Safety,Points,Punt,Punt.Yard,
                              Kickoff,Kickoff.Yard,Kickoff.Touchback,Kickoff.Out.Of.Bounds,Kickoff.Onside,
                              Fumble,Fumble.Lost,Tackle.Solo,Tackle.Assist,Tackle.For.Loss,Tackle.For.Loss.Yard,
                              Sack,Sack.Yard,
                              QB.Hurry,Fumble.Forced,Pass.Broken.Up,Kick.Punt.Blocked
  )
  ~ Player.Code,data=get(nam_d),FUN=sum,na.rm=T
  )
  )
}



collegenames05<-read.csv(file="theplayer_2005.csv")
collegenames06<-read.csv(file="theplayer_2006.csv")
collegenames07<-read.csv(file="theplayer_2007.csv")
collegenames08<-read.csv(file="theplayer_2008.csv")
collegenames09<-read.csv(file="theplayer_2009.csv")
collegenames10<-read.csv(file="theplayer_2010.csv")
collegenames11<-read.csv(file="theplayer_2011.csv")
collegenames12<-read.csv(file="theplayer_2012.csv")
collegenames13<-read.csv(file="theplayer_2013.csv")

collegenames05$PLAYERNAME<-toupper(paste(collegenames05$First.Name,collegenames05$Last.Name))
collegenames06$PLAYERNAME<-toupper(paste(collegenames06$First.Name,collegenames06$Last.Name))
collegenames07$PLAYERNAME<-toupper(paste(collegenames07$First.Name,collegenames07$Last.Name))
collegenames08$PLAYERNAME<-toupper(paste(collegenames08$First.Name,collegenames08$Last.Name))
collegenames09$PLAYERNAME<-toupper(paste(collegenames09$First.Name,collegenames09$Last.Name))
collegenames10$PLAYERNAME<-toupper(paste(collegenames10$First.Name,collegenames10$Last.Name))
collegenames11$PLAYERNAME<-toupper(paste(collegenames11$First.Name,collegenames11$Last.Name))
collegenames12$PLAYERNAME<-toupper(paste(collegenames12$First.Name,collegenames12$Last.Name))
collegenames13$PLAYERNAME<-toupper(paste(collegenames13$First.Name,collegenames13$Last.Name))



match(pro$pff_PLAYERNAME,collegenames05$PLAYERNAME)
x05<-collegenames05[match(pro$pff_PLAYERNAME,collegenames05$PLAYERNAME),]
mast05<-match(x05$Player.Code,collegestats05$Player.Code)
x05<-cbind(x05,collegestats05[mast05,])
colnames(x05)<-paste(colnames(x05),"05",sep = "")

x06<-collegenames06[match(pro$pff_PLAYERNAME,collegenames06$PLAYERNAME),]
mast06<-match(x06$Player.Code,collegestats06$Player.Code)
x06<-cbind(x06,collegestats06[mast06,])
colnames(x06)<-paste(colnames(x06),"06",sep = "")

x07<-collegenames07[match(pro$pff_PLAYERNAME,collegenames07$PLAYERNAME),]
mast07<-match(x07$Player.Code,collegestats07$Player.Code)
x07<-cbind(x07,collegestats05[mast07,])
colnames(x07)<-paste(colnames(x07),"07",sep = "")

x08<-collegenames08[match(pro$pff_PLAYERNAME,collegenames08$PLAYERNAME),]
mast08<-match(x08$Player.Code,collegestats08$Player.Code)
x08<-cbind(x08,collegestats05[mast08,])
colnames(x08)<-paste(colnames(x08),"08",sep = "")

x09<-collegenames09[match(pro$pff_PLAYERNAME,collegenames09$PLAYERNAME),]
mast09<-match(x09$Player.Code,collegestats09$Player.Code)
x09<-cbind(x09,collegestats09[mast09,])
colnames(x09)<-paste(colnames(x09),"09",sep = "")

x10<-collegenames10[match(pro$pff_PLAYERNAME,collegenames10$PLAYERNAME),]
mast10<-match(x10$Player.Code,collegestats10$Player.Code)
x10<-cbind(x10,collegestats10[mast10,])
colnames(x10)<-paste(colnames(x10),"10",sep = "")

x11<-collegenames11[match(pro$pff_PLAYERNAME,collegenames11$PLAYERNAME),]
mast11<-match(x11$Player.Code,collegestats11$Player.Code)
x11<-cbind(x11,collegestats11[mast11,])
colnames(x11)<-paste(colnames(x11),"11",sep = "")

x12<-collegenames12[match(pro$pff_PLAYERNAME,collegenames12$PLAYERNAME),]
mast12<-match(x12$Player.Code,collegestats12$Player.Code)
x12<-cbind(x12,collegestats12[mast12,])
colnames(x12)<-paste(colnames(x12),"12",sep = "")

x13<-collegenames13[match(pro$pff_PLAYERNAME,collegenames13$PLAYERNAME),]
mast13<-match(x13$Player.Code,collegestats13$Player.Code)
x13<-cbind(x13,collegestats13[mast13,])
#x13<-x13[!is.na(x13$PLAYERNAME),]
colnames(x13)<-paste(colnames(x13),"13",sep = "")

xcurr<-college[match(pro$pff_PLAYERNAME,college$pff_PLAYERNAME),]
colnames(xcurr)<-paste(colnames(xcurr),"curr",sep="")




finalpro<-cbind(pro,x05,x06,x07,x08,x09,x10,x11,x12,x13,xcurr)
finalpro$pff_PLAYERNAME

#Looking at colSums for grade- and stat-related columns, 
#we can infer which grades and stats are important for D linemen for evaluation
for(i in 12:30){
  hist(finalpro[,i],breaks=10,main=paste("Histogram of feature ",i,": ",colnames(finalpro)[i],sep=""))
}

#determined four significant grade areas: 
#18 (pff_PASSRUSH), 19 (pff_COVERAGE), 20 (pff_RUNDEFENSE), 21 (pff_PENALTY)
#4 weights: c1, c2, c3, c4 with the constraint that c1+c2+c3+c4 = 1
c1<-0.25
c2<-0.25
c3<-0.25
c4<-0.25

#standardize and weight
finalpro$stand_z_18<-(finalpro[,18]-mean(finalpro[,18],na.rm=T))/sd(finalpro[,18],na.rm=T)
finalpro$stand_z_19<-(finalpro[,19]-mean(finalpro[,19],na.rm=T))/sd(finalpro[,19],na.rm=T)
finalpro$stand_z_20<-(finalpro[,20]-mean(finalpro[,20],na.rm=T))/sd(finalpro[,20],na.rm=T)
finalpro$stand_z_21<-(finalpro[,21]-mean(finalpro[,21],na.rm=T))/sd(finalpro[,21],na.rm=T)

finalpro$stand_01_18<-(finalpro[,18]-min(finalpro[,18],na.rm=T))/
  (max(finalpro[,18],na.rm=T)-min(finalpro[,18],na.rm=T))
finalpro$stand_01_19<-(finalpro[,19]-min(finalpro[,19],na.rm=T))/
  (max(finalpro[,19],na.rm=T)-min(finalpro[,19],na.rm=T))
finalpro$stand_01_20<-(finalpro[,20]-min(finalpro[,20],na.rm=T))/
  (max(finalpro[,20],na.rm=T)-min(finalpro[,20],na.rm=T))
finalpro$stand_01_21<-(finalpro[,21]-min(finalpro[,21],na.rm=T))/
  (max(finalpro[,21],na.rm=T)-min(finalpro[,21],na.rm=T))

finalpro$grade_z_label<-(c1*finalpro$stand_z_18
                        +c2*finalpro$stand_z_19
                        +c3*finalpro$stand_z_20
                        +c4*finalpro$stand_z_21)/(c1+c2+c3+c4)

finalpro$grade_01_label<-(c1*finalpro$stand_01_18
                          +c2*finalpro$stand_01_19
                          +c3*finalpro$stand_01_20
                          +c4*finalpro$stand_01_21)/(c1+c2+c3+c4)

finalpro[order(finalpro$grade_z_label,decreasing = T),]$pff_PLAYERNAME

colSums(finalpro[,c(12:30)],na.rm=T)

design <- finalpro[,c(9,54:55,63:118,126:127,135:190,198:199,207:262,270:271,279:334,342:343,
  351:406,414:415,423:478,486:487,495:550,558:559,567:622,630:631,639:694,707:725)]
design <- aggregate(.~pff_PLAYERNAME,data=design,FUN=function(x) c(mean(x,na.rm=T)),na.action=na.pass)
  
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}

asdfsz<-design[,c("Tackle.Solo05","Tackle.Solo06","Tackle.Solo07","Tackle.Solo08","Tackle.Solo09",
          "Tackle.Solo10","Tackle.Solo11","Tackle.Solo12","Tackle.Solo13","pff_PASSRUSHcurr")]

design$nyrsout <- 0
design$nyrscoll <- 0
asdfsz$dummy <- 0

for(i in 1:nrow(asdfsz)){
  design$nyrscoll[i] <- sum(!is.nan(asdfsz[i,1:10]))
}

for(i in 1:nrow(asdfsz)){
  colno<-1
  while(is.nan(asdfsz[i,colno])){
    colno<-colno+1
  }
  
  if(colno>10){
    design$nyrsout[i]<-11
  }else{
    while(!is.nan(asdfsz[i,colno]) && colno>11){
      colno<-colno+1
    }
    
    design$nyrsout[i]<-(11-colno)
  }
}

design$nyrscoll[design$nyrscoll==0] <- 0.001
design[is.nan(design)] <- 0

newdes<-design[,1]
newdes<-cbind(newdes,design[,2:59]+design[,60:117]+design[,118:175]+design[,176:233]+design[,234:291]
              +design[,292:349]+design[,350:407]+design[,408:465]+design[,466:523])
colnames(newdes)<-substr(colnames(newdes),1,nchar(colnames(newdes))-2)
newdes<-cbind(newdes,design[,524:542])
colnames(newdes)[1]<-"PLAYERNAME"
newdes[,2:78]<-newdes[,2:78]/design$nyrscoll
newdes<-cbind(newdes,design$nyrsout,design$nyrscoll)
colnames(newdes)[79:80]<-c("nyrsout","nyrscoll")



label <- finalpro[,c(9,18,19,20,21)]
label <- aggregate(.~pff_PLAYERNAME,data=label,
          FUN=function(x) c(mean(x,na.rm=T)),na.action=na.pass)

for(i in 2:5){
  hist(label[,i],breaks=10,main=paste("Histogram of feature ",i,": ",colnames(label)[i],sep=""))
}

#determined four significant grade areas: 
#18 (pff_PASSRUSH), 19 (pff_COVERAGE), 20 (pff_RUNDEFENSE), 21 (pff_PENALTY)
#4 weights: c1, c2, c3, c4 with the constraint that c1+c2+c3+c4 = 1
c1<-0.40
c2<-0.10
c3<-0.40
c4<-0.10

#standardize and weight
label$stand_z_18<-(label[,2]-mean(label[,2],na.rm=T))/sd(label[,2],na.rm=T)
label$stand_z_19<-(label[,3]-mean(label[,3],na.rm=T))/sd(label[,3],na.rm=T)
label$stand_z_20<-(label[,4]-mean(label[,4],na.rm=T))/sd(label[,4],na.rm=T)
label$stand_z_21<-(label[,5]-mean(label[,5],na.rm=T))/sd(label[,5],na.rm=T)

label$stand_01_18<-(label[,2]-min(label[,2],na.rm=T))/
  (max(label[,2],na.rm=T)-min(label[,2],na.rm=T))
label$stand_01_19<-(label[,3]-min(label[,3],na.rm=T))/
  (max(label[,3],na.rm=T)-min(label[,3],na.rm=T))
label$stand_01_20<-(label[,4]-min(label[,4],na.rm=T))/
  (max(label[,4],na.rm=T)-min(label[,4],na.rm=T))
label$stand_01_21<-(label[,5]-min(label[,5],na.rm=T))/
  (max(label[,5],na.rm=T)-min(label[,5],na.rm=T))

label$grade_z_label<-(c1*label$stand_z_18
                         +c2*label$stand_z_19
                         +c3*label$stand_z_20
                         +c4*label$stand_z_21)/(c1+c2+c3+c4)

label$grade_01_label<-(c1*label$stand_01_18
                          +c2*label$stand_01_19
                          +c3*label$stand_01_20
                          +c4*label$stand_01_21)/(c1+c2+c3+c4)



###TRAIN-lm
fit0_01<-lm(as.formula(paste("label$grade_01_label~",paste(colnames(newdes)[2:80],collapse="+"),sep="")),data=newdes)
fit0_z<-lm(as.formula(paste("label$grade_z_label~",paste(colnames(newdes)[2:80],collapse="+"),sep="")),data=newdes)
fit1_01<-lm(as.formula(paste("label$grade_01_label~",paste(colnames(design)[2:544],collapse="+"),sep="")),data=design)
fit1_z<-lm(as.formula(paste("label$grade_z_label~",paste(colnames(design)[2:544],collapse="+"),sep="")),data=design)






###RANK-lm
college_des<-college[,c(10,13:31)]
college_des <- aggregate(.~pff_PLAYERNAME,data=college_des,FUN=function(x) c(mean(x,na.rm=T)),na.action=na.pass)
college_des[is.nan(college_des)] <- 0

zeros1<-matrix(0,nrow=nrow(college_des),ncol=522)
zeros2<-matrix(0,nrow=nrow(college_des),ncol=2)
zeros3<-matrix(0,nrow=nrow(college_des),ncol=58)

college_des_0_fin <- cbind(college_des[,1],zeros3,college_des[,2:20],zeros2)
college_des_1_fin <- cbind(college_des[,1],zeros1,college_des[,2:20],zeros2)

colnames(college_des_0_fin)<-colnames(newdes)
colnames(college_des_1_fin)<-colnames(design)

predict_0_01 <- predict.lm(fit0_01,college_des_0_fin[,2:80])
predict_0_z <- predict.lm(fit0_z,college_des_0_fin[,2:80])
predict_1_01 <- predict.lm(fit1_01,college_des_1_fin[,2:544])
predict_1_z <- predict.lm(fit1_z,college_des_1_fin[,2:544])

ranknames_1<-college_des[order(predict_0_01,decreasing = TRUE),]$pff_PLAYERNAME
ranknames_2<-college_des[order(predict_0_z,decreasing = TRUE),]$pff_PLAYERNAME
ranknames_3<-college_des[order(predict_1_01,decreasing = TRUE),]$pff_PLAYERNAME
ranknames_4<-college_des[order(predict_1_z,decreasing = TRUE),]$pff_PLAYERNAME

write.csv(data.frame(Rank1=ranknames_1,Rank2=ranknames_2,Rank3=ranknames_3,Rank4=ranknames_4),file="Rankings_DL.csv")
