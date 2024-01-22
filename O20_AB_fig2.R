####Figure 2####
install.packages("patchwork")

library(plyr)
library(cowplot)
library(nlme)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)
library(ggbreak)
library(patchwork)
library(ggpubr)

####Import####
LD = read.csv("O20AB_LD_raw.csv")
StarvM = read.csv("O20AB_SRm_raw.csv")
StarvF = read.csv("O20AB_SRf_raw.csv")
Fecund = read.csv("O20AB_F_raw.csv")
Viab = read.csv("O20AB_V_raw.csv")
Thorax = read.csv("O20AB_TL_raw.csv")

####Make Summary dfs####

######LD####
##by treatment / timepoint##
LD.sum=LD %>%
  group_by(Group) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   LD=mean(Mean.LD,na.rm = TRUE),
                   LDsd=sd(Mean.LD,na.rm = TRUE),
                   LDse=LDsd/sqrt(N),
                   LDm=mean(Mean.M.LD,na.rm = TRUE),
                   LDmsd=sd(Mean.M.LD,na.rm = TRUE),
                   LDmse=LDmsd/sqrt(N),
                   LDf=mean(Mean.F.LD,na.rm = TRUE),
                   LDfsd=sd(Mean.F.LD,na.rm = TRUE),
                   LDfse=LDfsd/sqrt(N),
  )
LD.sum=unique(LD.sum)

##by cage / timepoint / treatment##
LD.cage.sum=LD %>%
  group_by(Group2) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage=as.factor(Cage),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   LD=mean(Mean.LD,na.rm = TRUE),
                   LDsd=sd(Mean.LD,na.rm = TRUE),
                   LDse=LDsd/sqrt(N),
                   LDm=mean(Mean.M.LD,na.rm = TRUE),
                   LDmsd=sd(Mean.M.LD,na.rm = TRUE),
                   LDmse=LDmsd/sqrt(N),
                   LDf=mean(Mean.F.LD,na.rm = TRUE),
                   LDfsd=sd(Mean.F.LD,na.rm = TRUE),
                   LDfse=LDfsd/sqrt(N),
  )
LD.cage.sum=unique(LD.cage.sum)
#View(LD.cage.sum)

######SRM####
##by treatment / time point##
#View(StarvM)
SRM.sum=StarvM %>%
  group_by(Group) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   SRM=mean(Mean.SRm,na.rm = TRUE),
                   SRMsd=sd(Mean.SRm,na.rm = TRUE),
                   SRMse=SRMsd/sqrt(N),
  )
SRM.sum=unique(SRM.sum)
#View(SRM.sum)

##by cage / timepoint / treatment##
SRM.cage.sum=StarvM %>%
  group_by(Group2) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage=as.factor(Cage),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   SRM=mean(Mean.SRm,na.rm = TRUE),
                   SRMsd=sd(Mean.SRm,na.rm = TRUE),
                   SRMse=SRMsd/sqrt(N),
  )
SRM.cage.sum=unique(SRM.cage.sum)
#View(SRM.cage.sum)

######SRF####
##by treatment / time point##
#View(StarvF)
SRF.sum=StarvF %>%
  group_by(Group) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   SRF=mean(Mean.SRf,na.rm = TRUE),
                   SRFsd=sd(Mean.SRf,na.rm = TRUE),
                   SRFse=SRFsd/sqrt(N),
  )
SRF.sum=unique(SRF.sum)
#View(SRF.sum)

##by cage / timepoint / treatment##
SRF.cage.sum=StarvF %>%
  group_by(Group2) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage=as.factor(Cage),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   SRF=mean(Mean.SRf,na.rm = TRUE),
                   SRFsd=sd(Mean.SRf,na.rm = TRUE),
                   SRFse=SRFsd/sqrt(N),
  )
SRF.cage.sum=unique(SRF.cage.sum)

######Fecundity####
#View(Fecund)

Fec.sum=Fecund %>%
  group_by(Group) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   Fec=mean(Mean.eggperday,na.rm = TRUE),
                   Fecsd=sd(Mean.eggperday,na.rm = TRUE),
                   Fecse= Fecsd/sqrt(N),
                   TOTF=mean(totalfecund,na.rm = TRUE),
                   TOTFsd=sd(totalfecund,na.rm = TRUE),
                   TOTFse=TOTFsd/sqrt(N),
  )
Fec.sum=unique(Fec.sum)
#View(Fec.sum)

##by cage/ timepoint/ treatment##

Fec.cage.sum=Fecund %>%
  group_by(Group2) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage=as.factor(Cage),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   Fec=mean(Mean.eggperday,na.rm = TRUE),
                   Fecsd=sd(Mean.eggperday,na.rm = TRUE),
                   Fecse= Fecsd/sqrt(N),
                   TOTF=mean(totalfecund,na.rm = TRUE),
                   TOTFsd=sd(totalfecund,na.rm = TRUE),
                   TOTFse=TOTFsd/sqrt(N),
  )
Fec.cage.sum=unique(Fec.cage.sum)
#View(Fec.cage.sum)

######Viability####
#View(Viab)
Via.sum=Viab %>%
  group_by(Group) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   Viab=mean(Viability,na.rm = TRUE),
                   Viabsd=sd(Viability,na.rm = TRUE),
                   Viabse= Viabsd/sqrt(N),
  )
Via.sum=unique(Via.sum)
#View(Via.sum)

##by cage/ timepoint/ treatment##

Via.cage.sum=Viab %>%
  group_by(Group2) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   Cage=as.factor(Cage),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   Viab=mean(Viability,na.rm = TRUE),
                   Viabsd=sd(Viability,na.rm = TRUE),
                   Viabse= Viabsd/sqrt(N),
             
  )
Via.cage.sum=unique(Via.cage.sum)
#View(Via.cage.sum)

######Body Size######

#View(Thorax)
BS.sum=Thorax %>%
  group_by(Group) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   BS=mean(Mean.TL,na.rm = TRUE),
                   BSsd=sd(Mean.TL,na.rm = TRUE),
                   BSse= BSsd/sqrt(N),
  )
BS.sum=unique(BS.sum)
#View(BS.sum)

##by cage/ timepoint/ treatment##

BS.cage.sum=Thorax %>%
  group_by(Group2) %>%
  dplyr::summarise(Group=as.factor(Group),
                   N=n(),
                   TP=as.factor(Timepoint),
                   Pheno.Treat=as.factor(Pheno.Treat),
                   Cage=as.factor(Cage),
                   Cage.Pheno=as.factor(Cage.Pheno),
                   BS=mean(Mean.TL,na.rm = TRUE),
                   BSse=mean(SE.TL,na.rm = TRUE),
                   
  )
BS.cage.sum=unique(BS.cage.sum)
#View(BS.cage.sum)






####Merging Dfs####
##by tp/treatment##

Phenos.list <- list(LD.sum, SRM.sum, SRF.sum, Fec.sum, Via.sum, BS.sum)      

#merge all data frames together
Phenos=Phenos.list %>% reduce(full_join, by=c('Group','TP','Pheno.Treat','Cage.Pheno'))
#View(Phenos)
##Remove N columns
Phenos <- Phenos[, -c(2,15,19,23,30,34)]
Phenos <- Phenos[-17,]
View(Phenos)

write.csv (Phenos, "Phenos.csv", row.names=FALSE)
#View(Phenos) #### All phenotypic data in one table by treatment and time point

##by tp/cage/treatment##

Phenos.cage.list <- list(LD.cage.sum, SRM.cage.sum, SRF.cage.sum, Fec.cage.sum, Via.cage.sum, BS.cage.sum)      

#merge all data frames together
Phenos.cage=Phenos.cage.list %>% reduce(full_join, by=c('Group2','Group','TP','Pheno.Treat','Cage','Cage.Pheno'))
View(Phenos.cage)
write.csv (Phenos.cage, "Phenos.cage.csv", row.names=FALSE)

##Remove N columns
Phenos.cage <- Phenos.cage[, -c(3,17,21,25,32,36)]
Phenos.cage <- Phenos.cage[-c(87:88),]
View(Phenos.cage) #### All phenotypic data in one table by cage, treatment and time point


##Subsetting##
founder= subset(Phenos, Phenos$TP == 0)
founder.cage=subset(Phenos.cage, Phenos.cage$TP == 0)
#View(founder)
Pheno=subset(Phenos, Phenos$TP %in% c(1,2,3,4,5))
Pheno.cage=subset(Phenos.cage, Phenos.cage$TP %in% c(1,2,3,4,5))
#View(Pheno.cage)

Phenos24=subset(Phenos, Phenos$TP %in% c(2,4))
Phenos.cage24=subset(Phenos.cage, Phenos.cage$TP%in% c(2,4))

pheno = subset(Pheno, Pheno$Cage.Pheno %in% c("aa","bb"))
pheno.cage = subset(Phenos.cage, Phenos.cage$Cage.Pheno %in% c("aa","bb"))



#View(pheno)
aa.pheno.cage = subset(Phenos.cage, Phenos.cage$Cage.Pheno %in% c("aa"))
bb.pheno.cage = subset(Phenos.cage, Phenos.cage$Cage.Pheno %in% c("bb"))

aa.pheno24.cage = subset(Phenos.cage24, Phenos.cage24$Pheno.Treat %in% c("a"))
bb.pheno24.cage = subset(Phenos.cage24, Phenos.cage24$Pheno.Treat %in% c("b"))


####GRAPHING Fig 2 ####

##Development Time Male + Female Fig##

LD1 <- ggplot(pheno, aes(x=as.factor(TP), y=LD, group=Pheno.Treat))+
  geom_point(data = founder, aes(x=as.factor(TP), y=LD,color = Pheno.Treat), size=5)+
  geom_linerange(data=founder, aes(ymin=LD-LDse,ymax=LD+LDse, color=Pheno.Treat), size=1)+
  geom_point(aes(color=Pheno.Treat),size=5)+
  geom_line(aes(color=Pheno.Treat))+
  geom_linerange(aes(ymin=LD-LDse,ymax=LD+LDse, color=Pheno.Treat), size=1)+
  ylab("Development Time (hrs)")+
  xlab("Sample Timepoints")+
  scale_x_discrete(labels=c("Founder","TP1", "TP2", "TP3", "TP4", "TP5"))+
  scale_colour_manual(name = "Cage Treatment", 
                      breaks = c("a", "b"),
                      labels = c("Low","High"),
                      values = c("grey","black"))+
  geom_line(data=aa.pheno.cage, aes(x=as.factor(TP), y=LD, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4) + 
  geom_line(data=bb.pheno.cage, aes(x=as.factor(TP), y=LD, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4)+
  theme_cowplot(14)
LD1

## Male Development Time Fig##

LDm1 <- ggplot(pheno, aes(x=as.factor(TP), y=LDm, group=Pheno.Treat))+
  geom_point(data = founder, aes(x=as.factor(TP), y=LDm,color = Pheno.Treat), size=5)+
  geom_linerange(data=founder, aes(ymin=LDm-LDmse,ymax=LDm+LDmse, color=Pheno.Treat), size=1)+
  geom_point(aes(color=Pheno.Treat),size=5)+
  geom_line(aes(color=Pheno.Treat))+
  geom_linerange(aes(ymin=LDm-LDmse,ymax=LDm+LDmse, color=Pheno.Treat), size=1)+
  ylab("Male Development Time (hrs)")+
  xlab("Sample Timepoints")+
  scale_x_discrete(labels=c("Founder","TP1", "TP2", "TP3", "TP4", "TP5"))+
  scale_colour_manual(name = "Cage Treatment", 
                      breaks = c("a", "b"),
                      labels = c("Low","High"),
                      values = c("grey","black"))+
  geom_line(data=aa.pheno.cage, aes(x=as.factor(TP), y=LDm, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4) + 
  geom_line(data=bb.pheno.cage, aes(x=as.factor(TP), y=LDm, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4)+
  theme_cowplot(14)
LDm1

## Female Development Time Fig##

LDf1 <- ggplot(pheno, aes(x=as.factor(TP), y=LDf, group=Pheno.Treat))+
  geom_point(data = founder, aes(x=as.factor(TP), y=LDf,color = Pheno.Treat), size=5)+
  geom_linerange(data=founder, aes(ymin=LDf-LDfse,ymax=LDf+LDfse, color=Pheno.Treat), size=1)+
  geom_point(aes(color=Pheno.Treat),size=5)+
  geom_line(aes(color=Pheno.Treat))+
  geom_linerange(aes(ymin=LDf-LDfse,ymax=LDf+LDfse, color=Pheno.Treat), size=1)+
  ylab("Female Development Time (hrs)")+
  xlab("Sample Timepoints")+
  scale_x_discrete(labels=c("Founder","TP1", "TP2", "TP3", "TP4", "TP5"))+
  scale_colour_manual(name = "Cage Treatment", 
                      breaks = c("a", "b"),
                      labels = c("Low","High"),
                      values = c("grey","black"))+
  geom_line(data=aa.pheno.cage, aes(x=as.factor(TP), y=LDf, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4) + 
  geom_line(data=bb.pheno.cage, aes(x=as.factor(TP), y=LDf, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4)+
  theme_cowplot(14)
LDf1

##Starvation Resistance Female##

SRf1 <- ggplot(pheno, aes(x=as.factor(TP), y=SRF, group=Pheno.Treat))+
  geom_point(data = founder, aes(x=as.factor(TP), y=SRF,color = Pheno.Treat), size=5)+
  geom_linerange(data=founder, aes(ymin=SRF-SRFse,ymax=SRF+SRFse, color=Pheno.Treat), size=1)+
  geom_point(aes(color=Pheno.Treat),size=5)+
  geom_line(aes(color=Pheno.Treat))+
  geom_linerange(aes(ymin=SRF-SRFse,ymax=SRF+SRFse, color=Pheno.Treat), size=1)+
  ylab("Female Starvation Restistance (Hrs)")+
  xlab("Sample Timepoints")+
  scale_x_discrete(labels=c("Founder","TP1", "TP2", "TP3", "TP4", "TP5"))+
  scale_colour_manual(name = "Cage Treatment", 
                      breaks = c("a", "b"),
                      labels = c("Low","High"),
                      values = c("grey","black"))+
  geom_line(data=aa.pheno.cage, aes(x=as.factor(TP), y=SRF, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4) + 
  geom_line(data=bb.pheno.cage, aes(x=as.factor(TP), y=SRF, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4)+
 theme_cowplot(14)
SRf1

##Starvation Resistance Male##

SRm1 <- ggplot(pheno, aes(x=as.factor(TP), y=SRM, group=Pheno.Treat))+
  geom_point(data = founder, aes(x=as.factor(TP), y=SRM,color = Pheno.Treat), size=5)+
  geom_linerange(data=founder, aes(ymin=SRM-SRMse,ymax=SRM+SRMse, color=Pheno.Treat), size=1)+
  geom_point(aes(color=Pheno.Treat),size=5)+
  geom_line(aes(color=Pheno.Treat))+
  geom_linerange(aes(ymin=SRM-SRMse,ymax=SRM+SRMse, color=Pheno.Treat), size=1)+
  ylab("Male Starvation Restistance (Hrs)")+
  xlab("Sample Timepoints")+
  scale_x_discrete(labels=c("Founder","TP1", "TP2", "TP3", "TP4", "TP5"))+
  scale_colour_manual(name = "Cage Treatment", 
                      breaks = c("a", "b"),
                      labels = c("Low","High"),
                      values = c("grey","black"))+
  geom_line(data=aa.pheno.cage, aes(x=as.factor(TP), y=SRM, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4) + 
  geom_line(data=bb.pheno.cage, aes(x=as.factor(TP), y=SRM, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4)+
  theme_cowplot(14)
SRm1

##Fecundity per day##

Fec1 <- ggplot(pheno, aes(x=as.factor(TP), y=Fec, group=Pheno.Treat))+
  geom_point(data = founder, aes(x=as.factor(TP), y=Fec,color = Pheno.Treat), size=5)+
  geom_linerange(data=founder, aes(ymin=Fec-Fecse,ymax=Fec+Fecse, color=Pheno.Treat), size=1)+
  geom_point(aes(color=Pheno.Treat),size=5)+
  geom_line(aes(color=Pheno.Treat))+
  geom_linerange(aes(ymin=Fec-Fecse,ymax=Fec+Fecse, color=Pheno.Treat), size=1)+
  ylab("Fecundity (Eggs / Day)")+
  xlab("Sample Timepoints")+
  scale_x_discrete(labels=c("Founder","TP1", "TP2", "TP3", "TP4", "TP5"))+
  scale_colour_manual(name = "Cage Treatment", 
                      breaks = c("a", "b"),
                      labels = c("Low","High"),
                      values = c("grey","black"))+
  geom_line(data=aa.pheno.cage, aes(x=as.factor(TP), y=Fec, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4) + 
  geom_line(data=bb.pheno.cage, aes(x=as.factor(TP), y=Fec, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4)+
  theme_cowplot(14)
Fec1

##Fecundity Total ##

TOTF1 <- ggplot(pheno, aes(x=as.factor(TP), y=TOTF, group=Pheno.Treat))+
  geom_point(data = founder, aes(x=as.factor(TP), y=TOTF,color = Pheno.Treat), size=5)+
  geom_linerange(data=founder, aes(ymin=TOTF-TOTFse,ymax=TOTF+TOTFse, color=Pheno.Treat), size=1)+
  geom_point(aes(color=Pheno.Treat),size=5)+
  geom_line(aes(color=Pheno.Treat))+
  geom_linerange(aes(ymin=TOTF-TOTFse,ymax=TOTF+TOTFse, color=Pheno.Treat), size=1)+
  ylab("Fecundity (Total)")+
  xlab("Sample Timepoints")+
  scale_x_discrete(labels=c("Founder","TP1", "TP2", "TP3", "TP4", "TP5"))+
  scale_colour_manual(name = "Cage Treatment", 
                      breaks = c("a", "b"),
                      labels = c("Low Nutririon","High"),
                      values = c("grey","black"))+
  geom_line(data=aa.pheno.cage, aes(x=as.factor(TP), y=TOTF, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4) + 
  geom_line(data=bb.pheno.cage, aes(x=as.factor(TP), y=TOTF, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4)+
  theme_cowplot(14)
TOTF1

##Viability ##

Viab1 <- ggplot(pheno, aes(x=as.factor(TP), y=Viab, group=Pheno.Treat))+
  geom_point(data = founder, aes(x=as.factor(TP), y=Viab,color = Pheno.Treat), size=5)+
  geom_linerange(data=founder, aes(ymin=Viab-Viabse,ymax=Viab+Viabse, color=Pheno.Treat), size=1)+
  geom_point(aes(color=Pheno.Treat),size=5)+
  geom_line(aes(color=Pheno.Treat))+
  geom_linerange(aes(ymin=Viab-Viabse,ymax=Viab+Viabse, color=Pheno.Treat), size=1)+
  ylab("Egg to Adult Viability")+
  xlab("Sample Timepoints")+
  scale_x_discrete(labels=c("Founder","TP1", "TP2", "TP3", "TP4", "TP5"))+
  scale_colour_manual(name = "Cage Treatment", 
                      breaks = c("a", "b"),
                      labels = c("Low Nutririon","High"),
                      values = c("grey","black"))+
  ylim(0.5,1.1)+
  geom_line(data=aa.pheno.cage, aes(x=as.factor(TP), y=Viab, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4) + 
  geom_line(data=bb.pheno.cage, aes(x=as.factor(TP), y=Viab, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4)+
  theme_cowplot(14)
Viab1

##Body Size ##

BS1 <- ggplot(pheno, aes(x=as.factor(TP), y=BS, group=Pheno.Treat))+
  geom_point(data = founder, aes(x=as.factor(TP), y=BS,color = Pheno.Treat), size=5)+
  geom_linerange(data=founder, aes(ymin=BS-BSse,ymax=BS+BSse, color=Pheno.Treat), size=1)+
  geom_point(aes(color=Pheno.Treat),size=5)+
  geom_line(aes(color=Pheno.Treat))+
  geom_linerange(aes(ymin=BS-BSse,ymax=BS+BSse, color=Pheno.Treat), size=1)+
  ylab("Thorax Length (uM)")+
  xlab("Sample Timepoints")+
  scale_x_discrete(labels=c("Founder","TP1", "TP2", "TP3", "TP4", "TP5"))+
  scale_colour_manual(name = "Cage Treatment", 
                      breaks = c("a", "b"),
                      labels = c("Low Nutririon","High"),
                      values = c("grey","black"))+
  geom_line(data=aa.pheno.cage, aes(x=as.factor(TP), y=BS, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4) + 
  geom_line(data=bb.pheno.cage, aes(x=as.factor(TP), y=BS, group=as.factor(Cage), color=Pheno.Treat), 
            size=.5, alpha= .4)+
  theme_cowplot(14)
BS1

######Combine####

library(ggpubr)

Fig2 <- ggarrange(LD1 + rremove("xlab"),SRm1+ rremove("xlab"),SRf1+ rremove("xlab"),Viab1,BS1,Fec1,
                    labels = c("A", "B", "C", "D", "E", "F"),
                    ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom", font.label=list(color="black",size=16, face = "bold"))
Fig2
save(Fig2, file = "fig2.rdata")



######Linear Mixed Model Analysis F####
###Found in Table 2###


ABLmmdata=subset(Pheno.cage, Pheno.cage$Cage.Pheno %in% c("aa","bb"))
#View(ABLmmdata)

LD1<-lme(LD ~ TP*Pheno.Treat, random=~1|Cage/TP, data=ABLmmdata) ##
anova(LD1) ##TP + TREATMENT SIG / INTERCEPT = 0.08

LDm1<-lme(LDm ~ TP*Pheno.Treat, random=~1|Cage/TP, data=ABLmmdata) ##
anova(LDm1) ##TP + TREATMENT SIG / INTERCEPT = 0.11

LDf1<-lme(LDf ~ TP*Pheno.Treat, random=~1|Cage/TP, data=ABLmmdata) ##
anova(LDf1) ##TP + TREATMENT SIG / INTERCEPT = 0.06

V1<-lme(Viab ~ TP*Pheno.Treat, random=~1|Cage/TP, data=ABLmmdata) ##
anova(V1)    ##TP + INTERCEPT SIG / TREATMENT = .269

SRM1<-lme(SRM ~ TP*Pheno.Treat, random=~1|Cage/TP, data=ABLmmdata) ##
anova(SRM1) ##TP + TREATMENT SIG / INTERCEPT = 0.49

SRF1<-lme(SRF ~ TP*Pheno.Treat, random=~1|Cage/TP, data=ABLmmdata) ##
anova(SRF1) ##TP + TREATMENT SIG / INTERCEPT = 0.69

Fec1<-lme(Fec ~ TP*Pheno.Treat, random=~1|Cage/TP, data=ABLmmdata) ##
anova(Fec1) ##TP + TREATMENT / INTERCEPT = 0.06

TOTF1<-lme(TOTF ~ TP*Pheno.Treat, random=~1|Cage/TP, data=ABLmmdata)
anova(TOTF1)##TP + TREATMENT / INTERCEPT = 0.06

BS1<-lme(BS ~ TP*Pheno.Treat, random=~1|Cage/TP, data=ABLmmdata) ##
anova(BS1) ## INTERCEPT + TREATMENT SIG / TP = 0.1550



















#### Reciprocal Transfer SUPP Figure####



#view(Phenos24)
#view(founder)
#View(Phenos.cage24)

## Male + Female Development Time##

LDR1 <- ggplot(Phenos24, aes(x=as.numeric(TP), y=LD, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=LD-LDse,ymax=LD+LDse, color=Cage.Pheno), size=1)+
  ylab("Development Time (hrs)")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=LD,color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=LD-LDse,ymax=LD+LDse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=LD, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=LD, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)+
  theme(legend.position="none")
  
LDR1

##male development time

LDmR1 <- ggplot(Phenos24, aes(x=as.numeric(TP), y=LDm, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=LDm-LDmse,ymax=LDm+LDmse, color=Cage.Pheno), size=1)+
  ylab("Male Development Time (hrs)")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=LDm,color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=LDm-LDmse,ymax=LDm+LDmse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=LDm, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=LDm, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)+
  theme(legend.position="none")

LDmR1

##female developement time

LDfR1 <- ggplot(Phenos24, aes(x=as.numeric(TP), y=LDf, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=LDf-LDfse,ymax=LDf+LDfse, color=Cage.Pheno), size=1)+
  ylab("Female Development Time (hrs)")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=LDf,color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=LDf-LDfse,ymax=LDf+LDfse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=LDf, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=LDf, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)+
  theme(legend.position="none")

LDfR1

##Male Starvation Resistance

SRMR1 <- ggplot(Phenos24, aes(x=as.numeric(TP), y=SRM, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=SRM-SRMse,ymax=SRM+SRMse, color=Cage.Pheno), size=1)+
  ylab("Male Starvation Resistance (hrs)")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=SRM,color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=SRM-SRMse,ymax=SRM+SRMse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=SRM, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=SRM, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)+
  theme(legend.position="none")

SRMR1

##Female Starvation Resistance

SRFR1 <- ggplot(Phenos24, aes(x=as.numeric(TP), y=SRF, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=SRF-SRFse,ymax=SRF+SRFse, color=Cage.Pheno), size=1)+
  ylab("Female Starvation Resistance (hrs)")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=SRF,color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=SRF-SRFse,ymax=SRF+SRFse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=SRF, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=SRF, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)+
  theme(legend.position="none")

SRFR1

##FEecundity Eggs/day

FecR1 <- ggplot(Phenos24, aes(x=as.numeric(TP), y=Fec, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=Fec-Fecse,ymax=Fec+Fecse, color=Cage.Pheno), size=1)+
  ylab("Fecundity (Eggs/day)")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=Fec, color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=Fec-Fecse,ymax=Fec+Fecse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=Fec, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=Fec, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)+
  theme(legend.position="none")

FecR1

##Fecundity total 

TOTFR1 <- ggplot(Phenos24, aes(x=as.numeric(TP), y=TOTF, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=TOTF-TOTFse,ymax=TOTF+TOTFse, color=Cage.Pheno), size=1)+
  ylab("Fecundity (Total)")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=TOTF, color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=TOTF-TOTFse,ymax=TOTF+TOTFse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=TOTF, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=TOTF, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)+
  theme(legend.position="none")

TOTFR1

##Viability## 

ViabR1 <- ggplot(Phenos24, aes(x=as.numeric(TP), y=Viab, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=Viab-Viabse,ymax=Viab+Viabse, color=Cage.Pheno), size=1)+
  ylab("Egg to Adult Viability")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=Viab, color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=Viab-Viabse,ymax=Viab+Viabse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=Viab, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=Viab, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)+
  theme(legend.position="none")

ViabR1

##Body Size## 

BSR1 <- ggplot(Phenos24, aes(x=as.numeric(TP), y=BS, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=BS-BSse,ymax=BS+BSse, color=Cage.Pheno), size=1)+
  ylab("Thorax Length")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=BS, color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=BS-BSse,ymax=BS+BSse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=BS, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=BS, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)+
  theme(legend.position="none")

BSR1

######Combine####

###TEST TO MAKE LEGEND ALONE###

TEST <- ggplot(Phenos24, aes(x=as.numeric(TP), y=BS, group=Cage.Pheno))+
  geom_point(aes(color=Cage.Pheno,shape=Cage.Pheno),size=5)+
  geom_line(aes(color=Cage.Pheno))+
  geom_linerange(aes(ymin=BS-BSse,ymax=BS+BSse, color=Cage.Pheno), size=1)+
  ylab("Thorax Length")+
  xlab("Sample Timepoints") +
  scale_x_continuous(expand = c(0,0),limits = c(.8, 5.5), breaks = c(1,3,5),labels=c("Founder","TP2","TP4"))+
  scale_x_break(c(1.1, 2.9), scales = 5)+
  scale_colour_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                      breaks = c("aa", "ba", "bb", "ab"),
                      labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                      values = c("red","black","black","red"))+
  scale_shape_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                     breaks = c("aa", "ba", "bb", "ab"),
                     labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                     values = c(aa=8,ba=8,bb=19,ab=19))+
  scale_linetype_manual(name = "Cage Treatment (Phenotyping Treatment) ", 
                        breaks = c("aa", "ba", "bb", "ab"),
                        labels = c("Low (Low)", "High (Low)", "High (High)", "Low (High)"),
                        values=c(aa="dashed",ba="dashed",bb="F1",ab="F1"))+
  geom_point(data = founder, aes(x=as.numeric(TP), y=BS, color = Cage.Pheno, shape=Cage.Pheno), size=5)+
  geom_linerange(data=founder, aes(ymin=BS-BSse,ymax=BS+BSse, color=Cage.Pheno), size=1)+
  geom_line(data=aa.pheno24.cage, aes(x=as.numeric(TP), y=BS, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) + 
  geom_line(data=bb.pheno24.cage, aes(x=as.numeric(TP), y=BS, group=as.factor(Cage), color=Cage.Pheno, linetype=Cage.Pheno), 
            size=.5, alpha= .3) +
  theme_cowplot(font_size = 12)

TEST

##get legend##

leg <- get_legend(TEST)

# Convert to a ggplot and print
as_ggplot(leg)

##FILLER to center LEGEND
s=plot_spacer() + theme_void()

##MULTIPLOT##
aplot::plot_list(LDR1, LDmR1, LDfR1, SRMR1, SRFR1, ViabR1, BSR1, FecR1, TOTFR1,s,leg, nrow = 4, ncol = 3, heights = c(5,5,5,3))

aplot::plot_list(LDmR1, LDfR1, SRMR1, SRFR1, ViabR1, BSR1, FecR1,s,leg, nrow = 3, ncol = 3, heights = c(1,1,1))



