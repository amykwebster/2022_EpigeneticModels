#Analyses of scenarios, plotting epimutation vs back-epimutation
library(ggplot2)
library(gplots)
setwd("/Users/amywebster/Documents/PhillipsLab/Aim2/2022_Models/Models_072422/")
#SCENARIO 1a
Scenario1a_jacobian<-read.csv("model_Scenario1a_jacobian_2.csv",header = T)
Scenario1a_jacobian$p4_equil<- (1 - ((as.numeric(Scenario1a_jacobian$p1_equil) + as.numeric(Scenario1a_jacobian$p2_equil) + as.numeric(Scenario1a_jacobian$p3_equil))))

Scen1a_NumEquil<-ggplot(Scenario1a_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("white","blue"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen1a_NumEquil

Scenario1a_jacobian_stable<-subset(Scenario1a_jacobian,stable=="yes")
Scenario1a_jacobian_stable_ordered<-Scenario1a_jacobian_stable[order(Scenario1a_jacobian_stable$p4_equil),]
Scenario1a_jacobian_stable_ordered2<-Scenario1a_jacobian_stable[order(-Scenario1a_jacobian_stable$p4_equil),]
Scenario1a_jacobian_stable_ordered_removedups<-Scenario1a_jacobian_stable_ordered[!duplicated(Scenario1a_jacobian_stable_ordered[c(2,3)]),]
Scenario1a_jacobian_stable_ordered2_removedups<-Scenario1a_jacobian_stable_ordered2[!duplicated(Scenario1a_jacobian_stable_ordered2[c(2,3)]),]

Scen1a_Stable1<-ggplot(Scenario1a_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen1a_Stable1

Scenario1a_jacobian_stable_ordered2_removedups<-Scenario1a_jacobian_stable_ordered2_removedups[order(Scenario1a_jacobian_stable_ordered2_removedups$p4_equil),]
Scen1a_Stable2<-ggplot(Scenario1a_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen1a_Stable2

gridExtra::grid.arrange(Scen1a_NumEquil,Scen1a_Stable1,Scen1a_Stable2,nrow=1)

Scenario1a_0.8<-read.csv("model_Scenario1a_jacobian2_0.8.csv",header = T)
head(Scenario1a_0.8)
Scenario1a_0.8_stable<-subset(Scenario1a_0.8,stable=="yes")
Scenario1a_0.8_stable_1equil<-subset(Scenario1a_0.8_stable,num_equil=="1")
Scenario1a_0.8_stable_1equil$p4_equil<- (1 - ((as.numeric(Scenario1a_0.8_stable_1equil$p1_equil) + as.numeric(Scenario1a_0.8_stable_1equil$p2_equil) + as.numeric(Scenario1a_0.8_stable_1equil$p3_equil))))

Scenario1a_0.8_stable$p4_equil<- (1 - ((as.numeric(Scenario1a_0.8_stable$p1_equil) + as.numeric(Scenario1a_0.8_stable$p2_equil) + as.numeric(Scenario1a_0.8_stable$p3_equil))))
Scenario1a_0.8_stable_ordered<-Scenario1a_0.8_stable[order(Scenario1a_0.8_stable$p4_equil),]
Scenario1a_0.8_stable_ordered2<-Scenario1a_0.8_stable[order(-Scenario1a_0.8_stable$p4_equil),]
Scenario1a_0.8_stable_ordered_removedups<-Scenario1a_0.8_stable_ordered[!duplicated(Scenario1a_0.8_stable_ordered[c(2,3)]),]
Scenario1a_0.8_stable_ordered2_removedups<-Scenario1a_0.8_stable_ordered2[!duplicated(Scenario1a_0.8_stable_ordered2[c(2,3)]),]
Scenario1a_0.8_stable_ordered2_removedups<-Scenario1a_0.8_stable_ordered2_removedups[order(Scenario1a_0.8_stable_ordered2_removedups$p4_equil),]

Scen1a_0.8_NumEquil<-ggplot(Scenario1a_0.8_stable,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("white","blue"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen1a_0.8_NumEquil

Scen1a_0.8_Stable1<-ggplot(Scenario1a_0.8_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=7,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  geom_point(data=subset(Scenario1a_0.8_stable_ordered_removedups,num_equil=="3"),size=1,aes(x=epimutation,y=back_epimutation,shape=as.factor(num_equil)))+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen1a_0.8_Stable1

Scen1a_0.8_Stable2<-ggplot(Scenario1a_0.8_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=7,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

gridExtra::grid.arrange(Scen1a_NumEquil,Scen1a_Stable1,Scen1a_Stable2,Scen1a_0.8_NumEquil,Scen1a_0.8_Stable1,Scen1a_0.8_Stable2,nrow=2)


#SCENARIO 1b
Scenario1b_jacobian<-read.csv("model_Scenario1b_jacobian2.csv",header = T)
Scenario1b_jacobian$p4_equil<- (1 - ((as.numeric(Scenario1b_jacobian$p1_equil) + as.numeric(Scenario1b_jacobian$p2_equil) + as.numeric(Scenario1b_jacobian$p3_equil))))

Scen1b_NumEquil<-ggplot(Scenario1b_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("white","blue","black"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen1b_NumEquil

Scenario1b_jacobian_ordered<-Scenario1b_jacobian[order(Scenario1b_jacobian$p4_equil),]
Scen1b_Stable1<-ggplot(Scenario1b_jacobian,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=7,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen1b_Stable1



#SCENARIO 2a
setwd("/Users/amywebster/Documents/PhillipsLab/Aim2/2022_Models/Models_072422/TalapasCluster_Mathematica")

Scenario2a_jacobian<-read.csv("model_Scenario2a_jacobian2.csv",header = T)
Scenario2a_jacobian$p4_equil<- (1 - ((as.numeric(Scenario2a_jacobian$p1_equil) + as.numeric(Scenario2a_jacobian$p2_equil) + as.numeric(Scenario2a_jacobian$p3_equil))))

Scen2a_NumEquil<-ggplot(Scenario2a_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("white","blue"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen2a_NumEquil

Scenario2a_jacobian_stable<-subset(Scenario2a_jacobian,stable=="yes")
Scenario2a_jacobian_stable_ordered<-Scenario2a_jacobian_stable[order(Scenario2a_jacobian_stable$p4_equil),]
Scenario2a_jacobian_stable_ordered2<-Scenario2a_jacobian_stable[order(-Scenario2a_jacobian_stable$p4_equil),]
Scenario2a_jacobian_stable_ordered_removedups<-Scenario2a_jacobian_stable_ordered[!duplicated(Scenario2a_jacobian_stable_ordered[c(2,3)]),]
Scenario2a_jacobian_stable_ordered2_removedups<-Scenario2a_jacobian_stable_ordered2[!duplicated(Scenario2a_jacobian_stable_ordered2[c(2,3)]),]

range(Scenario2a_jacobian_stable$p1_equil) #always 0
range(Scenario2a_jacobian_stable$p2_equil) #almost 0 to almost 1
range(Scenario2a_jacobian_stable$p3_equil) #always 0
range(Scenario2a_jacobian_stable$p4_equil) #almost 0 to almost 1 

Scen2a_Stable1<-ggplot(Scenario2a_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen2a_Stable1

Scenario2a_jacobian_stable_ordered2_removedups<-Scenario2a_jacobian_stable_ordered2_removedups[order(Scenario2a_jacobian_stable_ordered2_removedups$p4_equil),]
Scen2a_Stable2<-ggplot(Scenario2a_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen2a_Stable2 #looks identical to stable1 as expected considering so few have 2 equilibria

gridExtra::grid.arrange(Scen2a_NumEquil,Scen2a_Stable1,Scen2a_Stable2,nrow=1)

#SCENARIO 2b

setwd("/Users/amywebster/Documents/PhillipsLab/Aim2/2022_Models/Models_072422/TalapasCluster_Mathematica")

Scenario2b_jacobian<-read.csv("model_Scenario2b_jacobian2.csv",header = T)
Scenario2b_jacobian$p4_equil<- (1 - ((as.numeric(Scenario2b_jacobian$p1_equil) + as.numeric(Scenario2b_jacobian$p2_equil) + as.numeric(Scenario2b_jacobian$p3_equil))))

Scen2b_NumEquil<-ggplot(Scenario2b_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("white","blue"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen2b_NumEquil

Scenario2b_jacobian_stable<-subset(Scenario2b_jacobian,stable=="yes")
Scenario2b_jacobian_stable_ordered<-Scenario2b_jacobian_stable[order(Scenario2b_jacobian_stable$p4_equil),]

range(Scenario2b_jacobian_stable$p1_equil) #always 0
range(Scenario2b_jacobian_stable$p2_equil) #almost 0 to almost 1
range(Scenario2b_jacobian_stable$p3_equil) #always 0
range(Scenario2b_jacobian_stable$p4_equil) #almost 0 to almost 1 

Scen2b_Stable1<-ggplot(Scenario2b_jacobian_stable_ordered,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen2b_Stable1


#SCENARIO 3a
Scenario3a_jacobian<-read.csv("model_Scenario3a_jacobian2.csv",header = T)
Scenario3a_jacobian$p4_equil<- (1 - ((as.numeric(Scenario3a_jacobian$p1_equil) + as.numeric(Scenario3a_jacobian$p2_equil) + as.numeric(Scenario3a_jacobian$p3_equil))))

Scen3a_NumEquil<-ggplot(Scenario3a_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("yellow","blue","green","red","purple"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen3a_NumEquil

Scenario3a_jacobian_stable<-subset(Scenario3a_jacobian,stable=="yes")
Scenario3a_jacobian_stable_ordered<-Scenario3a_jacobian_stable[order(Scenario3a_jacobian_stable$p4_equil),]

range(Scenario3a_jacobian_stable$p1_equil) #almost 0 to almost 1
range(Scenario3a_jacobian_stable$p2_equil) #very close to 0
range(Scenario3a_jacobian_stable$p3_equil) #almost 0 to almost 1
range(Scenario3a_jacobian_stable$p4_equil) #almost 0 to almost 1-- therefore possible to have some genetic variation

Scen3a_Stable1_p4<-ggplot(Scenario3a_jacobian_stable_ordered,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3a_Stable1_p4

Scen3a_Stable1_p1<-ggplot(Scenario3a_jacobian_stable_ordered,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3a_Stable1_p1

Scen3a_Stable1_p3<-ggplot(Scenario3a_jacobian_stable_ordered,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3a_Stable1_p3

Scen3a_Stable1_p2<-ggplot(Scenario3a_jacobian_stable_ordered,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3a_Stable1_p2

gridExtra::grid.arrange(Scen3a_Stable1_p1,Scen3a_Stable1_p2,Scen3a_Stable1_p3,Scen3a_Stable1_p4,nrow=1)


#SCENARIO 3b
Scenario3b_jacobian<-read.csv("model_Scenario3b_jacobian2.csv",header = T)
Scenario3b_jacobian$p4_equil<- (1 - ((as.numeric(Scenario3b_jacobian$p1_equil) + as.numeric(Scenario3b_jacobian$p2_equil) + as.numeric(Scenario3b_jacobian$p3_equil))))

Scen3b_NumEquil<-ggplot(Scenario3b_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("yellow","blue","green","red","purple"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen3b_NumEquil


Scenario3b_jacobian_stable<-subset(Scenario3b_jacobian,stable=="yes")
Scenario3b_jacobian_stable_ordered<-Scenario3b_jacobian_stable[order(Scenario3b_jacobian_stable$p4_equil),]

StableScenario3b_aberrant<-subset(Scenario3b_jacobian_stable_ordered,p2_equil>0.1)#129
unique(StableScenario3b_aberrant$epimutation)
unique(StableScenario3b_aberrant$back_epimutation)

range(Scenario3b_jacobian_stable$p1_equil) #almost 0 to almost 1
range(Scenario3b_jacobian_stable$p2_equil) #very close to 0
range(Scenario3b_jacobian_stable$p3_equil) #almost 0 to almost 1
range(Scenario3b_jacobian_stable$p4_equil) #almost 0 to almost 1-- therefore possible to have some genetic variation

Scen3b_Stable1_p4<-ggplot(subset(Scenario3b_jacobian_stable_ordered,p2_equil<0.1),aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3b_Stable1_p4

Scen3b_Stable1_p2<-ggplot(subset(Scenario3b_jacobian_stable_ordered,p2_equil<0.1),aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3b_Stable1_p2


Scen3b_Stable1_p1<-ggplot(subset(Scenario3b_jacobian_stable_ordered,p2_equil<0.1),aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3b_Stable1_p1



Scen3b_Stable1_p3<-ggplot(subset(Scenario3b_jacobian_stable_ordered,p2_equil<0.1),aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3b_Stable1_p3

gridExtra::grid.arrange(Scen3b_Stable1_p1,Scen3b_Stable1_p2,Scen3b_Stable1_p3,Scen3b_Stable1_p4,nrow=1)


#SCENARIO 3c
Scenario3c_jacobian<-read.csv("model_Scenario3c_jacobian2.csv",header = T)
Scenario3c_jacobian$p4_equil<- (1 - ((as.numeric(Scenario3c_jacobian$p1_equil) + as.numeric(Scenario3c_jacobian$p2_equil) + as.numeric(Scenario3c_jacobian$p3_equil))))

Scen3c_NumEquil<-ggplot(Scenario3c_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("yellow","blue","green","red","purple"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen3c_NumEquil

Scenario3c_jacobian_stable<-subset(Scenario3c_jacobian,stable=="yes")
Scenario3c_jacobian_stable_ordered<-Scenario3c_jacobian_stable[order(Scenario3c_jacobian_stable$p4_equil),]
Scenario3c_jacobian_stable_ordered2<-Scenario3c_jacobian_stable[order(-Scenario3c_jacobian_stable$p4_equil),]
Scenario3c_jacobian_stable_ordered_removedups<-Scenario3c_jacobian_stable_ordered[!duplicated(Scenario3c_jacobian_stable_ordered[c(2,3)]),]
Scenario3c_jacobian_stable_ordered2_removedups<-Scenario3c_jacobian_stable_ordered2[!duplicated(Scenario3c_jacobian_stable_ordered2[c(2,3)]),]

Scen3c_Stable1_p4<-ggplot(Scenario3c_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3c_Stable1_p4

Scenario3c_jacobian_stable_ordered2_removedups<-Scenario3c_jacobian_stable_ordered2_removedups[order(Scenario3c_jacobian_stable_ordered2_removedups$p4_equil),]
Scen3c_Stable2_p4<-ggplot(Scenario3c_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen3c_Stable2_p4 #this is the same, focus on stable1

Scen3c_Stable1_p2<-ggplot(Scenario3c_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3c_Stable1_p2

Scen3c_Stable1_p3<-ggplot(Scenario3c_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3c_Stable1_p3

Scen3c_Stable1_p1<-ggplot(Scenario3c_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen3c_Stable1_p1

gridExtra::grid.arrange(Scen3c_Stable1_p1,Scen3c_Stable1_p2,Scen3c_Stable1_p3,Scen3c_Stable1_p4,nrow=1)

#SCENARIO 4a

Scenario4a_jacobian<-read.csv("model_Scenario4a_jacobian3.csv",header = T)
Scenario4a_jacobian$p4_equil<- (1 - ((as.numeric(Scenario4a_jacobian$p1_equil) + as.numeric(Scenario4a_jacobian$p2_equil) + as.numeric(Scenario4a_jacobian$p3_equil))))

Scen4a_NumEquil<-ggplot(Scenario4a_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("yellow","blue","green","red","purple"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4a_NumEquil

Scenario4a_jacobian_stable<-subset(Scenario4a_jacobian,stable=="yes")
Scenario4a_jacobian_stable_ordered<-Scenario4a_jacobian_stable[order(Scenario4a_jacobian_stable$p4_equil),]
Scenario4a_jacobian_stable_ordered2<-Scenario4a_jacobian_stable[order(-Scenario4a_jacobian_stable$p4_equil),]
Scenario4a_jacobian_stable_ordered_removedups<-Scenario4a_jacobian_stable_ordered[!duplicated(Scenario4a_jacobian_stable_ordered[c(2,3)]),]
Scenario4a_jacobian_stable_ordered2_removedups<-Scenario4a_jacobian_stable_ordered2[!duplicated(Scenario4a_jacobian_stable_ordered2[c(2,3)]),]

Scen4a_Stable1_p4<-ggplot(Scenario4a_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4a_Stable1_p4

Scenario4a_jacobian_stable_ordered2_removedups<-Scenario4a_jacobian_stable_ordered2_removedups[order(Scenario4a_jacobian_stable_ordered2_removedups$p4_equil),]
Scen4a_Stable2_p4<-ggplot(Scenario4a_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4a_Stable2_p4

Scen4a_Stable1_p1<-ggplot(Scenario4a_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4a_Stable1_p1

Scen4a_Stable2_p1<-ggplot(Scenario4a_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4a_Stable2_p1


Scen4a_Stable1_p2<-ggplot(Scenario4a_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4a_Stable1_p2

Scen4a_Stable2_p2<-ggplot(Scenario4a_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4a_Stable2_p2

Scen4a_Stable1_p3<-ggplot(Scenario4a_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4a_Stable1_p3

Scen4a_Stable2_p3<-ggplot(Scenario4a_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4a_Stable2_p3

gridExtra::grid.arrange(Scen4a_Stable1_p1,Scen4a_Stable1_p2,Scen4a_Stable1_p3,Scen4a_Stable1_p4,
                        Scen4a_Stable2_p1,Scen4a_Stable2_p2,Scen4a_Stable2_p3,Scen4a_Stable2_p4,nrow=2)



#SCENARIO 4b

Scenario4b_jacobian<-read.csv("model_Scenario4b_jacobian3.csv",header = T)
Scenario4b_jacobian$p4_equil<- (1 - ((as.numeric(Scenario4b_jacobian$p1_equil) + as.numeric(Scenario4b_jacobian$p2_equil) + as.numeric(Scenario4b_jacobian$p3_equil))))

Scen4b_NumEquil<-ggplot(Scenario4b_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("yellow","blue","green","red","purple"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4b_NumEquil

Scenario4b_jacobian_stable<-subset(Scenario4b_jacobian,stable=="yes")
Scenario4b_jacobian_stable_ordered<-Scenario4b_jacobian_stable[order(Scenario4b_jacobian_stable$p4_equil),]
Scenario4b_jacobian_stable_ordered2<-Scenario4b_jacobian_stable[order(-Scenario4b_jacobian_stable$p4_equil),]
Scenario4b_jacobian_stable_ordered_removedups<-Scenario4b_jacobian_stable_ordered[!duplicated(Scenario4b_jacobian_stable_ordered[c(2,3)]),]
Scenario4b_jacobian_stable_ordered2_removedups<-Scenario4b_jacobian_stable_ordered2[!duplicated(Scenario4b_jacobian_stable_ordered2[c(2,3)]),]
head(Scenario4b_jacobian_stable_ordered)
Scenario4b_5or6<-subset(Scenario4b_jacobian_stable_ordered,num_equil=="5"|num_equil=="6")
Scenario4b_5or6_ordered1<-subset(Scenario4b_jacobian_stable_ordered_removedups,num_equil=="5"|num_equil=="6")
Scenario4b_5or6_ordered2<-subset(Scenario4b_jacobian_stable_ordered2_removedups,num_equil=="5"|num_equil=="6")
head(Scenario4b_5or6_ordered1)
head(Scenario4b_5or6_ordered2)
Scenario4b_5or6_ordered1$included<-"yes"
Scenario4b_5or6_ordered2$included<-"yes"
Scenario4b_5or6_included<-rbind(Scenario4b_5or6_ordered1,Scenario4b_5or6_ordered2)
Scenario4b_5or6_venn<-venn(list(Scenario4b_5or6$p4_equil,Scenario4b_5or6_ordered1$p4_equil,Scenario4b_5or6_ordered2$p4_equil))
notIncludedVenn<-attr(Scenario4b_5or6_venn,"intersections")$`A`
Scen4b_5or6_merge<-merge(Scenario4b_5or6,Scenario4b_5or6_included,by=0,all.x = TRUE)
Scen4b_5or6_merge<-as.data.frame(Scen4b_5or6_merge)
head(Scen4b_5or6_merge)
notIncluded_4b_5or6<-Scen4b_5or6_merge[!complete.cases(Scen4b_5or6_merge$included),]
notIncluded_4b_5or6_filter<-notIncluded_4b_5or6[,2:21]
Scenario4b_jacobian_stable_ordered_removedups3<-subset(Scenario4b_jacobian_stable_ordered_removedups,num_equil!=5 & num_equil!=6)
colnames(notIncluded_4b_5or6_filter)<-colnames(Scenario4b_jacobian_stable_ordered_removedups3)
Scenario4b_jacobian_stable_ordered_removedups4<-rbind(Scenario4b_jacobian_stable_ordered_removedups3,notIncluded_4b_5or6_filter)


Scen4b_Stable1_p4<-ggplot(Scenario4b_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4b_Stable1_p4

Scenario4b_jacobian_stable_ordered2_removedups<-Scenario4b_jacobian_stable_ordered2_removedups[order(Scenario4b_jacobian_stable_ordered2_removedups$p4_equil),]
Scen4b_Stable2_p4<-ggplot(Scenario4b_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4b_Stable2_p4

Scenario4b_jacobian_stable_ordered3_removedups<-Scenario4b_jacobian_stable_ordered_removedups4[order(Scenario4b_jacobian_stable_ordered_removedups4$p4_equil),]

Scen4b_Stable3_p4<-ggplot(Scenario4b_jacobian_stable_ordered3_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4b_Stable3_p4


Scen4b_Stable1_p1<-ggplot(Scenario4b_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4b_Stable1_p1

Scen4b_Stable2_p1<-ggplot(Scenario4b_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4b_Stable2_p1

Scen4b_Stable3_p1<-ggplot(Scenario4b_jacobian_stable_ordered3_removedups,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4b_Stable3_p1


Scen4b_Stable1_p2<-ggplot(Scenario4b_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4b_Stable1_p2

Scen4b_Stable2_p2<-ggplot(Scenario4b_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4b_Stable2_p2


Scen4b_Stable3_p2<-ggplot(Scenario4b_jacobian_stable_ordered3_removedups,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4b_Stable3_p2

Scen4b_Stable1_p3<-ggplot(Scenario4b_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4b_Stable1_p3

Scen4b_Stable2_p3<-ggplot(Scenario4b_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4b_Stable2_p3

Scen4b_Stable3_p3<-ggplot(Scenario4b_jacobian_stable_ordered3_removedups,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4b_Stable3_p3


gridExtra::grid.arrange(Scen4b_Stable1_p1,Scen4b_Stable1_p2,Scen4b_Stable1_p3,Scen4b_Stable1_p4,
                        Scen4b_Stable2_p1,Scen4b_Stable2_p2,Scen4b_Stable2_p3,Scen4b_Stable2_p4,
                        Scen4b_Stable3_p1,Scen4b_Stable3_p2,Scen4b_Stable3_p3,Scen4b_Stable3_p4,nrow=3)


head(Scenario4b_jacobian)
Scenario4b_6equil<-subset(Scenario4b_jacobian,num_equil=="6")
head(Scenario4b_6equil,20)
Scenario4b_6equil_onebackepi<-subset(Scenario4b_6equil,back_epimutation=="0.002044247")
tail(Scenario4b_6equil_onebackepi,50)
Scenario4b_6equil_onebackepi2<-subset(Scenario4b_6equil,back_epimutation=="0.001951728")

ggplot(Scenario4b_6equil_onebackepi,aes(x=epimutation,y=p2_equil))+
  geom_point(aes(color=stable))

ggplot(Scenario4b_6equil_onebackepi2,aes(x=epimutation,y=p2_equil))+
  geom_point(aes(color=stable),alpha=0.25,size=5)

#SCENARIO 4c
Scenario4c_jacobian<-read.csv("model_Scenario4c_jacobian3.csv",header = T)
Scenario4c_jacobian$p4_equil<- (1 - ((as.numeric(Scenario4c_jacobian$p1_equil) + as.numeric(Scenario4c_jacobian$p2_equil) + as.numeric(Scenario4c_jacobian$p3_equil))))
Scen4c_NumEquil<-ggplot(Scenario4c_jacobian,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  facet_grid(.~num_equil)+
  scale_color_manual(values=c("yellow","blue","green","red","purple"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_NumEquil



head(Scenario4c_jacobian_stable)
Scenario4c_jacobian_stable<-subset(Scenario4c_jacobian,stable=="yes" & (num_equil>1 | p2_equil< 0.005) & (num_equil!=2|back_epimutation>0.005) & (num_equil!=3|epimutation>0.0001 | back_epimutation<0.002))

#after filtering (some) abnormalities
Scen4c_NumEquil2<-ggplot(Scenario4c_jacobian_stable,aes(x=epimutation,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  facet_grid(.~num_equil)+
  scale_color_manual(values=c("yellow","blue","green","red","purple"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_NumEquil2

Scenario4c_jacobian_stable_ordered<-Scenario4c_jacobian_stable[order(Scenario4c_jacobian_stable$p4_equil),]
Scenario4c_jacobian_stable_ordered2<-Scenario4c_jacobian_stable[order(-Scenario4c_jacobian_stable$p4_equil),]
Scenario4c_jacobian_stable_ordered_removedups<-Scenario4c_jacobian_stable_ordered[!duplicated(Scenario4c_jacobian_stable_ordered[c(2,3)]),]
Scenario4c_jacobian_stable_ordered2_removedups<-Scenario4c_jacobian_stable_ordered2[!duplicated(Scenario4c_jacobian_stable_ordered2[c(2,3)]),]
head(Scenario4c_jacobian_stable_ordered)
Scenario4c_4or5<-subset(Scenario4c_jacobian_stable_ordered,num_equil=="4"|num_equil=="5")
Scenario4c_4or5_ordered1<-subset(Scenario4c_jacobian_stable_ordered_removedups,num_equil=="4"|num_equil=="5")
Scenario4c_4or5_ordered2<-subset(Scenario4c_jacobian_stable_ordered2_removedups,num_equil=="4"|num_equil=="5")
head(Scenario4c_4or5_ordered1)
head(Scenario4c_4or5_ordered2)
Scenario4c_4or5_ordered1$included<-"yes"
Scenario4c_4or5_ordered2$included<-"yes"
Scenario4c_4or5_included<-rbind(Scenario4c_4or5_ordered1,Scenario4c_4or5_ordered2)
Scenario4c_4or5_venn<-venn(list(Scenario4c_4or5$p4_equil,Scenario4c_4or5_ordered1$p4_equil,Scenario4c_4or5_ordered2$p4_equil))
notIncludedVenn4c<-attr(Scenario4c_4or5_venn,"intersections")$`A`
Scen4c_4or5_merge<-merge(Scenario4c_4or5,Scenario4c_4or5_included,by=0,all.x = TRUE)
Scen4c_4or5_merge<-as.data.frame(Scen4c_4or5_merge)
head(Scen4c_4or5_merge)
notIncluded_4c_4or5<-Scen4c_4or5_merge[!complete.cases(Scen4c_4or5_merge$included),]
notIncluded_4c_4or5_filter<-notIncluded_4c_4or5[,2:21]
Scenario4c_jacobian_stable_ordered_removedups3<-subset(Scenario4c_jacobian_stable_ordered_removedups,num_equil!=4 & num_equil!=5)
colnames(notIncluded_4c_4or5_filter)<-colnames(Scenario4c_jacobian_stable_ordered_removedups3)
Scenario4c_jacobian_stable_ordered_removedups4<-rbind(Scenario4c_jacobian_stable_ordered_removedups3,notIncluded_4c_4or5_filter)

Scen4c_Stable1_p4<-ggplot(Scenario4c_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4c_Stable1_p4

Scenario4c_jacobian_stable_ordered2_removedups<-Scenario4c_jacobian_stable_ordered2_removedups[order(Scenario4c_jacobian_stable_ordered2_removedups$p4_equil),]
Scen4c_Stable2_p4<-ggplot(Scenario4c_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_Stable2_p4

Scenario4c_jacobian_stable_ordered3_removedups<-Scenario4c_jacobian_stable_ordered_removedups4[order(Scenario4c_jacobian_stable_ordered_removedups4$p4_equil),]

Scen4c_Stable3_p4<-ggplot(Scenario4c_jacobian_stable_ordered3_removedups,aes(x=epimutation,y=back_epimutation,color=p4_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_Stable3_p4


Scen4c_Stable1_p2<-ggplot(Scenario4c_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4c_Stable1_p2

Scen4c_Stable2_p2<-ggplot(Scenario4c_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_Stable2_p2

Scen4c_Stable3_p2<-ggplot(Scenario4c_jacobian_stable_ordered3_removedups,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_Stable3_p2


Scen4c_Stable1_p1<-ggplot(Scenario4c_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4c_Stable1_p1

Scen4c_Stable2_p1<-ggplot(Scenario4c_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_Stable2_p1

Scen4c_Stable3_p1<-ggplot(Scenario4c_jacobian_stable_ordered3_removedups,aes(x=epimutation,y=back_epimutation,color=p1_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_Stable3_p1


Scen4c_Stable1_p3<-ggplot(Scenario4c_jacobian_stable_ordered_removedups,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")
Scen4c_Stable1_p3

Scen4c_Stable2_p3<-ggplot(Scenario4c_jacobian_stable_ordered2_removedups,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_Stable2_p3

Scen4c_Stable3_p3<-ggplot(Scenario4c_jacobian_stable_ordered3_removedups,aes(x=epimutation,y=back_epimutation,color=p3_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_Stable3_p3



gridExtra::grid.arrange(Scen4c_Stable1_p1,Scen4c_Stable1_p2,Scen4c_Stable1_p3,Scen4c_Stable1_p4,
                        Scen4c_Stable2_p1,Scen4c_Stable2_p2,Scen4c_Stable2_p3,Scen4c_Stable2_p4,
                        Scen4c_Stable3_p1,Scen4c_Stable3_p2,Scen4c_Stable3_p3,Scen4c_Stable3_p4,nrow=3)


gridExtra::grid.arrange(Scen3a_NumEquil,Scen3b_NumEquil,Scen3c_NumEquil,Scen4a_NumEquil,Scen4b_NumEquil,Scen4c_NumEquil,nrow=2)



#Now alter fitness but hold epimutation rate constant at 1e-5

#SCENARIO 3C analysis

Scenario3c_jacobian_w22<-read.csv("model_Scenario3c_jacobian_fitnessw22.csv",header = T)
Scenario3c_jacobian_w22$p4_equil<- (1 - ((as.numeric(Scenario3c_jacobian_w22$p1_equil) + as.numeric(Scenario3c_jacobian_w22$p2_equil) + as.numeric(Scenario3c_jacobian_w22$p3_equil))))


Scenario3c_jacobian_stable_w22<-subset(Scenario3c_jacobian_w22,stable=="yes")
Scenario3c_jacobian_stable_w22_ordered<-Scenario3c_jacobian_stable_w22[order(Scenario3c_jacobian_stable_w22$p4_equil),]

Scen3c_Stable1_p2_w22<-ggplot(Scenario3c_jacobian_stable_w22_ordered,aes(x=w22,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+
 # scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="blue",limit=c(0,1))+
  ggtitle("Scenario 3c: Epimutation 1e-5")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="w22 Fitness",y="Back-epimutation rate")
Scen3c_Stable1_p2_w22

subset_w22_highbackepi<-subset(Scenario3c_jacobian_stable_w22_ordered,back_epimutation>0.01)
range(subset_w22_highbackepi$p2_equil)


Scen3c_Stable1_p2_w22_2<-ggplot(subset_w22_highbackepi,aes(x=w22,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+
  # scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="blue")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="w22 Fitness",y="Back-epimutation rate")
Scen3c_Stable1_p2_w22_2


#SCENARIO 3C with 0.01 epimutation rate

Scenario3c_jacobian_w22_highepi<-read.csv("model_Scenario3c_jacobian_fitnessw22_highepi.csv",header = T)
Scenario3c_jacobian_w22_highepi$p4_equil<- (1 - ((as.numeric(Scenario3c_jacobian_w22_highepi$p1_equil) + as.numeric(Scenario3c_jacobian_w22_highepi$p2_equil) + as.numeric(Scenario3c_jacobian_w22_highepi$p3_equil))))
head(Scenario3c_jacobian_stable_w22_highepi)

Scenario3c_jacobian_stable_w22_highepi<-subset(Scenario3c_jacobian_w22_highepi,stable=="yes")
Scenario3c_jacobian_stable_w22_ordered_highepi<-Scenario3c_jacobian_stable_w22_highepi[order(Scenario3c_jacobian_stable_w22_highepi$p4_equil),]

Scen3c_Stable1_p2_w22_highepi<-ggplot(Scenario3c_jacobian_stable_w22_ordered_highepi,aes(x=w22,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+
  # scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="blue",limit=c(0,1))+
  ggtitle("Scenario 3c: Epimutation 0.01")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="w22 Fitness",y="Back-epimutation rate")
Scen3c_Stable1_p2_w22_highepi

subset_w22_highbackepi_highepi<-subset(Scenario3c_jacobian_stable_w22_ordered_highepi,back_epimutation>0.01)
range(subset_w22_highbackepi_highepi$p2_equil)

Scen3c_Stable1_p2_w22_2_highepi<-ggplot(subset_w22_highbackepi_highepi,aes(x=w22,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+
  # scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="blue",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="w22 Fitness",y="Back-epimutation rate")
Scen3c_Stable1_p2_w22_2_highepi

#SCENARIO 4C analysis with 1e-5 epimutation rate

Scenario4c_jacobian_w22<-read.csv("model_Scenario4c_jacobian_fitnessw22.csv",header = T)
Scenario4c_jacobian_w22$p4_equil<- (1 - ((as.numeric(Scenario4c_jacobian_w22$p1_equil) + as.numeric(Scenario4c_jacobian_w22$p2_equil) + as.numeric(Scenario4c_jacobian_w22$p3_equil))))
Scen4c_NumEquil_w22<-ggplot(Scenario4c_jacobian_w22,aes(x=w22,y=back_epimutation,color=as.factor(num_equil)))+
  geom_point(size=2,alpha=0.5)+scale_y_continuous(trans='log10')+
  scale_color_manual(values=c("yellow","blue","green","red","purple"))+ggtitle("Regions of multiple equilibria")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="w22 Fitness",y="Back-epimutation rate")

Scen4c_NumEquil_w22

Scenario4c_jacobian_stable_w22<-subset(Scenario4c_jacobian_w22,stable=="yes")
range(Scenario4c_jacobian_stable_w22$p2_equil)
head(Scenario4c_jacobian_stable_w22)
Scenario4c_jacobian_stable_ordered_w22<-Scenario4c_jacobian_stable_w22[order(Scenario4c_jacobian_stable_w22$p4_equil),]
Scenario4c_jacobian_stable_ordered2_w22<-Scenario4c_jacobian_stable_w22[order(-Scenario4c_jacobian_stable_w22$p4_equil),]
Scenario4c_jacobian_stable_ordered_removedups_w22<-Scenario4c_jacobian_stable_ordered_w22[!duplicated(Scenario4c_jacobian_stable_ordered_w22[c(3,5)]),]
Scenario4c_jacobian_stable_ordered2_removedups_w22<-Scenario4c_jacobian_stable_ordered2_w22[!duplicated(Scenario4c_jacobian_stable_ordered2_w22[c(3,5)]),]
head(Scenario4c_jacobian_stable_ordered_w22)

Scen4c_Stable1_p2_w22<-ggplot(Scenario4c_jacobian_stable_ordered_removedups_w22,aes(x=w22,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  ggtitle("Scenario 4c: Epimutation 1e-5")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="w22 Fitness",y="Back-epimutation rate")
Scen4c_Stable1_p2_w22

dim(Scenario4c_jacobian_stable_ordered2_removedups_w22)
head(Scenario4c_jacobian_stable_ordered2_removedups_w22)
range(Scenario4c_jacobian_stable_ordered2_removedups_w22$p2_equil)
subset(Scenario4c_jacobian_stable_ordered2_removedups_w22,w22<0.8 & p2_equil>0.15)
Scenario4c_jacobian_stable_ordered2_removedups_w22<-Scenario4c_jacobian_stable_ordered2_removedups_w22[order(Scenario4c_jacobian_stable_ordered2_removedups_w22$p4_equil),]
Scen4c_Stable2_p2_w22<-ggplot(Scenario4c_jacobian_stable_ordered2_removedups_w22,aes(x=w22,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="blue",limit=c(0,1))+
  ggtitle("Scenario 4c: Epimutation 1e-5")+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="w22 Fitness",y="Back-epimutation rate")

Scen4c_Stable2_p2_w22



gridExtra::grid.arrange(Scen3c_Stable1_p2_w22,Scen4c_Stable2_p2_w22,Scen3c_Stable1_p2_w22_highepi,nrow=2)



#going to try to figure out what's going on with 4c...
Scen4c_1equil<-subset(Scenario4c_jacobian_stable,num_equil=="1")
head(Scen4c_1equil)
Scen4c_1equil2<-subset(Scen4c_1equil,p2_equil>0.001)
#the weird ones are ones with only one equilibrium.....!!!

ggplot(Scen4c_1equil2,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

Scen4c_3equil<-subset(Scenario4c_jacobian_stable,num_equil=="3")
head(Scen4c_3equil)
Scen4c_3equil1<-subset(Scen4c_3equil,p2_equil<0.001)
Scen4c_3equil2<-subset(Scen4c_3equil,p2_equil>0.001)
#the weird ones are ones with only one equilibrium.....!!!

ggplot(Scen4c_3equil2,aes(x=epimutation,y=back_epimutation,color=p2_equil))+
  geom_point(size=2,alpha=0.5)+scale_x_continuous(trans='log10')+scale_y_continuous(trans='log10')+
  #scale_color_gradient2(low="#900C3F",high="#6B33FF",mid="black",midpoint=0.5,limit=c(0,1))+
  scale_color_gradient2(low="black",high="#c1272d",limit=c(0,1))+
  theme_classic(base_size = 12)+theme(aspect.ratio = 1)+labs(x="Epimutation rate",y="Back-epimutation rate")

