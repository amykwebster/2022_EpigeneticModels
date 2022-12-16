
setwd("/Users/amywebster/Documents/PhillipsLab/Aim2/Scripts_forPub/FinalScripts/")
library(ggplot2)
library(reshape2)
FocalBackEpi<-read.csv("model_FocalBackEpi_AllScenarios.csv",header = T)
head(FocalBackEpi)
FocalBackEpi<-FocalBackEpi[,1:22]
FocalBackEpi$p4_equil<-(1-FocalBackEpi$p1_equil-FocalBackEpi$p2_equil-FocalBackEpi$p3_equil)
head(FocalBackEpi)
FocalBackEpi_melt<-melt(FocalBackEpi,id.vars = c("scenario","mutation","epimutation","back_epimutation","w11","w22","w33","w44","w12","w13","w14","w23","w24","w34","num_equil","eigen1","eigen2","eigen3","determinant"),variable.name="p_equil",value.name="equil_freq")
head(FocalBackEpi_melt)
FocalBackEpi_stable<-subset(FocalBackEpi,eigen1<0 & eigen2<0 & eigen3<0)
FocalBackEpi_melt_stable<-subset(FocalBackEpi_melt,eigen1<0 & eigen2<0 & eigen3<0)
FocalBackEpi_melt_unstable<-subset(FocalBackEpi_melt,eigen1>0 | eigen2>0 | eigen3>0)
FocalBackEpi_melt_stable$stability<-"stable"
FocalBackEpi_melt_unstable$stability<-"unstable"
FocalBackEpi_melt_merge<-rbind(FocalBackEpi_melt_stable,FocalBackEpi_melt_unstable)
levels(FocalBackEpi_melt_merge$scenario)
p1<-ggplot(FocalBackEpi_stable,aes(x=epimutation,y=p1_equil))+
  geom_point(aes(color=as.factor(back_epimutation)),size=0.5,alpha=0.8)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+facet_wrap(.~scenario)+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency (p1)")+
  scale_x_continuous(trans='log10')

p2<-ggplot(FocalBackEpi_stable,aes(x=epimutation,y=p2_equil))+
  geom_point(aes(color=as.factor(back_epimutation)),size=0.5,alpha=0.8)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+facet_wrap(.~scenario)+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency (p2)")+
  scale_x_continuous(trans='log10')

p3<-ggplot(FocalBackEpi_stable,aes(x=epimutation,y=p3_equil))+
  geom_point(aes(color=as.factor(back_epimutation)),size=0.5,alpha=0.8)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+facet_wrap(.~scenario)+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency (p3)")+
  scale_x_continuous(trans='log10')

p4<-ggplot(FocalBackEpi_stable,aes(x=epimutation,y=p4_equil))+
  geom_point(aes(color=as.factor(back_epimutation)),size=0.5,alpha=0.8)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+facet_wrap(.~scenario)+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency (p4)")+
  scale_x_continuous(trans='log10')


gridExtra::grid.arrange(p1,p2,p3,p4,ncol=2)

head(FocalBackEpi_melt_merge)
Focal_Back<-ggplot(FocalBackEpi_melt_merge,aes(x=epimutation,y=equil_freq,alpha=stability))+
  geom_point(aes(color=as.factor(back_epimutation),alpha=stability),size=0.5)+
  theme_classic(base_size = 2)+ theme(aspect.ratio = 1)+
  facet_grid(scenario~p_equil)+
  scale_alpha_manual(values=c(1,0.03))+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency")+
  scale_x_continuous(trans='log10')


FocalBackEpi_Symmetric<-read.csv("model_FocalBackEpi_AllScenarios_Symmetric.csv",header = T)
head(FocalBackEpi_Symmetric)
FocalBackEpi_Symmetric<-FocalBackEpi_Symmetric[,1:22]
FocalBackEpi_Symmetric$p4_equil<-(1-FocalBackEpi_Symmetric$p1_equil-FocalBackEpi_Symmetric$p2_equil-FocalBackEpi_Symmetric$p3_equil)
FocalBackEpi_Sym_stable<-subset(FocalBackEpi_Symmetric,eigen1<0 & eigen2<0 & eigen3<0)

FocalBackEpi_Sym_melt<-melt(FocalBackEpi_Symmetric,id.vars = c("scenario","mutation","epimutation","back_epimutation","w11","w22","w33","w44","w12","w13","w14","w23","w24","w34","num_equil","eigen1","eigen2","eigen3","determinant"),variable.name="p_equil",value.name="equil_freq")
head(FocalBackEpi_Sym_melt)
FocalBackEpi_Sym_melt_stable<-subset(FocalBackEpi_Sym_melt,eigen1<0 & eigen2<0 & eigen3<0)
FocalBackEpi_Sym_melt_unstable<-subset(FocalBackEpi_Sym_melt,eigen1>0 | eigen2>0 | eigen3>0)
FocalBackEpi_Sym_melt_stable$stability<-"stable"
FocalBackEpi_Sym_melt_unstable$stability<-"unstable"
FocalBackEpi_Sym_melt_merge<-rbind(FocalBackEpi_Sym_melt_stable,FocalBackEpi_Sym_melt_unstable)

FocalBack_Sym<-ggplot(FocalBackEpi_Sym_melt_merge,aes(x=epimutation,y=equil_freq,alpha=stability))+
  geom_point(aes(color=as.factor(back_epimutation),alpha=stability),size=0.5)+
  theme_classic(base_size = 2)+ theme(aspect.ratio = 1)+
  facet_grid(scenario~p_equil)+
  scale_alpha_manual(values=c(1,0.03))+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency")+
  scale_x_continuous(trans='log10')

head(FocalBackEpi_Sym_stable)
GeneticVariationMaintained<-subset(FocalBackEpi_Sym_stable,back_epimutation==0.1 & ((p1_equil>0.05 & p2_equil>0.05)|(p1_equil>0.05 & p4_equil>0.05)|(p2_equil>0.05 & p3_equil>0.05)|(p3_equil>0.05 & p4_equil>0.05)))
head(GeneticVariationMaintained)
levels(factor(GeneticVariationMaintained$scenario))
GeneticVariationMaintained2<-subset(FocalBackEpi_stable,back_epimutation==0.1 & ((p1_equil>0.05 & p2_equil>0.05)|(p1_equil>0.05 & p4_equil>0.05)|(p2_equil>0.05 & p3_equil>0.05)|(p3_equil>0.05 & p4_equil>0.05)))
levels(factor(GeneticVariationMaintained2$scenario)) #1d and 3c


#Supp Fig 1 - neutral wrt fitness
Supp1<-ggplot(subset(FocalBackEpi_Sym_melt_merge,scenario=="1c"),aes(x=epimutation,y=equil_freq,alpha=stability))+
  geom_point(aes(color=as.factor(back_epimutation),alpha=stability),size=0.5)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+
  facet_grid(.~p_equil)+
  scale_alpha_manual(values=c(1,0.03))+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency")+
  scale_x_continuous(trans='log10')
Supp1

levels(FocalBackEpi_Sym_melt_merge$scenario)
Supp2<-ggplot(subset(FocalBackEpi_Sym_melt_merge,scenario=="1a-symmetric"|scenario=="1b-symmetric"|scenario=="1d-symmetric"|scenario=="2a-symmetric"|scenario=="2b-symmetric"|scenario=="2c-deleterious-symmetric"),aes(x=epimutation,y=equil_freq,alpha=stability))+
  geom_point(aes(color=as.factor(back_epimutation),alpha=stability),size=0.5)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+
  facet_grid(scenario~p_equil)+
  scale_alpha_manual(values=c(1,0.03))+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency")+
  scale_x_continuous(trans='log10')
Supp2

FocalBackEpi_all<-rbind(FocalBackEpi_melt_merge,FocalBackEpi_Sym_melt_merge)
Supp2_og<-ggplot(subset(FocalBackEpi_all,scenario=="1a"|scenario=="1b"|scenario=="1d"|scenario=="2a"|scenario=="2b"|scenario=="2c-deleterious"),aes(x=epimutation,y=equil_freq,alpha=stability))+
  geom_point(aes(color=as.factor(back_epimutation),alpha=stability),size=0.5)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+
  facet_grid(scenario~p_equil)+
  scale_alpha_manual(values=c(1,0.03))+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency")+
  scale_x_continuous(trans='log10')
Supp2_og


Supp3_og<-ggplot(subset(FocalBackEpi_all,scenario=="3a"|scenario=="3b"|scenario=="3c"|scenario=="3d-deleterious"|scenario=="4a"|scenario=="4b"|scenario=="4c"|scenario=="4d-deleterious"),aes(x=epimutation,y=equil_freq,alpha=stability))+
  geom_point(aes(color=as.factor(back_epimutation),alpha=stability),size=0.5)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+
  facet_grid(scenario~p_equil)+
  scale_alpha_manual(values=c(1,0.03))+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency")+
  scale_x_continuous(trans='log10')
Supp3_og

Supp3<-ggplot(subset(FocalBackEpi_all,scenario=="3a-symmetric"|scenario=="3b-symmetric"|scenario=="3c-symmetric"|scenario=="3d-deleterious-symmetric"|scenario=="4a-symmetric"|scenario=="4b-symmetric"|scenario=="4c-symmetric"|scenario=="4d-deleterious-symmetric"),aes(x=epimutation,y=equil_freq,alpha=stability))+
  geom_point(aes(color=as.factor(back_epimutation),alpha=stability),size=0.5)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+
  facet_grid(scenario~p_equil)+
  scale_alpha_manual(values=c(1,0.03))+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency")+
  scale_x_continuous(trans='log10')
Supp3

Fig7_extra<-ggplot(subset(FocalBackEpi_all,scenario=="3c"|scenario=="3d-deleterious"|scenario=="4c"|scenario=="4d-deleterious"),aes(x=epimutation,y=equil_freq,alpha=stability))+
  geom_point(aes(color=as.factor(back_epimutation),alpha=stability),size=0.5)+
  theme_classic(base_size = 8)+ theme(aspect.ratio = 1)+
  facet_grid(scenario~p_equil)+
  scale_alpha_manual(values=c(1,0.03))+
  scale_color_manual(values=c("#fbb03b","#f15a24","#c1272d"))+
  labs(x="Epimutation rate",y="Equilibrium allele frequency")+
  scale_x_continuous(trans='log10')
Fig7_extra

pdf("SuppFig1_Neutral.pdf")
Supp1
dev.off()

gridExtra::grid.arrange(Focal_Back,FocalBack_Sym,ncol=2)







