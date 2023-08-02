library(tidyverse)

setwd("/Users/amywebster/OneDrive - University Of Oregon/Aim2")

model_dominanceCoeff<-read.csv("model_DominanceCoeff.csv",header = T)

model_dominanceCoeff_2<-model_dominanceCoeff[,c(3,4,15,21,22,23)]

pivot_model_dom<-model_dominanceCoeff_2%>%
  pivot_longer(cols = c(4,5,6),names_to = "type_of_estimate",values_to = "equil")

ggplot(pivot_model_dom,aes(x=h,y=log2(equil),color=type_of_estimate))+
  geom_line()+facet_grid(epimutation_rate~backepi_rate,scales = "free_y")+
  theme_bw(base_size = 10)+theme(aspect.ratio = 1)+
  labs(x="Dominance coefficient (h)",y="log2(p2+p4)")

#Deviation from EBE

EBE_deviation<-read.csv("model_deviationfromEBE2.csv",header = T)
head(EBE_deviation)
ggplot(EBE_deviation,aes(x=w44-w22,y=p4,color=EBE_class))+
  geom_line(data=EBE_deviation,aes(x=w44-w22,y=EBE_exp),color="black")+
  geom_point(alpha=0.3)+facet_grid(epimutation~back_epimutation)+
  theme_bw(base_size = 10)+theme(aspect.ratio = 1)+
  scale_x_continuous(trans='log10')+
  labs(x="Selection coefficient",y="p4")

#EBE_dev_hcoeff<-read.csv("model_deviationfromEBE_hcoeff.csv",header = T)
#head(EBE_dev_hcoeff)
#ggplot(EBE_dev_hcoeff,aes(x=h,y=p4))+
#  geom_point()+
#  geom_line(data=EBE_dev_hcoeff,aes(x=h,y=EBE_exp),color="pink")+
#  facet_grid(epimutation~back_epimutation)+
#  theme_bw(base_size = 10)+theme(aspect.ratio = 1)





