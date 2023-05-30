 for year 2020

library(ggplot2)
library(plotrix)
library(dplyr)
df=data.frame(offers=c(20,88,31,158,230,114,9),branch=c("civil","Mechanical","E&E","E&C","CSE","ISE","Biotechnology"))
print(df)
hsize <- 2
df <- df %>% 
  mutate(x = hsize)
ggplot(df, aes(x = hsize, y = offers, fill = branch)) +
  geom_col(color = "black") +
  geom_text(aes(label = offers),
            position = position_stack(vjust = 0.5)) + coord_polar(theta = "y") +labs(title = "Branchwise Placements at NMAMIT (2019-20)")+ scale_fill_manual(values = 
                                                                                                                                                                c("red","blue","green","yellow","coral1" ,"pink","mediumorchid")) + xlim(c(0.2, hsize + 
                                                                                                                                                                                                                                             0.5)) + theme(panel.background = element_rect(fill = "white"),panel.grid = 
                                                                                                                                                                                                                                                             element_blank(),axis.title = element_blank(), axis.ticks = element_blank(),axis.text = 
                                                                                                                                                                                                                                                             element_blank()) 
df2=data.frame(companies=c("DELL","RBEI","Vlinder Labs","Mercedes Benz","Juniper","Mobiezy","Yokogawa","Laurus",
                           "AIBOD","Ritsumeikan","L&T"),offers=c(2,1,3,3,6,2,2,3,2,3,3))
df2
theme_set(theme_bw())
ggplot(df2, aes(x=companies, y=offers)) + 
  geom_bar(stat="identity", width=.5, fill= "blue" ) + 
  labs(title="Company-Wise interships at NMAMIT (2019-2020)") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
data3 <- c(1,1,2,2,7,1,5)
lab <- paste(round(data3/sum(data3) * 100, 2),"%")
pie3D(data3,col = rainbow(7),labels = lab,explode = 0.05,
      labelcex=1,theta=0.8,main="Branch-wise Internships at NMAMIT (2019-
20)",border="black")
legend(.70,1.05,c("CIVIL","ME","E&E","E&C","CS","IS","MCA"),cex=0.6,fill=rainbow(7))





for year 2021


  library(ggplot2)
library(plotrix)
library(tidyverse)
library(ggsci)
library(ggpubr)
library(ggrepel)
#Company wise internships
data5 <- c(2,1,2,1,1,2,5,3,1,8,4,1,7,2)
x <- c("sony India","Dell Technologie","mscripts","LeadSquared","Aptean India Pvt 
Ltd","Juniper Networks","ACI worldwid","Laurus Infosystems","Mobiezy","Diya 
Systems (Glowtouch)","Krishi Tantra","Bang Design Private Limited","Climber 
Knowledge & Careers Private Limited","Integrum Technologies")
barplot(data5,names.arg=x,ylab="Internship offers",col=heat.colors(6),cex.names = 
          0.55,las=3, main="Company-Wise Internship opportunities at NMAMIT (2020-
21)",border="black")

#graph 3-Branch wise Placements
df=data.frame(offers=c(356,25,219,68,109,19,214,121),branch=c("CSE","CIVIL","ISE","E
&E","ME","BIOTECH","E&C","MCA"))
df2<- df %>% 
  mutate(csum = rev(cumsum(rev(offers))), pos = offers/2 + lead(csum, 1), pos = 
           if_else(is.na(pos), offers/2, pos))
ggplot(df, aes(x = "" , y = offers, fill = (branch))) +geom_col(width = 1, color= 1) +coord_polar(theta = "y") +scale_fill_brewer(palette = "Pastel1")+geom_label_repel(data = df2,aes(y = pos, label = paste0(offers)),size = 4.5, nudge_x =1, show.legend = FALSE) +guides(fill = guide_legend(title = "Branch")) +
  theme_void()+labs(title = "Branch wise Placements at NMAMIT 2021-22") 




for year 2022

library(ggplot2)
library(plotrix)
library(tidyverse)
library(ggsci)
library(ggpubr)
data=read.csv("cwp.csv")
print(data)
ggplot(data,aes(Companies,No.of.Offers)) + geom_bar(stat="identity",width=0.5)+ scale_y_continuous(limits = c(0,100))+ 
  theme(axis.text.x=element_text(angle=90,size=6))+labs(title = "Company wise Placements at NMAMIT 2021-22")

#All year placements Line graph
data1=read.csv("ayp.csv")
print(data1)
ggplot(data=data1,mapping = aes(x= YEAR, y=Placement.Offers))+geom_point(size=5)+ geom_line(colour="red")+labs(title = "Placements at NMAMIT(2015-2022)")


#All Year Internship line-Graph
data2=read.csv("ayi.csv")
print(data2)
ggplot(data=data2,
       mapping = aes(x= YEAR, y=Internship.offers))+
  geom_point( size=5)+ geom_line(colour="red")+labs(title = "Internship Opportunities 
at NMAMIT (2018-2022)")

#Branch-wise internship pie chart
data3=read.csv("bwi.csv")
data3
class(data3)
p=round(data3$Total.Number.of.Selections/sum(data3$Total.Number.of.Selections)*100)
id1=paste(round(p),"%-",sep = "")
id2=data3$Branch
id3=paste(id1,id2)
id3
pie(data3$Total.Number.of.Selections,main = "Branch-Wise Internships at NMAMIT 
(2021-22)",col = rainbow(9),id3,border='white')

#Branch-wise placements 3D pie chart
library(plotrix)
data4 <- c(219,356,68,109,19,25,214,121)
lab <- paste(round(data4/sum(data4) * 100, 2),"%")
pie3D(data4,col = rainbow(8),labels = lab,explode = 0.05,labelcex=1.15,theta=0.8,main="Branch-wise Placements at NMAMIT (2021-22)",border="white")
legend(.40,1.05,c("ISE","CSE","E&E","ME","BIOTECH","CIVIL","E&C","MCA"),cex=0.65,fill=rainbow(8))


#company-wise internships Bar Graph
data5 <- c(2,2,5,2,14,4,4,13,4,2,5,1,1,4,1,2)
x <- c("Nutanix","Robert Bosch","ACI Worldwide","Informatica","JupiterNetworks","Amazon","Laurus systems","Utthunga Tech"," Mobiezy","Triphase 
Tech","All State","Yokogawa","Navya Tech","Prodigy Tech","Nextuple","Tayana 
Software")
barplot(data5,names.arg=x,ylab="Internship offers",col=rainbow(8),cex.names = 
          0.55,las=3, main="Company-Wise Internship opportunities at NMAMIT (2021-
22)",border="black")

