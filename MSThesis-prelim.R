library(dplyr)
library(tidyr)
library(ggplot2)

MBcounts<-read.csv("~/MS Thesis/MBcounts.csv",header=TRUE)
MBcounts<-MBcounts %>% mutate(SB_prop=SB/total,DB_prop=MB/total)
MBcounts<-MBcounts %>% pivot_longer(cols=c(SB_prop,DB_prop),names_to="broodcount",values_to="proportion")
MBcounts$broodcount<-as.factor(MBcounts$broodcount)

SBcounts<-MBcounts%>%filter(broodcount=="SB_prop")
SBmodel<-lm(proportion~year,data=SBcounts); summary(SBmodel)


ggplot(data=SBcounts, aes(x=year,y=proportion)) +
  geom_point() + stat_smooth(method="lm",formula=y~x,geom="smooth") + ylab("Proportion of single broods") +
  xlab("Year")

ggplot(data=MBcounts, aes(x=year,y=proportion,fill=broodcount))  +
  geom_bar(stat="identity") + stat_smooth(method="lm",formula=y~x,geom="smooth",data=SBcounts)



LD_RS<-read.csv("~/MS Thesis/LD_RS.csv",header=TRUE,na.strings=c("","NA"))
LD_RS$nestcount<-as.factor(LD_RS$nestcount)

ggplot(data=LD_RS, aes(x=LD, y=RSbinary, na.rm=TRUE)) +
  geom_point() + geom_smooth(method="glm", method.args=list(family="binomial")) +
  facet_wrap( ~ year, ncol=2)


model2<-lm(RSbinary~nestcount,data=LD_RS); summary(model2)
ggplot(data=LD_RS, aes(x=nestcount, y=RSbinary)) +
  geom_jitter() + stat_smooth(method="lm",formula=y~x,geom="smooth",data=LD_RS)


model3<-lm(RS~nestcount,data=LD_RS); summary(model3)
ggplot(data=LD_RS, aes(x=nestcount, y=RS)) +
  geom_jitter() + stat_smooth(method="lm",formula=y~x,geom="smooth",data=LD_RS)


model4<-lm(RS~LD,data=LD_RS); summary(model4)
ggplot(data=LD_RS, aes(x=LD, y=RS)) +
  geom_jitter() + stat_smooth(method="lm",formula=y~x,geom="smooth",data=LD_RS)



model5<-lm(RS~year,data=LD_RS); summary(model5)
ggplot(data=LD_RS, aes(x=year, y=RS)) +
  geom_jitter() + stat_smooth(method="lm",formula=y~x,geom="smooth",data=LD_RS)


