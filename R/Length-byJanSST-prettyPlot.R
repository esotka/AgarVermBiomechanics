### surface area by Jan SST

rm(list=ls())
library(lmerTest)
library(ggplot2)
library(reshape)
library(gridExtra)
library(visreg)
### site Metadata
meta <- read.csv('data/siteMeta.csv')
meta$Continent <- factor(meta$Continent)
metatmp <- data.frame(long=levels(meta$Continent)[c(2,4,3,1)],short=c("Japan","wNA","eNA","Europe"))

########################
### architecture data ###
########################
arch <- read.csv('data/architecture.csv')
arch$site <- tolower(substr(arch$Individual_ID,1,3))
arch$Continent <- meta$Continent[match(arch$site,meta$field_site_code_2015)]
arch$Continent <- factor(arch$Continent,levels=levels(arch$Continent)[c(2,4,3,1)])
print("architecture data")
#print(table(arch$Continent,arch$StacyLifehistory))
arch$MudRock <- meta$SiteA[match(arch$site,meta$field_site_code_2015)]
arch$MudRock <- factor(arch$MudRock,levels=c("rockyshore","mudflat"))
#print(table(arch$Continent,arch$MudRock))
arch$Continent.short <- metatmp$short[match(arch$Continent,metatmp$long)]
arch$Continent.short <- factor(arch$Continent.short,levels=c("Japan","wNA","eNA","Europe"))
arch$natnon <- meta$NatNon[match(arch$site,meta$field_site_code_2015)]
arch$natnon <- factor(arch$natnon,levels=c("Japan","Introduced"))
arch$latitude <- meta$lat[match(arch$site,meta$field_site_code_2015)]
arch$JanSST <- meta$JanSST[match(arch$site,meta$field_site_code_2015)]
#### DO ANOVAS #####
#### remove western NA #####
tmp <- arch[arch$Length=="Y" & ! arch$Continent.short=="wNA",]
m <- lmer(log(tmp[,"sumlength"])~JanSST*Continent.short+(1|site), data=tmp)
print("all thalli")
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
print("reproductive tetrasporophytes only")
tmp <- arch[arch$Length=="Y"& arch$StacyLifehistory=="Tetrasporophyte" & !arch$Continent.short=="wNA",]
m <- lmer(log(tmp[,"sumlength"])~JanSST*Continent.short+(1|site), data=tmp)
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))

#pdf("output/Length-byJanSST-prettyPlot.pdf",width=5,height=5)
### plot - 3 continents
### all thalli
tmp <- arch[arch$Length=="Y" & ! arch$Continent.short=="wNA",]
tmp <- melt(tmp[,c("site","natnon","Continent.short","sumlength")])
tmp2 <- cast(tmp,site+natnon+Continent.short~variable,mean,na.rm=T)
tmp2$janSST <- meta$JanSST[match(tmp2$site,meta$field_site_code_2015)]

f1 <-  ggplot(data=tmp2, aes(x=janSST,y=sumlength)) +
  geom_point(size=2.0, aes(shape=Continent.short)) +
  scale_shape_manual(values=c(21,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent.short),size=.5,color="black") +
  theme_classic() +
  ylab("Total length") +
  xlab("January SST") +
  ylim(c(0,1500)) +
  theme(legend.position = "top")



png('output/Length-byJanSST-prettyPlot.png',width=5,height=9,units="in",res=700)
grid.arrange(f1,nrow=3,ncol=1)
dev.off()







fit <- lm(sumlength~Continent.short*janSST,data=tmp2)
visreg(fit,"janSST",by="Continent.short")#,overlay=T)

### Reproductive thalli only
tmp <- arch[arch$Length=="Y" & arch$StacyLifehistory=="Tetrasporophyte" & ! arch$Continent.short=="wNA",]
tmp <- melt(tmp[,c("site","natnon","Continent.short","sumlength")])
tmp2 <- cast(tmp,site+natnon+Continent.short~variable,mean,na.rm=T)
tmp2$janSST <- meta$JanSST[match(tmp2$site,meta$field_site_code_2015)]
fit <- lm(sumlength~Continent.short*janSST,data=tmp2)
visreg(fit,"janSST",by="Continent.short")#,overlay=T)

