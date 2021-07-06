### pretty plot - surface area
rm(list=ls())
library(lmerTest)
library(ggplot2)
library(reshape)
library(gridExtra)
### site Metadata
meta <- read.csv('data/siteMeta.csv')
meta$NatNon <- as.character(meta$NatNon)
meta$NatNon[meta$NatNon=="Introduced"] <- "Non-native"


#######################
### architecture data ###
########################
arch <- read.csv('data/architecture.csv')
arch$site <- tolower(substr(arch$Individual_ID,1,3))
arch$Continent <- meta$Continent[match(arch$site,meta$field_site_code_2015)]
arch$Continent <- as.character(arch$Continent)
arch$Continent[arch$Continent=="NorthAmericaWest"] <- "Western NA"
arch$Continent[arch$Continent=="NorthAmericaEast"] <- "Eastern NA"
arch$Continent <- factor(arch$Continent)
arch$Continent <- factor(arch$Continent,levels=levels(arch$Continent)[c(3,4,1,2)])
print("architecture data")

### criteria for drift vs attached
arch$attachment.status2 <- c()
arch$attachment.status2[as.character(arch$attachment.status)%in%c("Buried","Diopatra","Drift","Drift/Buried","Drift/partly Buried","Drifts")] <- "drift"
arch$attachment.status2[as.character(arch$attachment.status)%in%c("Attached","bedrock","bedrock/TL","Bedrock/TL","Buried Attached","Buried rock","","gastropod","H with sand","hard rock","holdfast","large rock","large stones","pebble","pebbles","plastic","rock","snail")] <- "attach"
arch <- arch[!arch$attachment.status=="Unknown",]
arch <- arch[!is.na(arch$attachment.status2),]
arch$attachment.status2 <- factor(arch$attachment.status2)
arch$natnon <- meta$NatNon[match(arch$site,meta$field_site_code_2015)]
arch$natnon <- factor(arch$natnon,levels=c("Japan","Non-native"))
arch$latitude <- meta$lat[match(arch$site,meta$field_site_code_2015)]
arch$JanSST <- meta$JanSST[match(arch$site,meta$field_site_code_2015)]

# anova
tmp <- arch[arch$Surfacearea=="Y",]
m <- lmer(log(tmp$sumprojarea)~natnon+attachment.status2+(1|site), data=tmp) 
print("all thalli")
print(table(tmp$natnon,tmp$attachment.status2))
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))

### pretty plot of all data
tmp2 <- melt(tmp[,c("site","natnon","attachment.status2","sumprojarea")])
tmp2$site_attach <- paste(tmp2$site,tmp2$attachment.status2)
tmp3 <- cast(tmp2,site+natnon+attachment.status2~variable,mean,na.rm=T)
tmp3$sd <- cast(tmp2,site+natnon+attachment.status2~variable,sd,na.rm=T)$sumprojarea
tmp3$n <- table(tmp2$site_attach)
tmp3$se <- (tmp3$sd)/sqrt(tmp3$n)

f1 <- ggplot(tmp3,aes(y=sumprojarea,x=natnon,fill=attachment.status2,ymax=sumprojarea+se,ymin=sumprojarea-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Projected Area") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

### pretty plot of tetrasporophytes
tmp2 <- tmp[tmp$Surfacearea=="Y"& tmp$StacyLifehistory=="Tetrasporophyte",]
tmp2 <- melt(tmp2[,c("site","natnon","attachment.status2","sumprojarea")])
#tmp2 <- cast(tmp,site+natnon+attachment.status2~variable,mean,na.rm=T)

tmp2$site_attach <- paste(tmp2$site,tmp2$attachment.status2)
tmp3 <- cast(tmp2,site+natnon+attachment.status2~variable,mean,na.rm=T)
tmp3$sd <- cast(tmp2,site+natnon+attachment.status2~variable,sd,na.rm=T)$sumprojarea
tmp3$n <- table(tmp2$site_attach)
tmp3$se <- (tmp3$sd)/sqrt(tmp3$n)



f2 <- ggplot(tmp3,aes(y=sumprojarea,x=natnon,fill=attachment.status2,ymax=sumprojarea+se,ymin=sumprojarea-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Projected Area") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

png('output/SurfaceArea-prettyPlot-drift.vs.attached.png',width=8,height=9,units="in",res=700)
grid.arrange(f1,f2,nrow=3,ncol=2)
dev.off()
