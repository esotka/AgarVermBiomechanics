### pretty plot - surface area
rm(list=ls())
library(lmerTest)
library(ggplot2)
library(reshape)
library(gridExtra)
### site Metadata
meta <- read.csv('data/siteMeta.csv')
tmp <- data.frame(long=levels(meta$Continent),short=c("Japan","wNA","eNA","Europe"))

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
arch$Continent.short <- tmp$short[match(arch$Continent,tmp$long)]
arch$Continent.short <- factor(arch$Continent.short,levels=c("Japan","wNA","eNA","Europe"))
arch$natnon <- meta$NatNon[match(arch$site,meta$field_site_code_2015)]
arch$natnon <- factor(arch$natnon,levels=c("Japan","Introduced"))
arch$latitude <- meta$lat[match(arch$site,meta$field_site_code_2015)]
arch$JanSST <- meta$JanSST[match(arch$site,meta$field_site_code_2015)]
#### DO ANOVAS #####
tmp <- arch[arch$Surfacearea=="Y",]
m <- lmer(log(tmp[,"sumprojarea"])~natnon+MudRock+(1|site), data=tmp) 
print("all thalli")
print(table(tmp$natnon,tmp$MudRock))
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
print("reproductive tetrasporophytes only")
tmp <- arch[arch$Surfacearea=="Y"& arch$StacyLifehistory=="Tetrasporophyte",]
m <- lmer(log(tmp[,"sumprojarea"])~natnon+MudRock+(1|site), data=tmp) 
print(table(tmp$natnon,tmp$MudRock))
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))

### pretty plot of all data
tmp <- arch[arch$Surfacearea=="Y",]
tmp <- melt(tmp[,c("site","natnon","MudRock","sumprojarea")])
tmp2 <- cast(tmp,site+natnon+MudRock~variable,mean,na.rm=T)
f1 <- ggplot(tmp2,aes(y=sumprojarea,x=natnon,fill=MudRock)) +
  geom_boxplot() +
  geom_point(pch = 21, position = position_jitterdodge(jitter.width=0.1)) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Projected Area") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

### pretty plot of tetrasporophytes
tmp <- arch[arch$Surfacearea=="Y"& arch$StacyLifehistory=="Tetrasporophyte",]
tmp <- melt(tmp[,c("site","natnon","MudRock","sumprojarea")])
tmp2 <- cast(tmp,site+natnon+MudRock~variable,mean,na.rm=T)
f2 <- ggplot(tmp2,aes(y=sumprojarea,x=natnon,fill=MudRock)) +
  geom_boxplot() +
  geom_point(pch = 21, position = position_jitterdodge(jitter.width=0.1)) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Projected Area") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

png('output/SurfaceArea-prettyPlot.png',width=8,height=9,units="in",res=700)
grid.arrange(f1,f2,nrow=3,ncol=2)
dev.off()
