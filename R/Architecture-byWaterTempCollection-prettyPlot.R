### surface area by Jan SST

rm(list=ls())
library(lmerTest)
library(ggplot2)
library(reshape)
library(gridExtra)
library(visreg)
### site Metadata
meta <- read.csv('data/siteMeta.csv')
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
arch$Collection.waterTemp <- meta$Collection.waterTemp[match(arch$site,meta$field_site_code_2015)]
#### DO ANOVAS - Length #####
#### remove western NA #####
tmp <- arch[arch$Length=="Y" & ! arch$Continent.short=="wNA",]
m <- lmer(log(tmp[,"sumlength"])~Collection.waterTemp*Continent.short+(1|site), data=tmp)
print("all thalli")
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
print("reproductive tetrasporophytes only")
tmp <- arch[arch$Length=="Y"& arch$StacyLifehistory=="Tetrasporophyte" & !arch$Continent.short=="wNA",]
m <- lmer(log(tmp[,"sumlength"])~Collection.waterTemp*Continent.short+(1|site), data=tmp)
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))

### plot - 3 continents
### all thalli
tmp <- arch[arch$Length=="Y" & ! arch$Continent.short=="wNA",]
tmp <- melt(tmp[,c("site","natnon","Continent.short","sumlength")])
tmp2 <- cast(tmp,site+natnon+Continent.short~variable,mean,na.rm=T)
tmp2$Collection.waterTemp <- meta$Collection.waterTemp[match(tmp2$site,meta$field_site_code_2015)]
fit <- lm(sumlength~Continent.short*Collection.waterTemp,data=tmp2)
visreg(fit,"Collection.waterTemp",by="Continent.short")#,overlay=T)

### Reproductive thalli only
tmp <- arch[arch$Surfacearea=="Y" & arch$StacyLifehistory=="Tetrasporophyte" & ! arch$Continent.short=="wNA",]
tmp <- melt(tmp[,c("site","natnon","Continent.short","sumlength")])
tmp2 <- cast(tmp,site+natnon+Continent.short~variable,mean,na.rm=T)
tmp2$Collection.waterTemp <- meta$Collection.waterTemp[match(tmp2$site,meta$field_site_code_2015)]
fit <- lm(sumlength~Continent.short*Collection.waterTemp,data=tmp2)
visreg(fit,"Collection.waterTemp",by="Continent.short")#,overlay=T)


#### DO ANOVAS - surface area #####
#### remove western NA #####
tmp <- arch[arch$Surfacearea=="Y" & ! arch$Continent.short=="wNA",]
m <- lmer(log(tmp[,"sumprojarea"])~Collection.waterTemp*Continent.short+(1|site), data=tmp)
print("all thalli")
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
print("reproductive tetrasporophytes only")
tmp <- arch[arch$Surfacearea=="Y"& arch$StacyLifehistory=="Tetrasporophyte" & !arch$Continent.short=="wNA",]
m <- lmer(log(tmp[,"sumprojarea"])~Collection.waterTemp*Continent.short+(1|site), data=tmp)
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))

### plot - 3 continents
### all thalli
tmp <- arch[arch$Surfacearea=="Y" & ! arch$Continent.short=="wNA",]
tmp <- melt(tmp[,c("site","natnon","Continent.short","sumprojarea")])
tmp2 <- cast(tmp,site+natnon+Continent.short~variable,mean,na.rm=T)
tmp2$Collection.waterTemp <- meta$Collection.waterTemp[match(tmp2$site,meta$field_site_code_2015)]
fit <- lm(sumprojarea~Continent.short*Collection.waterTemp,data=tmp2)
visreg(fit,"Collection.waterTemp",by="Continent.short")#,overlay=T)

### Reproductive thalli only
tmp <- arch[arch$Surfacearea=="Y" & arch$StacyLifehistory=="Tetrasporophyte" & ! arch$Continent.short=="wNA",]
tmp <- melt(tmp[,c("site","natnon","Continent.short","sumprojarea")])
tmp2 <- cast(tmp,site+natnon+Continent.short~variable,mean,na.rm=T)
tmp2$Collection.waterTemp <- meta$Collection.waterTemp[match(tmp2$site,meta$field_site_code_2015)]
fit <- lm(sumprojarea~Continent.short*Collection.waterTemp,data=tmp2)
visreg(fit,"Collection.waterTemp",by="Continent.short")#,overlay=T)





