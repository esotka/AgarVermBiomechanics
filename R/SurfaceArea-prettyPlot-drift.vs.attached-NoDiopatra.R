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
arch$attachment.status2[as.character(arch$attachment.status)%in%c("Buried","Drift","Drift/Buried","Drift/partly Buried","Drifts")] <- "drift" # remove "Diopatra"
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

tmp2 <- tmp[tmp$Surfacearea=="Y"& tmp$StacyLifehistory=="Tetrasporophyte",]
tmp2 <- melt(tmp2[,c("site","natnon","attachment.status2","sumprojarea")])
print("all thalli")
print(table(tmp2$natnon,tmp2$attachment.status2))
m <- lmer(log(tmp2$sumprojarea)~natnon+attachment.status2+(1|site), data=tmp2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))



