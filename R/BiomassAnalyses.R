### Is biomass give equivalent results as does surface area?
rm(list=ls())
# Correlation between SA and wet biomass
arch <- read.csv("data/architecture.csv")
biom <- read.csv("data/biomass.csv")
arch$biomass <- biom$wetmass[match(arch$Individual_ID,biom$sampleID)]
res <- cor.test(arch$biomass, arch$sumprojarea, 
                method = "pearson")
print(res)

## are the ANOVAs similar between SA and wet biomass?
library(lmerTest)

### site Metadata
meta <- read.csv('data/siteMeta.csv')
meta$Continent <- factor(meta$Continent)
tmp <- data.frame(long=levels(meta$Continent),short=c("Japan","wNA","eNA","Europe"))

### drift vs attached
arch$site <- tolower(substr(arch$Individual_ID,1,3))
arch$Continent <- meta$Continent[match(arch$site,meta$field_site_code_2015)]
arch$Continent <- as.character(arch$Continent)
arch$Continent[arch$Continent=="NorthAmericaWest"] <- "Western NA"
arch$Continent[arch$Continent=="NorthAmericaEast"] <- "Eastern NA"
arch$Continent <- factor(arch$Continent)
arch$Continent <- factor(arch$Continent,levels=levels(arch$Continent)[c(3,4,1,2)])

arch$attachment.status2 <- c()
arch$attachment.status2[as.character(arch$attachment.status)%in%c("Buried","Diopatra","Drift","Drift/Buried","Drift/partly Buried","Drifts")] <- "drift"
arch$attachment.status2[as.character(arch$attachment.status)%in%c("Attached","bedrock","bedrock/TL","Bedrock/TL","Buried Attached","Buried rock","","gastropod","H with sand","hard rock","holdfast","large rock","large stones","pebble","pebbles","plastic","rock","snail")] <- "attach"
arch <- arch[!arch$attachment.status=="Unknown",]
arch <- arch[!is.na(arch$attachment.status2),]
arch$attachment.status2 <- factor(arch$attachment.status2)
arch$natnon <- meta$NatNon[match(arch$site,meta$field_site_code_2015)]
arch$natnon <- factor(arch$natnon,levels=c("Japan","Introduced"))

y <- c("sumprojarea","biomass")
#y.n <- c("Length","Surfacearea","Diameter","Volume")

### note: there were no significant interactions; so, i removed the interaction term
for (i in 1:2)
{
print(y[i])
  tmp <- arch[!is.na(arch[y[i]]),]
  tmp <- tmp[tmp$Surfacearea=="Y",]
m <- lmer(log(tmp[,y[i]])~natnon+attachment.status2+(1|site), data=tmp) 
print("all thalli")
print(table(tmp$natnon,tmp$attachment.status2))
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
print("reproductive tetrasporophytes only")
tmp <- tmp[tmp$StacyLifehistory=="Tetrasporophyte",]
m <- lmer(log(tmp[,y[i]])~natnon+attachment.status2+(1|site), data=tmp) 
print(table(tmp$natnon,tmp$attachment.status2))
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
}

#### ### surface area by Jan SST
arch <- read.csv("data/architecture.csv")
biom <- read.csv("data/biomass.csv")
arch$biomass <- biom$wetmass[match(arch$Individual_ID,biom$sampleID)]

arch$site <- tolower(substr(arch$Individual_ID,1,3))
arch$Continent <- meta$Continent[match(arch$site,meta$field_site_code_2015)]
arch$Continent <- as.character(arch$Continent)
arch$Continent[arch$Continent=="NorthAmericaWest"] <- "Western NA"
arch$Continent[arch$Continent=="NorthAmericaEast"] <- "Eastern NA"
arch$Continent <- factor(arch$Continent)
arch$Continent <- factor(arch$Continent,levels=levels(arch$Continent)[c(3,4,1,2)])

#### remove western NA #####

arch$latitude <- meta$lat[match(arch$site,meta$field_site_code_2015)]
arch$JanSST <- meta$JanSST[match(arch$site,meta$field_site_code_2015)]
tmp <- arch[arch$Surfacearea=="Y" & ! arch$Continent=="Western NA",]
m <- lmer(log(tmp[,"sumprojarea"])~JanSST*Continent+(1|site), data=tmp)
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
print("reproductive tetrasporophytes only")
tmp <- arch[arch$Surfacearea=="Y"& arch$StacyLifehistory=="Tetrasporophyte" &  ! arch$Continent=="Western NA",]
m <- lmer(log(tmp[,"sumprojarea"])~JanSST*Continent+(1|site), data=tmp)
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))

## biomass version
tmp <- arch[arch$Surfacearea=="Y" & ! arch$Continent=="Western NA",]
m <- lmer(log(tmp[,"biomass"])~JanSST*Continent+(1|site), data=tmp)
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
print("reproductive tetrasporophytes only")
tmp <- arch[arch$Surfacearea=="Y"& arch$StacyLifehistory=="Tetrasporophyte" &  ! arch$Continent=="Western NA",]
m <- lmer(log(tmp[,"biomass"])~JanSST*Continent+(1|site), data=tmp)
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))




