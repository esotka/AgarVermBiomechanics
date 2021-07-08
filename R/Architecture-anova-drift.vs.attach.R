### analyses of drift vs attached thalli of architecture data.
rm(list=ls())
library(ggplot2)
library(reshape)
library(visreg)
library(lmerTest)

### site Metadata
meta <- read.csv('data/siteMeta.csv')
meta$Continent <- factor(meta$Continent)
tmp <- data.frame(long=levels(meta$Continent),short=c("Japan","wNA","eNA","Europe"))

########################
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
arch$natnon <- factor(arch$natnon,levels=c("Japan","Introduced"))

#### DO ANOVAS #####
y <- c("sumlength","sumprojarea","sumavediam","sumrtvol")
y.n <- c("Length","Surfacearea","Diameter","Volume")

### note: there were no significant interactions; so, i removed the interaction term
sink('output/Architecture.anova-drift.vs.attached.txt')
for (i in 1:4)
{
  y.n.tmp <- arch[,y.n[i]]
  tmp <- arch[y.n.tmp=="Y",]
  m <- lmer(log(tmp[,y[i]])~natnon+attachment.status2+(1|site), data=tmp) 
  print(y[i])
  print("all thalli")
  print(table(tmp$natnon,tmp$attachment.status2))
  print(summary(m)$call)
  print(summary(m)$coefficients); print(anova(m))
  print("reproductive tetrasporophytes only")
  tmp <- arch[y.n.tmp=="Y" & arch$StacyLifehistory=="Tetrasporophyte",]
  m <- lmer(log(tmp[,y[i]])~natnon+attachment.status2+(1|site), data=tmp) 
  print(table(tmp$natnon,tmp$attachment.status2))
  print(summary(m)$call)
  print(summary(m)$coefficients); print(anova(m))
  
}

sink()

