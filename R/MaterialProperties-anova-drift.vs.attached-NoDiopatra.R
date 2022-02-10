### summary material properties
rm(list=ls())
library(ggplot2)
library(gridExtra)
library(lmerTest)
library(reshape)
### site Metadata
meta <- read.csv('data/siteMeta.csv')
meta$NatNon <- as.character(meta$NatNon)
meta$NatNon[meta$NatNon=="Introduced"] <- "Non-native"
meta$NatNon <- as.factor(meta$NatNon)
sink('output/MaterialProperties.anova-drift.vs.attached-NoDiopatra.txt')

#####################
### breakage data ###
#####################
br <- read.csv('data/breakage.csv')
br$site <- tolower(substr(br$sample_id,1,3))
br$Continent <- meta$Continent[match(br$site,meta$field_site_code_2015)]
br$Continent <- as.character(br$Continent)
br$Continent[br$Continent=="NorthAmericaWest"] <- "Western NA"
br$Continent[br$Continent=="NorthAmericaEast"] <- "Eastern NA"
br$Continent <- factor(br$Continent)
br$Continent <- factor(br$Continent,levels=levels(br$Continent)[c(3,4,1,2)])

### criteria for drift vs attached
br <- br[!br$attach2=="",]
br$attach3 <- c()
br$attach3[as.character(br$attachment)%in%c("buried","drift","drift_buried","drift_partly_buried")] <- "drift" # remove "diopatra"
br$attach3[as.character(br$attachment)%in%c("attached","bedrock","bedrock_Tide_pool","buried_attached","buried_rock","h_with_sand","hard_rock","large_pebbles","large_rock","large_stones","oyster","p_with_sand","pebble","pebbles","rock","rope","shell","stick","tiny_pebble","wood")] <- "attach"
br <- br[complete.cases(br$attach3),]
br$attach3 <- factor(br$attach3)
br$natnon <- meta$NatNon[match(br$site,meta$field_site_code_2015)]
br$natnon <- factor(br$natnon,levels=c("Japan","Non-native"))

#### peak break ###
print("all thalli")
m <- lmer(log(peak_force)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstress)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstrain)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(auc_modulus)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(slope_Mpa)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
####  only tetrasporophytes

br2 <- br[br$Life_History2=="tetrasporophyte",]
print("tetrasporophytes only")
m <- lmer(log(peak_force)~natnon+attach3+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstress)~natnon+attach3+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstrain)~natnon+attach3+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(auc_modulus)~natnon+attach3+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(slope_Mpa)~natnon+attach3+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))

sink()
