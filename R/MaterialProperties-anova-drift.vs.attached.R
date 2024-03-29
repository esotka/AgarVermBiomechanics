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
sink('output/MaterialProperties.anova-drift.vs.attached.txt')
pdf('output/MaterialProperties-drift.vs.attached.pdf')
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
br$attach3[as.character(br$attachment)%in%c("buried","diopatra","drift","drift_buried","drift_partly_buried")] <- "drift"
br$attach3[as.character(br$attachment)%in%c("attached","bedrock","bedrock_Tide_pool","buried_attached","buried_rock","h_with_sand","hard_rock","large_pebbles","large_rock","large_stones","oyster","p_with_sand","pebble","pebbles","rock","rope","shell","stick","tiny_pebble","wood")] <- "attach"
br$attach3 <- factor(br$attach3)
br$natnon <- meta$NatNon[match(br$site,meta$field_site_code_2015)]
br$natnon <- factor(br$natnon,levels=c("Japan","Non-native"))

#### peak break ###
print("all thalli")
m <- lmer(log(peak_force)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(peak_force)~attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(peak_force)~natnon+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=peak_force,x=natnon,fill=attach3)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Breaking force (N)") + xlab("") +
  theme_classic()
print(f)

#### maxstress ###
m <- lmer(log(maxstress)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstress)~attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstress)~natnon+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=maxstress,x=natnon,fill=attach3)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("maxstress") + xlab("") +
  theme_classic()
print(f)

#### maxstrain ###
m <- lmer(log(maxstrain)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstrain)~attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstrain)~natnon+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=maxstrain,x=natnon,fill=attach3)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("maxstrain") + xlab("") +
  theme_classic()
print(f)

#### auc_modulus ###
m <- lmer(log(auc_modulus)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(auc_modulus)~attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(auc_modulus)~natnon+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=auc_modulus,x=natnon,fill=attach3)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("auc_modulus") + xlab("") +
  theme_classic()
print(f)

#### slope_Mpa ###
m <- lmer(log(slope_Mpa)~natnon+attach3+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(slope_Mpa)~attach3+(1|site), data=br)
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(slope_Mpa)~natnon+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=slope_Mpa,x=natnon,fill=attach3)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("slope_Mpa") + xlab("") +
  theme_classic()
print(f)
dev.off()
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

### print 5-paneled figure - means of popns

### breakage
tmp <- melt(br[,c("site","natnon","attach3","peak_force")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+attach3~variable,sd,na.rm=T)$peak_force
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)
f1 <- ggplot(tmp2,aes(y=peak_force,x=natnon,fill=attach3,ymin=peak_force-se,ymax=peak_force+se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Breaking force (N)") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

### slope
tmp <- melt(br[,c("site","natnon","attach3","slope_Mpa")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+attach3~variable,sd,na.rm=T)$slope_Mpa
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)
f2 <- ggplot(tmp2,aes(y=slope_Mpa,x=natnon,fill=attach3,ymin=slope_Mpa-se,ymax=slope_Mpa+se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +scale_fill_manual(values=c("white","grey")) +
  ylab("Modulus (MPa)") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

### maxstress
tmp <- melt(br[,c("site","natnon","attach3","maxstress")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+attach3~variable,sd,na.rm=T)$maxstress
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f3 <- ggplot(tmp2,aes(y=maxstress,x=natnon,fill=attach3,ymin=maxstress-se,ymax=maxstress+se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +scale_fill_manual(values=c("white","grey")) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Peak Stress (MPa)") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

### maxstrain
tmp <- melt(br[,c("site","natnon","attach3","maxstrain")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+attach3~variable,sd,na.rm=T)$maxstrain
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)


f4 <- ggplot(tmp2,aes(y=maxstrain,x=natnon,fill=attach3,ymin=maxstrain-se,ymax=maxstrain+se)) +
  geom_boxplot(outlier.shape=NA) + 
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +scale_fill_manual(values=c("white","grey")) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Peak Strain") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()
#print(f4)

### auc_modulus
tmp <- melt(br[,c("site","natnon","attach3","auc_modulus")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+attach3~variable,sd,na.rm=T)$auc_modulus
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f5 <- ggplot(tmp2,aes(y=auc_modulus,x=natnon,fill=attach3,ymin=auc_modulus-se,ymax=auc_modulus+se)) +
  geom_boxplot(outlier.shape=NA) + 
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +scale_fill_manual(values=c("white","grey")) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("AUC (MJ/m3)") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()
png('output/MaterialProperties-drift.vs.attached.png',width=5,height=9,units="in",res=700)
grid.arrange(f1,f2,f3,f4,f5,nrow=3,ncol=2)
dev.off()

### print 5-paneled figure - means of popns
### INCLUDE REGIONS AS BARS

### breakage
tmp <- melt(br[,c("site","Continent","natnon","attach3","peak_force")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+Continent+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+attach3~variable,sd,na.rm=T)$peak_force
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f1 <- ggplot(tmp2,aes(y=peak_force,x=Continent,fill=attach3,ymax=peak_force+se,ymin=peak_force-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("Breaking force (N)") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[1,]) +
  annotate("text", x=c(1.8,2.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[2,]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[4,])

### slope
tmp <- melt(br[,c("site","Continent","natnon","attach3","slope_Mpa")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+Continent+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+attach3~variable,sd,na.rm=T)$slope_Mpa
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f2 <- ggplot(tmp2,aes(y=slope_Mpa,x=Continent,fill=attach3,ymax=slope_Mpa+se,ymin=slope_Mpa-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("Modulus (MPa)") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[1,]) +
  annotate("text", x=c(1.8,2.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[2,]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[4,])

### maxstress
tmp <- melt(br[,c("site","Continent","natnon","attach3","maxstress")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+Continent+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+attach3~variable,sd,na.rm=T)$maxstress
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)
f3 <- ggplot(tmp2,aes(y=maxstress,x=Continent,fill=attach3,ymax=maxstress+se,ymin=maxstress-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("Peak Stress (MPa)") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[1,]) +
  annotate("text", x=c(1.8,2.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[2,]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[4,])

### maxstrain
tmp <- melt(br[,c("site","Continent","natnon","attach3","maxstrain")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+Continent+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+attach3~variable,sd,na.rm=T)$maxstrain
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f4 <- ggplot(tmp2,aes(y=maxstrain,x=Continent,fill=attach3,ymax=maxstrain+se,ymin=maxstrain-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("Peak Strain") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[1,]) +
  annotate("text", x=c(1.8,2.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[2,]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[4,])

### auc_modulus
tmp <- melt(br[,c("site","Continent","natnon","attach3","auc_modulus")])
tmp$site_attach <- paste(tmp$site,tmp$attach3)
tmp2 <- cast(tmp,site+Continent+natnon+attach3~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+attach3~variable,sd,na.rm=T)$auc_modulus
tmp2$n <- table(tmp$site_attach)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)
f5 <- ggplot(tmp2,aes(y=auc_modulus,x=Continent,fill=attach3,ymax=auc_modulus+se,ymin=auc_modulus-se)) +
  geom_boxplot() +
  geom_point(pch = 21, position = position_jitterdodge(jitter.width=0.1)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("AUC (MJ/m3)") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[1,]) +
  annotate("text", x=c(1.8,2.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[2,]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$attach3)[4,])

png('output/MaterialProperties-drift.vs.attached-Continents.png',width=8,height=9,units="in",res=700)
grid.arrange(f1,f2,f3,f4,f5,nrow=3,ncol=2)
dev.off()


