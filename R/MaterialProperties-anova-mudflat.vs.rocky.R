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
sink('output/MaterialProperties.anova-mudflat.rocky.txt')
pdf('output/MaterialProperties-mudflat.rocky.pdf')
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

### mudflat vs rockyshore
br$MudRock <- meta$SiteA[match(br$site,meta$field_site_code_2015)]
br$MudRock <- factor(br$MudRock,levels=c("rockyshore","mudflat"))
#### peak break ###
print("all thalli")
m <- lmer(log(peak_force)~natnon*MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(peak_force)~natnon+MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=peak_force,x=natnon,fill=MudRock)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Peak force") + xlab("") +
  theme_classic()
print(f)

#### maxstress ###
m <- lmer(log(maxstress)~natnon*MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstress)~natnon+MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=maxstress,x=natnon,fill=MudRock)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Max stress") + xlab("") +
  theme_classic()
print(f)

#### maxstrain ###
m <- lmer(log(maxstrain)~natnon*MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstrain)~natnon+MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=maxstrain,x=natnon,fill=MudRock)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Max strain") + xlab("") +
  theme_classic()
print(f)

#### auc_modulus ###
m <- lmer(log(auc_modulus)~natnon*MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(auc_modulus)~natnon+MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=auc_modulus,x=natnon,fill=MudRock)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("auc_modulus") + xlab("") +
  theme_classic()
print(f)

#### slope_Mpa ###
m <- lmer(log(slope_Mpa)~natnon*MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(slope_Mpa)~natnon+MudRock+(1|site), data=br) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
f <- ggplot(br,aes(y=slope_Mpa,x=natnon,fill=MudRock)) +
  geom_boxplot() +
  scale_fill_manual(values=c("white","grey")) +
  ylab("slope_Mpa") + xlab("") +
  theme_classic()
print(f)
dev.off()
####  only tetrasporophytes

br2 <- br[br$Life_History2=="tetrasporophyte",]
print("tetrasporophytes only")
m <- lmer(log(peak_force)~natnon*MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(peak_force)~natnon+MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstress)~natnon*MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstress)~natnon+MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstrain)~natnon*MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(maxstrain)~natnon+MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(auc_modulus)~natnon*MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(auc_modulus)~natnon+MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(slope_Mpa)~natnon*MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
m <- lmer(log(slope_Mpa)~natnon+MudRock+(1|site), data=br2) 
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))

sink()

### print 5-paneled figure - means of popns

### breakage
tmp <- melt(br[,c("site","natnon","MudRock","peak_force")])
#tmp2 <- cast(tmp,site+natnon+MudRock~variable,mean,na.rm=T)
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+natnon+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+MudRock~variable,sd,na.rm=T)$peak_force
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)


f1 <- ggplot(tmp2,aes(y=peak_force,x=natnon,fill=MudRock,ymin=peak_force-se,ymax=peak_force+se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("peak force") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

### slope
tmp <- melt(br[,c("site","natnon","MudRock","slope_Mpa")])
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+natnon+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+MudRock~variable,sd,na.rm=T)$slope_Mpa
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f2 <- ggplot(tmp2,aes(y=slope_Mpa,x=natnon,fill=MudRock,ymin=slope_Mpa-se,ymax=slope_Mpa+se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +  scale_fill_manual(values=c("white","grey")) +
  ylab("Slope") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

### maxstress
tmp <- melt(br[,c("site","natnon","MudRock","maxstress")])
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+natnon+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+MudRock~variable,sd,na.rm=T)$maxstress
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f3 <- ggplot(tmp2,aes(y=maxstress,x=natnon,fill=MudRock,ymin=maxstress-se,ymax=maxstress+se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +  scale_fill_manual(values=c("white","grey")) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Max Stress") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

### maxstrain
tmp <- melt(br[,c("site","natnon","MudRock","maxstrain")])
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+natnon+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+MudRock~variable,sd,na.rm=T)$maxstrain
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f4 <- ggplot(tmp2,aes(y=maxstrain,x=natnon,fill=MudRock,ymin=maxstrain-se,ymax=maxstrain+se)) +
  geom_boxplot(outlier.shape=NA) + 
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +  scale_fill_manual(values=c("white","grey")) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("Max Strain") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()
#print(f4)

### auc_modulus
tmp <- melt(br[,c("site","natnon","MudRock","auc_modulus")])
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+natnon+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+natnon+MudRock~variable,sd,na.rm=T)$auc_modulus
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f5 <- ggplot(tmp2,aes(y=auc_modulus,x=natnon,fill=MudRock,ymin=auc_modulus-se,ymax=auc_modulus+se)) +
  geom_boxplot(outlier.shape=NA) + 
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) +  scale_fill_manual(values=c("white","grey")) +
  scale_fill_manual(values=c("white","grey")) +
  ylab("AUC") + xlab("") +
  guides(fill=FALSE) +
  theme_classic()

png('output/MaterialProperties-mudflat.rocky.png',width=5,height=9,units="in",res=700)
grid.arrange(f1,f2,f3,f4,f5,nrow=3,ncol=2)
dev.off()

### print 5-paneled figure - means of popns
### INCLUDE REGIONS AS BARS


#tmp <- melt(br[,c("site","natnon","MudRock","peak_force")])
#f1 <- ggplot(tmp2,aes(y=peak_force,x=natnon,fill=MudRock)) +
#  geom_boxplot() +
#  geom_point(pch = 21, position = position_jitterdodge(jitter.width=0.1)) +
#  scale_fill_manual(values=c("white","grey")) +
#  ylab("peak force") + xlab("") +
#  guides(fill=FALSE) +
#  theme_classic()

### breakage
tmp <- melt(br[,c("site","Continent","natnon","MudRock","peak_force")])
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+Continent+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+MudRock~variable,sd,na.rm=T)$peak_force
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f1 <- ggplot(tmp2,aes(y=peak_force,x=Continent,fill=MudRock,ymax=peak_force+se,ymin=peak_force-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("peak force") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[1,]) +
  annotate("text", x=c(2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[2,2]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[4,])

### slope
tmp <- melt(br[,c("site","Continent","natnon","MudRock","slope_Mpa")])
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+Continent+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+MudRock~variable,sd,na.rm=T)$slope_Mpa
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f2 <- ggplot(tmp2,aes(y=slope_Mpa,x=Continent,fill=MudRock,ymax=slope_Mpa+se,ymin=slope_Mpa-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("Slope") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[1,]) +
  annotate("text", x=c(2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[2,2]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[4,])

### maxstress
tmp <- melt(br[,c("site","Continent","natnon","MudRock","maxstress")])
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+Continent+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+MudRock~variable,sd,na.rm=T)$maxstress
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)
f3 <- ggplot(tmp2,aes(y=maxstress,x=Continent,fill=MudRock,ymax=maxstress+se,ymin=maxstress-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("Max stress") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[1,]) +
  annotate("text", x=c(2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[2,2]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[4,])

### maxstrain
tmp <- melt(br[,c("site","Continent","natnon","MudRock","maxstrain")])
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+Continent+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+MudRock~variable,sd,na.rm=T)$maxstrain
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f4 <- ggplot(tmp2,aes(y=maxstrain,x=Continent,fill=MudRock,ymax=maxstrain+se,ymin=maxstrain-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("Max strain") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[1,]) +
  annotate("text", x=c(2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[2,2]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[4,])

### auc_modulus
tmp <- melt(br[,c("site","Continent","natnon","MudRock","auc_modulus")])
tmp$site_MudRock <- paste(tmp$site,tmp$MudRock)
tmp2 <- cast(tmp,site+Continent+MudRock~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site+Continent+MudRock~variable,sd,na.rm=T)$auc_modulus
tmp2$n <- table(tmp$site_MudRock)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)

f5 <- ggplot(tmp2,aes(y=auc_modulus,x=Continent,fill=MudRock,ymax=auc_modulus+se,ymin=auc_modulus-se)) +
  geom_boxplot() +
  geom_pointrange(pch = 21, cex=0.3, position = position_jitterdodge(jitter.width=0.5)) + 
  scale_fill_manual(values=c("white","grey")) +
  ylab("AUC") + xlab("") +
  guides(fill=FALSE) +
  theme_classic() + theme(legend.position = "none") +
  annotate("text", x=c(.8,1.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[1,]) +
  annotate("text", x=c(2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[2,2]) +
  annotate("text", x=c(2.8,3.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[3,]) +
  annotate("text", x=c(3.8,4.2), y=0, label= table(tmp2$Continent,tmp2$MudRock)[4,])

png('output/MaterialProperties-mudflat.rocky-Continents.png',width=8,height=9,units="in",res=700)
grid.arrange(f1,f2,f3,f4,f5,nrow=3,ncol=2)
dev.off()


