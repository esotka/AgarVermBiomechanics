### summary material properties
rm(list=ls())
library(ggplot2)
library(gridExtra)
library(lmerTest)
library(reshape)
### site Metadata
meta <- read.csv('data/siteMeta.csv')
sink('output/MaterialProperties.byLatitude.txt')
#####################
### breakage data ###
#####################
br <- read.csv('data/breakage.csv')
br$site <- tolower(substr(br$sample_id,1,3))
br$Continent <- meta$Continent[match(br$site,meta$field_site_code_2015)]
br$Continent <- factor(br$Continent)
br$Continent <- factor(br$Continent,levels=levels(br$Continent)[c(2,4,3,1)])
code <- data.frame(long=levels(br$Continent),short=c("Japan","wNA","eNA","Europe"))
br$Continent.short <- code$short[match(br$Continent,code$long)]
br$Continent.short <- factor(br$Continent.short,levels=c("Japan","wNA","eNA","Europe"))
br$lat <- meta$lat[match(br$site,meta$field_site_code_2015)]
## Log transform ###
### latitude - ANOVAs ###
m <- lmer(log(peak_force)~Continent.short+lat+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(log(maxstress)~Continent.short+lat+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(log(maxstrain)~Continent.short+lat+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(log(slope_Mpa)~Continent.short+lat+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(log(auc_modulus)~Continent.short+lat+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))

### latitude - ANOVAs ###
br2 <- br[br$Life_History2=="tetrasporophyte",]
print("tetrasporophytes only")
m <- lmer(log(peak_force)~Continent.short+lat+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(maxstress~Continent.short+lat+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(log(maxstrain)~Continent.short+lat+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(log(slope_Mpa)~Continent.short+lat+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(log(auc_modulus)~Continent.short+lat+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))


sink()

### plot 3 - no wNA

# peak force
tmp <- melt(br[!br$Continent=="NorthAmericaWest",c("site","peak_force")])
tmp2 <- cast(tmp,site~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site~variable,sd,na.rm=T)$peak_force
tmp2$n <- table(tmp$site)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)
tmp2$lat <- meta$lat[match(tmp2$site,meta$field_site_code_2015)]
tmp2$Continent <- br$Continent.short[match(tmp2$site,br$site)]

f1 <-  ggplot(data=tmp2, aes(x=lat,y=peak_force,ymin=peak_force-se,ymax=peak_force+se)) +
    geom_pointrange(size=0.5, aes(shape=Continent)) +
  scale_fill_manual(values=c("black","red","darkred")) +
  scale_shape_manual(values=c(21,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent,fill=Continent),size=.5,color="black") +
  theme_classic() +
    ylab("Breakage Force (N)") +
    xlab("Latitude") +
  theme(
    legend.position = c(.2, .95),
    legend.justification = c("right", "top"),
    legend.title=element_blank()
  )

# maxstrain
tmp <- melt(br[!br$Continent=="NorthAmericaWest",c("site","maxstrain")])
tmp2 <- cast(tmp,site~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site~variable,sd,na.rm=T)$maxstrain
tmp2$n <- table(tmp$site)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)
tmp2$lat <- meta$lat[match(tmp2$site,meta$field_site_code_2015)]
tmp2$Continent <- br$Continent.short[match(tmp2$site,br$site)]

f2 <-  ggplot(data=tmp2, aes(x=lat,y=maxstrain,ymin=maxstrain-se,ymax=maxstrain+se)) +
  geom_pointrange(size=0.5, aes(shape=Continent)) +
  scale_fill_manual(values=c("black","red","darkred")) +
  scale_shape_manual(values=c(21,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent,fill=Continent),size=.5,color="black") +
  theme_classic() +
  ylab("Max Strain") +
  xlab("Latitude") +
  theme(legend.position = "none") 

# slope
tmp <- melt(br[!br$Continent=="NorthAmericaWest",c("site","slope_Mpa")])
tmp2 <- cast(tmp,site~variable,mean,na.rm=T)
tmp2$sd <- cast(tmp,site~variable,sd,na.rm=T)$slope_Mpa
tmp2$n <- table(tmp$site)
tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)
tmp2$lat <- meta$lat[match(tmp2$site,meta$field_site_code_2015)]
tmp2$Continent <- br$Continent.short[match(tmp2$site,br$site)]

f3 <-  ggplot(data=tmp2, aes(x=lat,y=slope_Mpa,ymin=slope_Mpa-se,ymax=slope_Mpa+se)) +
  geom_pointrange(size=0.5, aes(shape=Continent)) +
  scale_fill_manual(values=c("black","red","darkred")) +
  scale_shape_manual(values=c(21,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent,fill=Continent),size=.5,color="black") +
  theme_classic() +
  ylab("Slope") +
  xlab("Latitude") +
  theme(legend.position = "none") 

png('output/MaterialProperties-byLatitude.png',width=5,height=9,units="in",res=700)
grid.arrange(f1,f2,f3,nrow=3,ncol=1)
dev.off()

### ### plot 5 - with wNA
datatypes <- c("peak_force","slope_Mpa","maxstress","maxstrain","auc_modulus")
y_labels <- c("Breaking Force (N)","Modulus (MPa)","Peak Stress","Peak Strain","Modulus (Mpa)")

dat.out <- list()
for (i in 1:5)
{
  tmp <- melt(br[,c(colnames(br)%in%c("site",datatypes[i]))])
  tmp2 <- cast(tmp,site~variable,mean,na.rm=T)
  tmp2$sd <- cast(tmp,site~variable,sd,na.rm=T)[,2]
  tmp2$n <- table(tmp$site)
  tmp2$se <- (tmp2$sd)/sqrt(tmp2$n)
  tmp2$lat <- meta$lat[match(tmp2$site,meta$field_site_code_2015)]
  tmp2$Continent <- br$Continent.short[match(tmp2$site,br$site)]

dat.out[[i]] <- tmp2
}
  
f1 <- ggplot(data=dat.out[[1]],aes(x=dat.out[[1]]$lat,y=dat.out[[1]][,2],ymin=dat.out[[1]][,2]-dat.out[[1]]$se,ymax=dat.out[[1]][,2]+dat.out[[1]]$se)) +
  geom_pointrange(size=0.5, aes(shape=Continent)) +
#  scale_shape_manual(values=c(21,19,17,15)) +
#  geom_smooth(method=lm,aes(linetype=Continent),size=.5,color="black") +
  scale_fill_manual(values=c("black","blue","red","darkred")) +
  scale_shape_manual(values=c(21,15,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent,fill=Continent),size=.5,color="black") +
  theme_classic() +
  ylab(y_labels[1]) +
  xlab("Latitude") +
  theme(
    legend.position = c(.2, .95),
    legend.justification = c("right", "top"),
    legend.title=element_blank()
  )


f2 <- ggplot(data=dat.out[[2]],aes(x=dat.out[[2]]$lat,y=dat.out[[2]][,2],ymin=dat.out[[2]][,2]-dat.out[[2]]$se,ymax=dat.out[[2]][,2]+dat.out[[2]]$se)) +
  geom_pointrange(size=0.5, aes(shape=Continent)) +
  scale_fill_manual(values=c("black","blue","red","darkred")) +
  scale_shape_manual(values=c(21,15,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent,fill=Continent),size=.5,color="black") +
  theme_classic() +
  ylab(y_labels[2]) +
  xlab("Latitude")+
  theme(legend.position = "none") 

f3 <- ggplot(data=dat.out[[3]],aes(x=dat.out[[3]]$lat,y=dat.out[[3]][,2],ymin=dat.out[[3]][,2]-dat.out[[3]]$se,ymax=dat.out[[3]][,2]+dat.out[[3]]$se)) +
  geom_pointrange(size=0.5, aes(shape=Continent)) +
  scale_fill_manual(values=c("black","blue","red","darkred")) +
  scale_shape_manual(values=c(21,15,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent,fill=Continent),size=.5,color="black") +
  theme_classic() +
  ylab(y_labels[3]) +
  xlab("Latitude") +
  theme(legend.position = "none") 

f4 <- ggplot(data=dat.out[[4]],aes(x=dat.out[[4]]$lat,y=dat.out[[4]][,2],ymin=dat.out[[4]][,2]-dat.out[[4]]$se,ymax=dat.out[[4]][,2]+dat.out[[4]]$se)) +
  geom_pointrange(size=0.5, aes(shape=Continent)) +
  scale_fill_manual(values=c("black","blue","red","darkred")) +
  scale_shape_manual(values=c(21,15,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent,fill=Continent),size=.5,color="black") +
  theme_classic() +
  ylab(y_labels[4]) +
  xlab("Latitude") +
  theme(legend.position = "none") 

f5 <- ggplot(data=dat.out[[5]],aes(x=dat.out[[5]]$lat,y=dat.out[[5]][,2],ymin=dat.out[[5]][,2]-dat.out[[5]]$se,ymax=dat.out[[5]][,2]+dat.out[[5]]$se)) +
  geom_pointrange(size=0.5, aes(shape=Continent)) +
  scale_fill_manual(values=c("black","blue","red","darkred")) +
  scale_shape_manual(values=c(21,15,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent,fill=Continent),size=.5,color="black") +
  theme_classic() +
  ylab(y_labels[5]) +
  xlab("Latitude") +
  theme(legend.position = "none") 

png('output/MaterialProperties-byLatitude-withWesternNA.png',width=12,height=12,units="in",res=700)
grid.arrange(f1,f2,f3,f4,f5,nrow=3,ncol=2)
#grid.arrange(f1[[1]],f1[[2]],f1[[3]],f1[[4]],f1[[5]],nrow=3,ncol=2)
dev.off()

