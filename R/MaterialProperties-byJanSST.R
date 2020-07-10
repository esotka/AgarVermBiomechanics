### summary material properties
rm(list=ls())
library(ggplot2)
library(lmerTest)
library(reshape)
### site Metadata
meta <- read.csv('data/siteMeta.csv')
sink('output/MaterialProperties.byJanSST.txt')
#####################
### breakage data ###
#####################
br <- read.csv('data/breakage.csv')
br$site <- tolower(substr(br$sample_id,1,3))
br$Continent <- meta$Continent[match(br$site,meta$field_site_code_2015)]
br$Continent <- factor(br$Continent,levels=levels(br$Continent)[c(2,4,3,1)])
code <- data.frame(long=levels(br$Continent),short=c("Japan","wNA","eNA","Europe"))
br$Continent.short <- code$short[match(br$Continent,code$long)]
br$Continent.short <- factor(br$Continent.short,levels=c("Japan","wNA","eNA","Europe"))
br$JanSST <- meta$JanSST[match(br$site,meta$field_site_code_2015)]
### latitude - ANOVAs ###
m <- lmer(peak_force~Continent.short*JanSST+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(maxstress~Continent.short*JanSST+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(maxstrain~Continent.short*JanSST+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(slope_Mpa~Continent.short*JanSST+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(auc_modulus~Continent.short*JanSST+ (1|site), data=br[!br$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))

### only tetrasporophytes ###
print("only tetrasporophytes")
br2 <- br[br$Life_History2=="tetrasporophyte",]
m <- lmer(peak_force~Continent.short*JanSST+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(maxstress~Continent.short*JanSST+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(maxstrain~Continent.short*JanSST+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(slope_Mpa~Continent.short*JanSST+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))
m <- lmer(auc_modulus~Continent.short*JanSST+ (1|site), data=br2[!br2$Continent=="NorthAmericaWest",])
print(summary(m)$call)
print(summary(m)$coefficients);print(anova(m))




sink()

### plot 3 - no wNA

# peak force
tmp <- melt(br[!br$Continent=="NorthAmericaWest",c("site","peak_force")])
tmp2 <- cast(tmp,site~variable,mean,na.rm=T)
tmp2$JanSST <- meta$JanSST[match(tmp2$site,meta$field_site_code_2015)]
tmp2$Continent <- br$Continent.short[match(tmp2$site,br$site)]

f1 <-  ggplot(data=tmp2, aes(x=JanSST,y=peak_force)) +
  geom_point(size=2.0, aes(shape=Continent)) +
  scale_shape_manual(values=c(21,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent),size=.5,color="black") +
  theme_classic() +
  ylab("peak breakage force N (mean)") +
  xlab("January SST") +
  theme(legend.position = "none") +
  annotate(geom = "text", x = 14.5, y = c(3,2.7,2.4), label = c("Japan","Eastern NA","Europe"),hjust = 0) +
  annotate(geom="pointrange",x=13.5,y=c(3,2.7,2.4),ymin=0,ymax=0,pch=c(21,19,17)) +
  annotate(geom="segment",x=13,xend=14,y=c(3,2.7,2.4),yend=c(3,2.7,2.4),linetype=c("solid","dotted","dashed"))

# maxstrain
tmp <- melt(br[!br$Continent=="NorthAmericaWest",c("site","maxstrain")])
tmp2 <- cast(tmp,site~variable,mean,na.rm=T)
tmp2$JanSST <- meta$JanSST[match(tmp2$site,meta$field_site_code_2015)]
tmp2$Continent <- br$Continent.short[match(tmp2$site,br$site)]

f2 <-  ggplot(data=tmp2, aes(x=JanSST,y=maxstrain)) +
  geom_point(size=2.0, aes(shape=Continent)) +
  scale_shape_manual(values=c(21,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent),size=.5,color="black") +
  theme_classic() +
  ylab("Max Strain") +
  xlab("January SST") +
  theme(legend.position = "none") 

# slope
tmp <- melt(br[!br$Continent=="NorthAmericaWest",c("site","slope_Mpa")])
tmp2 <- cast(tmp,site~variable,mean,na.rm=T)
tmp2$JanSST <- meta$JanSST[match(tmp2$site,meta$field_site_code_2015)]
tmp2$Continent <- br$Continent.short[match(tmp2$site,br$site)]

f3 <-  ggplot(data=tmp2, aes(x=JanSST,y=slope_Mpa)) +
  geom_point(size=2.0, aes(shape=Continent)) +
  scale_shape_manual(values=c(21,19,17)) +
  geom_smooth(method=lm,aes(linetype=Continent),size=.5,color="black") +
  theme_classic() +
  ylab("Slope") +
  xlab("January SST") +
  theme(legend.position = "none") 

png('output/MaterialProperties-byJanSST.png',width=5,height=9,units="in",res=700)
grid.arrange(f1,f2,f3,nrow=3,ncol=1)
dev.off()


