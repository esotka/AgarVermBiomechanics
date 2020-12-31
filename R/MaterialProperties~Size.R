### does population-level size correlate with peak breakage force?
rm(list=ls())
library(reshape)
### site Metadata
meta <- read.csv('data/siteMeta.csv')
tmp <- data.frame(long=levels(meta$Continent)[c(2,4,3,1)],short=c("Japan","wNA","eNA","Europe"))


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

br$JanSST <- meta$JanSST[match(br$site,meta$field_site_code_2015)]

########################
### architecture data ###
########################
arch <- read.csv('data/architecture.csv')
arch$site <- tolower(substr(arch$Individual_ID,1,3))
arch$Continent <- meta$Continent[match(arch$site,meta$field_site_code_2015)]
arch$Continent <- factor(arch$Continent,levels=levels(arch$Continent)[c(2,4,3,1)])
arch$MudRock <- meta$SiteA[match(arch$site,meta$field_site_code_2015)]
arch$MudRock <- factor(arch$MudRock,levels=c("rockyshore","mudflat"))
arch$Continent.short <- tmp$short[match(arch$Continent,tmp$long)]
arch$Continent.short <- factor(arch$Continent.short,levels=c("Japan","wNA","eNA","Europe"))
arch$natnon <- meta$NatNon[match(arch$site,meta$field_site_code_2015)]
arch$natnon <- factor(arch$natnon,levels=c("Japan","Introduced"))
arch$latitude <- meta$lat[match(arch$site,meta$field_site_code_2015)]
arch$JanSST <- meta$JanSST[match(arch$site,meta$field_site_code_2015)]

### all breakage sites are in the architecture sites (41). 3 architecture sites are NOT in the breakage sites ("dkm" "ehs" "tom")
### all thalli
archtmp <- arch[arch$Surfacearea=="Y",]
archtmp <- melt(archtmp[,c("site","natnon","Continent.short","sumprojarea")])
archtmp$value <- log(archtmp$value)
archtmp2 <- cast(archtmp,site+natnon+Continent.short~variable,mean,na.rm=T)
all <- data.frame(archtmp2)

#### log of material properties -all data ###
y <- c("peak_force","maxstress","maxstrain","auc_modulus","slope_Mpa")
corr.out <- c()
for (i in 1:5)
{
  brtmp <- tapply(log(br[,colnames(br)==y[i]]),br$site,mean,na.rm=T)
  all[,ncol(all)+1] <- brtmp[match(all$site,names(brtmp))]
  ### corr tests
  tmp <- data.frame(all[,ncol(all)],all$sumprojarea,natnon=all$natnon)
  tmp <- tmp[complete.cases(tmp),]
  p = cor.test(tmp[,1],tmp[,2])$p.value
  cor.est = cor.test(tmp[,1],tmp[,2])$estimate
  ### corr tests native only
  p.nat = cor.test(tmp[tmp$natnon=="Japan",1],tmp[tmp$natnon=="Japan",2])$p.value
  cor.est.nat = cor.test(tmp[tmp$natnon=="Japan",1],tmp[tmp$natnon=="Japan",2])$estimate
  
  ### corr tests non-native only
  p.non = cor.test(tmp[tmp$natnon=="Introduced",1],tmp[tmp$natnon=="Introduced",2])$p.value
  cor.est.non = cor.test(tmp[tmp$natnon=="Introduced",1],tmp[tmp$natnon=="Introduced",2])$estimate
  
  corr.out <- rbind(corr.out,data.frame(x="sumprojarea",y=y[i],data="all",p,cor.est,p.nat,cor.est.nat,p.non,cor.est.non))
}


### corr tests of non-transformed data & spearman rank ##
y <- c("peak_force","maxstress","maxstrain","auc_modulus","slope_Mpa")
spear.corr.out <- c()
for (j in 1:5)
{
  brtmp <- tapply(br[,colnames(br)==y[j]],br$site,mean,na.rm=T)
  all[,ncol(all)+1] <- brtmp[match(all$site,names(brtmp))]
  ### corr tests
  tmp <- data.frame(all[,ncol(all)],all$sumprojarea,natnon=all$natnon)
  tmp <- tmp[complete.cases(tmp),]
  p = cor.test(tmp[,1],tmp[,2],method = "spearman")$p.value
  cor.est = cor.test(tmp[,1],tmp[,2],method = "spearman")$estimate
  ### corr tests native only
  p.nat = cor.test(tmp[tmp$natnon=="Japan",1],tmp[tmp$natnon=="Japan",2],method = "spearman")$p.value
  cor.est.nat = cor.test(tmp[tmp$natnon=="Japan",1],tmp[tmp$natnon=="Japan",2],method = "spearman")$estimate
  
  ### corr tests non-native only
  p.non = cor.test(tmp[tmp$natnon=="Introduced",1],tmp[tmp$natnon=="Introduced",2],method = "spearman")$p.value
  cor.est.non = cor.test(tmp[tmp$natnon=="Introduced",1],tmp[tmp$natnon=="Introduced",2],method = "spearman")$estimate
  
  spear.corr.out <- rbind(spear.corr.out,data.frame(x="sumprojarea",y=y[j],data="all",p,cor.est,p.nat,cor.est.nat,p.non,cor.est.non))
}



colnames(all)[5:9] <- y
all <- all[complete.cases(all),]
pdf('output/MaterialProperties~size.pdf')
library(car)
print(scatterplotMatrix(~sumprojarea + peak_force + maxstress + maxstrain + auc_modulus + slope_Mpa ,data=all,main="all thalli"))
print(scatterplotMatrix(~sumprojarea + peak_force + maxstress + maxstrain + auc_modulus + slope_Mpa | natnon,data=all,main="all thalli"))



### tetrasporophytes only

archtmp <- arch[arch$Surfacearea=="Y" & arch$StacyLifehistory=="Tetrasporophyte",]
archtmp <- melt(archtmp[,c("site","natnon","Continent.short","sumprojarea")])
archtmp$value <- log(archtmp$value)
archtmp2 <- cast(archtmp,site+natnon+Continent.short~variable,mean,na.rm=T)
all <- data.frame(archtmp2)

#### log of material properties ###
y <- c("peak_force","maxstress","maxstrain","auc_modulus","slope_Mpa")
for (i in 1:5)
{
  brtmp <- tapply(log(br[,colnames(br)==y[i]]),br$site,mean,na.rm=T)
  all[,ncol(all)+1] <- brtmp[match(all$site,names(brtmp))]
  ### corr tests
  tmp <- data.frame(all[,ncol(all)],all$sumprojarea,natnon=all$natnon)
  tmp <- tmp[complete.cases(tmp),]
  p = cor.test(tmp[,1],tmp[,2])$p.value
  cor.est = cor.test(tmp[,1],tmp[,2])$estimate
  ### corr tests native only
  p.nat = cor.test(tmp[tmp$natnon=="Japan",1],tmp[tmp$natnon=="Japan",2])$p.value
  cor.est.nat = cor.test(tmp[tmp$natnon=="Japan",1],tmp[tmp$natnon=="Japan",2])$estimate
  
  ### corr tests non-native only
  p.non = cor.test(tmp[tmp$natnon=="Introduced",1],tmp[tmp$natnon=="Introduced",2])$p.value
  cor.est.non = cor.test(tmp[tmp$natnon=="Introduced",1],tmp[tmp$natnon=="Introduced",2])$estimate
  
  corr.out <- rbind(corr.out,data.frame(x="sumprojarea",y=y[i],data="tetrasporophytes",p,cor.est,p.nat,cor.est.nat,p.non,cor.est.non))
}

### corr tests of non-transformed data & spearman rank ##
y <- c("peak_force","maxstress","maxstrain","auc_modulus","slope_Mpa")
for (j in 1:5)
{
  brtmp <- tapply(br[,colnames(br)==y[j]],br$site,mean,na.rm=T)
  all[,ncol(all)+1] <- brtmp[match(all$site,names(brtmp))]
  ### corr tests
  tmp <- data.frame(all[,ncol(all)],all$sumprojarea,natnon=all$natnon)
  tmp <- tmp[complete.cases(tmp),]
  p = cor.test(tmp[,1],tmp[,2],method = "spearman")$p.value
  cor.est = cor.test(tmp[,1],tmp[,2],method = "spearman")$estimate
  ### corr tests native only
  p.nat = cor.test(tmp[tmp$natnon=="Japan",1],tmp[tmp$natnon=="Japan",2],method = "spearman")$p.value
  cor.est.nat = cor.test(tmp[tmp$natnon=="Japan",1],tmp[tmp$natnon=="Japan",2],method = "spearman")$estimate
  
  ### corr tests non-native only
  p.non = cor.test(tmp[tmp$natnon=="Introduced",1],tmp[tmp$natnon=="Introduced",2],method = "spearman")$p.value
  cor.est.non = cor.test(tmp[tmp$natnon=="Introduced",1],tmp[tmp$natnon=="Introduced",2],method = "spearman")$estimate
  
  spear.corr.out <- rbind(spear.corr.out,data.frame(x="sumprojarea",y=y[j],data="tetrasporophytes",p,cor.est,p.nat,cor.est.nat,p.non,cor.est.non))
}
print(spear.corr.out)



colnames(all)[5:9] <- y
all <- all[complete.cases(all),]

print(scatterplotMatrix(~sumprojarea + peak_force + maxstress + maxstrain + auc_modulus + slope_Mpa ,data=all,main="tetrasporophyte thalli"))
print(scatterplotMatrix(~sumprojarea + peak_force + maxstress + maxstrain + auc_modulus + slope_Mpa | natnon,data=all,main="tetrasporophyte thalli"))

dev.off()

#write.csv(corr.out,'output/MaterialProperties~size.correlations.csv')
