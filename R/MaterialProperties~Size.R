### does population-level size correlate with peak breakage force?

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
archtmp2 <- cast(archtmp,site+natnon+Continent.short~variable,mean,na.rm=T)

brtmp <- tapply(br$peak_force,br$site,mean,na.rm=T)
all <- data.frame(archtmp2)
all$peak_force <- brtmp[match(all$site,names(brtmp))]
all <- all[complete.cases(all),]
library(lattice)
pdf('output/MaterialProperties~size.pdf')
print(xyplot(peak_force~sumprojarea | natnon,data=all,type=c("r","p")))
print(xyplot(peak_force~sumprojarea,data=all,type=c("r","p")))
dev.off()
print(cor.test(all$peak_force,all$sumprojarea))
