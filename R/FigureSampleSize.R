### Figure of sample sizes
rm(list=ls())
library(ggplot2)
### site Metadata
meta <- read.csv('data/siteMeta.csv')
tmp <- data.frame(long=c("Japan","NorthAmericaWest","NorthAmericaEast","Europe"),short=c("Japan","wNA","eNA","Europe"))

########################
### architecture data ###
### Surface area samples only ########
########################
arch <- read.csv('data/architecture.csv')
arch$site <- tolower(substr(arch$Individual_ID,1,3))
arch$Continent <- meta$Continent[match(arch$site,meta$field_site_code_2015)]
arch$Continent <- factor(arch$Continent)
arch$Continent <- factor(arch$Continent,levels=levels(arch$Continent)[c(2,4,3,1)])
arch <- arch[arch$Surfacearea=="Y",]
print("architecture data")
print(table(arch$Continent,arch$StacyLifehistory))
arch$MudRock <- meta$SiteA[match(arch$site,meta$field_site_code_2015)]
arch$MudRock <- factor(arch$MudRock,levels=c("rockyshore","mudflat"))
print(table(arch$Continent,arch$MudRock))
arch$Continent.short <- tmp$short[match(arch$Continent,tmp$long)]
arch$Continent.short <- factor(arch$Continent.short,levels=c("Japan","wNA","eNA","Europe"))

arch$attachment.status2 <- c()
arch$attachment.status2[as.character(arch$attachment.status)%in%c("Buried","Diopatra","Drift","Drift/Buried","Drift/partly Buried","Drifts")] <- "Drift"
arch$attachment.status2[as.character(arch$attachment.status)%in%c("Attached","bedrock","bedrock/TL","Bedrock/TL","Buried Attached","Buried rock","","gastropod","H with sand","hard rock","holdfast","large rock","large stones","pebble","pebbles","plastic","rock","snail")] <- "Fixed"
arch <- arch[!arch$attachment.status=="Unknown",]
arch <- arch[!is.na(arch$attachment.status2),]
arch$attachment.status2 <- factor(arch$attachment.status2)
arch$natnon <- meta$NatNon[match(arch$site,meta$field_site_code_2015)]
arch$natnon <- factor(arch$natnon,levels=c("Japan","Introduced"))



pdf('output/FigureSampleSize-drift.vs.attached-archData.pdf',width=4,height=3)
f <- ggplot(arch,aes(Continent.short,fill=attachment.status2)) +
  geom_bar(position="stack",colour="black") +
  scale_fill_manual(values=c("white","grey")) +
  theme(legend.title=element_blank()) +
  ylab("n") + xlab("") +
  theme_classic()
print(f)
dev.off()

#####################
### breakage data ###
#####################
br <- read.csv('data/breakage.csv')
br$site <- tolower(substr(br$sample_id,1,3))
br$Continent <- meta$Continent[match(br$site,meta$field_site_code_2015)]
br$Continent <- factor(br$Continent,levels=levels(br$Continent)[c(2,4,3,1)])
### criteria for drift vs attached
br <- br[!br$attach2=="",]
br$attach3 <- c()
br$attach3[as.character(br$attachment)%in%c("buried","diopatra","drift","drift_buried","drift_partly_buried")] <- "Drift"
br$attach3[as.character(br$attachment)%in%c("attached","bedrock","bedrock_Tide_pool","buried_attached","buried_rock","h_with_sand","hard_rock","large_pebbles","large_rock","large_stones","oyster","p_with_sand","pebble","pebbles","rock","rope","shell","stick","tiny_pebble","wood")] <- "Fixed"
br$attach3 <- factor(br$attach3)
print("breakage")
print(table(br$Continent,br$Life_History2))
print(table(br$Continent,br$attach3))

br$Continent.short <- tmp$short[match(br$Continent,tmp$long)]
br$Continent.short <- factor(br$Continent.short,levels=c("Japan","wNA","eNA","Europe"))

pdf('output/FigureSampleSize-drift.vs.attached-breakageData.pdf',width=5.75,height=3)
#f <- ggplot(br,aes(Continent.short,fill=attach3)) +
#  geom_bar(position="stack",colour="black") +
#  scale_fill_manual(values=c("white","grey")) +
#  theme(legend.title=element_blank()) +
#  ylab("n") + xlab("") +
#  theme_classic()
f <- ggplot(br,aes(Continent.short,fill=paste(attach3,Life_History2))) +
  geom_bar(position="stack",colour="black") +
  scale_fill_manual(values=gray(3:0/3)) +
  theme(legend.title=element_blank()) +
  ylab("n") + xlab("") +
  theme_classic()
print(f)
dev.off()

### mudflat vs rockyshore
meta$SiteA <- as.character(meta$SiteA)
meta$SiteA[meta$SiteA=="rockyshore"] <- "Rocky-shore"
meta$SiteA[meta$SiteA=="mudflat"] <- "Mudflat"
br$MudRock <- meta$SiteA[match(br$site,meta$field_site_code_2015)]
br$MudRock <- factor(br$MudRock,levels=c("Rocky-shore","Mudflat"))
print(table(br$Continent.short,br$MudRock))
pdf('output/FigrueSampleSize-mudflat.vs.hard-breakageData.pdf',width=6,height=3)
#f <- ggplot(br,aes(Continent.short,fill=MudRock)) +
#  geom_bar(position="stack",colour="black") +
#  scale_fill_manual(values=c("white","gray")) +
#  theme(legend.title=element_blank()) +
#  ylab("n") + xlab("") +
#  theme_classic()
f <- ggplot(br,aes(Continent.short,fill=paste(MudRock,Life_History2))) +
  geom_bar(position="stack",colour="black") +
  scale_fill_manual(values=gray(3:0/3)) +
  theme(legend.title=element_blank()) +
  ylab("n") + xlab("") +
  theme_classic()
print(f)
dev.off()
#guides(fill=FALSE) +
#theme(legend.position = "top") +
#scale_fill_discrete(name="",labels=c("attached","drift"))

