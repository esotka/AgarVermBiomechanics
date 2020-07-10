### analyses of drift vs attached thalli of architecture data.
rm(list=ls())
library(ggplot2)
library(reshape)
library(visreg)
library(lmerTest)

### site Metadata
meta <- read.csv('data/siteMeta.csv')
tmp <- data.frame(long=levels(meta$Continent),short=c("Japan","wNA","eNA","Europe"))

########################
### architecture data ###
########################
arch <- read.csv('data/architecture.csv')
arch$site <- tolower(substr(arch$Individual_ID,1,3))
arch$Continent <- meta$Continent[match(arch$site,meta$field_site_code_2015)]
arch$Continent <- factor(arch$Continent,levels=levels(arch$Continent)[c(2,4,3,1)])
print("architecture data")
#print(table(arch$Continent,arch$StacyLifehistory))
arch$MudRock <- meta$SiteA[match(arch$site,meta$field_site_code_2015)]
arch$MudRock <- factor(arch$MudRock,levels=c("rockyshore","mudflat"))
#print(table(arch$Continent,arch$MudRock))
arch$Continent.short <- tmp$short[match(arch$Continent,tmp$long)]
arch$Continent.short <- factor(arch$Continent.short,levels=c("Japan","wNA","eNA","Europe"))
arch$natnon <- meta$NatNon[match(arch$site,meta$field_site_code_2015)]
arch$natnon <- factor(arch$natnon,levels=c("Japan","Introduced"))
arch$latitude <- meta$lat[match(arch$site,meta$field_site_code_2015)]
arch$JanSST <- meta$JanSST[match(arch$site,meta$field_site_code_2015)]
#### DO ANOVAS #####
y <- c("sumlength","sumprojarea","sumavediam","sumrtvol")
y.n <- c("Length","Surfacearea","Diameter","Volume")
sink('output/Architecture.anova-mudflat.rocky.txt')
for (i in 1:4)
{
y.n.tmp <- arch[,y.n[i]]
tmp <- arch[y.n.tmp=="Y",]
  m <- lmer(log(tmp[,y[i]])~natnon*MudRock+(1|site), data=tmp) 
print(y[i])
print("all thalli")
print(table(tmp$natnon,tmp$MudRock))
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
print("reproductive tetrasporophytes only")
tmp <- arch[y.n.tmp=="Y" & arch$StacyLifehistory=="Tetrasporophyte",]
m <- lmer(log(tmp[,y[i]])~natnon*MudRock+(1|site), data=tmp) 
print(table(tmp$natnon,tmp$MudRock))
print(summary(m)$call)
print(summary(m)$coefficients); print(anova(m))
}

#### correlations and ANCOVA ####
TwoVarsYN <- combn(y.n,2)
TwoVars <- combn(y,2)
out <- c()
for (j in 1:dim(TwoVars)[2])
{
  a <- arch[,TwoVarsYN[1,j]]; b <- arch[,TwoVarsYN[2,j]]
  tmp <- arch[a=="Y" & b=="Y",]
  r <- cor.test(tmp[,TwoVars[1,j]],tmp[,TwoVars[2,j]])
  out <- rbind(out,data.frame(var1=TwoVars[1,j],var2=TwoVars[2,j],
                              n=dim(tmp)[1],
                              r=round(r$estimate,3),p=round(r$p.value,3)))
### ancova all data ###
  print(paste("ANCOVA-",TwoVars[1,j],TwoVars[2,j]))
  print("all data")
  m <- lmer(log(tmp[,TwoVars[1,j]])~log(tmp[,TwoVars[2,j]])*tmp$natnon+(1|tmp$site))
  print(anova(m))
  print("only reproductive tetrasporophytes")
  tmp2 <- tmp[tmp$StacyLifehistory=="Tetrasporophyte",]
  m <- lmer(log(tmp2[,TwoVars[1,j]])~log(tmp2[,TwoVars[2,j]])*tmp2$natnon+(1|tmp2$site))
  print(anova(m))
  
  print(paste("ANCOVA-",TwoVars[2,j],TwoVars[1,j]))
  print("all data")
  m <- lmer(log(tmp[,TwoVars[2,j]])~log(tmp[,TwoVars[1,j]])*tmp$natnon+(1|tmp$site))
  print(anova(m))
  print("only reproductive tetrasporophytes")
  m <- lmer(log(tmp2[,TwoVars[2,j]])~log(tmp2[,TwoVars[1,j]])*tmp2$natnon+(1|tmp2$site))
  print(anova(m))
}
print(out)
sink()

#dev.off()