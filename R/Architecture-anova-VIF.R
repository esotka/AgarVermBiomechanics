### Variance inflation factors
rm(list=ls())
library(lmerTest)
meta <- read.csv('data/siteMeta.csv')
########### FUNCTION FOR VIF ############
#### FROM jon lefcheck: https://jonlefcheck.net/2012/12/28/dealing-with-multicollinearity-using-variance-inflation-factors/
#### and Florian Jaeger https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}


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
arch$natnon <- meta$NatNon[match(arch$site,meta$field_site_code_2015)]
arch$natnon <- factor(arch$natnon,levels=c("Japan","Introduced"))
#### DO ANOVAS #####
y <- c("sumlength","sumprojarea","sumavediam","sumrtvol")
y.n <- c("Length","Surfacearea","Diameter","Volume")
sink('output/Architecture.anova-mudflat.rocky-VIF.txt')
print("all thalli")
stats <- c()
for (i in 1:4)
{
  y.n.tmp <- arch[,y.n[i]]
  tmp <- arch[y.n.tmp=="Y",]
  m <- lmer(log(tmp[,y[i]])~natnon+MudRock+(1|site), data=tmp) 
  p = round(summary(m)$coefficients[,5],3)#anova(m)$'Pr(>F)'
  signif = ifelse(p<0.05,"*","")
  vif <- c(NA,vif.mer(m))
  stats <- rbind(stats,data.frame(data=y.n[i],p,signif,vif))
} 

print(stats)

print("tetrasporophytes only")  
stats <- c()
for (i in 1:4)
{
  y.n.tmp <- arch[,y.n[i]]
  tmp <- arch[y.n.tmp=="Y",]
  tmp <- tmp[tmp$StacyLifehistory=="Tetrasporophyte",]
  m <- lmer(log(tmp[,y[i]])~natnon+MudRock+(1|site), data=tmp) 
  p = round(summary(m)$coefficients[,5],3)#anova(m)$'Pr(>F)'
  signif = ifelse(p<0.05,"*","")
  vif <- c(NA,vif.mer(m))
  stats <- rbind(stats,data.frame(data=y.n[i],p,signif,vif))
} 

print(stats)

  
sink()
