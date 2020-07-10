### Variance inflation factors
library(lmerTest)
library(olsrr)
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

#######################

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
br$natnon <- factor(br$natnon,levels=c("Japan","Introduced"))

#####################
#### STATS #########
#####################

sink('output/MaterialProperties.anova-drift.vs.attached-VIF.txt')

print("######### all thalli #########")
stats <- c()
datatypes <- c("peak_force","slope_Mpa","maxstress","maxstrain","auc_modulus")
for (i in 1:length(datatypes))
{
#### natnon + attachment ###
m <- lmer(log(br[,colnames(br)==datatypes[i]])~natnon+attach3+(1|site), data=br) 
p = round(summary(m)$coefficients[,5],3)#anova(m)$'Pr(>F)'
signif = ifelse(p<0.05,"*","")
vif <- c(NA,vif.mer(m))
stats <- rbind(stats,data.frame(data=datatypes[i],p,signif,vif))
}

print(stats)

print("######### tetrasporophytes only #########")
stats <- c()
datatypes <- c("peak_force","slope_Mpa","maxstress","maxstrain","auc_modulus")
for (i in 1:length(datatypes))
{
  #### natnon + attachment ###
  tmp <- br[br$Life_History2=="tetrasporophyte",]
  m <- lmer(log(tmp[,colnames(tmp)==datatypes[i]])~natnon+attach3+(1|site), data=tmp) 
  p = round(summary(m)$coefficients[,5],3)#anova(m)$'Pr(>F)'
  signif = ifelse(p<0.05,"*","")
  vif <- c(NA,vif.mer(m))
  stats <- rbind(stats,data.frame(data=datatypes[i],p,signif,vif))
}

print(stats)



sink()
