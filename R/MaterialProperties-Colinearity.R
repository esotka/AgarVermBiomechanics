### co-linearity of Mechanical properties
rm(list=ls())
meta <- read.csv('data/siteMeta.csv')
br <- read.csv('data/breakage.csv')

library(car)
pdf('output/MaterialProperties-Colinearity.pdf')
scatterplotMatrix(~log(slope_Mpa) + log(auc_modulus) + log(maxstrain) + log(maxstress) + log(peak_force),data=br,spread=F)
dev.off()

library(psych)
sink('output/MaterialProperties-Colinearity.txt')
tmp <- log(br[,c("slope_Mpa","auc_modulus","maxstrain","maxstress","peak_force")])
print(corr.test(tmp))
sink()
