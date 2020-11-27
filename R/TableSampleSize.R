### meta table
rm(list=ls())
meta <- read.csv('data/siteMeta.csv')
br <- read.csv('data/breakage.csv')
arch <- read.csv('data/architecture.csv')

### Table for MS

out <- meta[,c("Continent","field_site_code_2015","Population","SiteA","lat","long")]
colnames(out) <- c("Continent","Population.code","Population","substrate","Latitude","Longitude")
out$Continent <- factor(out$Continent,levels=levels(out$Continent)[c(2,4,3,1)])
out <- out[order(out$Latitude),]
out <- out[order(out$Continent),]

## sample size breakage data
br$site <- tolower(substr(br$sample_id,1,3))
tmp <- data.frame(n=table(br$site))
out$n.Breakage <- tmp$n.Freq[match(out$Population.code,tmp$n.Var1)]

## sample size architecture data
arch$site <- tolower(substr(arch$Individual_ID,1,3))
tmp <- data.frame(n=table(arch$site,arch$"Length"))
tmp <- tmp[tmp$n.Var2=="Y",]
out$n.Architecture.Length <- tmp$n.Freq[match(out$Population.code,tmp$n.Var1)]
tmp <- data.frame(n=table(arch$site,arch$"Surfacearea"))
tmp <- tmp[tmp$n.Var2=="Y",]
out$n.Architecture.SurfaceArea <- tmp$n.Freq[match(out$Population.code,tmp$n.Var1)]
tmp <- data.frame(n=table(arch$site,arch$"Diameter"))
tmp <- tmp[tmp$n.Var2=="Y",]
out$n.Architecture.Diameter <- tmp$n.Freq[match(out$Population.code,tmp$n.Var1)]
tmp <- data.frame(n=table(arch$site,arch$"Volume"))
tmp <- tmp[tmp$n.Var2=="Y",]
out$n.Architecture.Volume <- tmp$n.Freq[match(out$Population.code,tmp$n.Var1)]
out[out==0] <- NA

#c("Length","Surfacearea","Surfacearea","Diameter","Volume")
write.csv(out,"output/Table-SampleSize.csv",row.names = F)
