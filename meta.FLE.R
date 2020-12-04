
# FLE metanalysis

require('metafor')
citation('metafor')

par(mar=c(4,4,.5,2))

meta <- read.csv2("Google Drive/1_PhD/meta.FLE.csv",header=T,sep=";",dec=",",fill = T)

str(meta)

table(meta$Domain)
table(meta$NL)
table(meta$FL)
table(meta$group,meta$Domain)
table(meta$NL,meta$Domain)

##################################################################
# moral decision making domain
meta.m <- subset(meta, Domain == 'moral')

m.m <- rma(yi = ES, vi = var, method = 'REML', data = meta.m)

# cumulative effect size
summary(m.m)

round(m.m$b,3)
round((c(m.m$ci.lb,m.m$ci.ub)),3)

# Egger's test
regtest(m.m)

# plots
forest(m.m,slab=paste(meta.m$Authors),
       xlab="Hedges' g",mlab=" ",xlim=c(-1.5,2),alim=c(-1,1.5),at=c(-1,-.5,0,.5,1,1.5), cex = 0.8)

funnel(trimfill(m.m),xlab="Hedges' g",ylim=c(0,.6), steps = 4)

trimfill(m.m)

# leave-one.out
print(leave1out(m.m))
summary(leave1out(m.m)$estimate)
sd(leave1out(m.m)$estimate)

# meta-regression with proficiency as moderator
mr.m <- rma(yi = ES, vi = var, method = 'REML', mods = ~ proficiency, data = meta.m)
summary(mr.m)

# meta-regression with linguistic group as moderator
mr.m2 <- rma(yi = ES, vi = var, method = 'REML', mods = ~ group, data = meta.m)
summary(mr.m2)

mr.m2 <- rma(yi = ES, vi = var, method = 'REML', mods = ~ group -1, data = meta.m)
summary(mr.m2)


##################################################################
# risk aversion domain
meta.r <- subset(meta, Domain == 'risk')

m.r <- rma(yi = ES, vi = var, method = 'REML', data = meta.r)

# cumulative effect size
summary(m.r)

round(m.r$b,3)
round((c(m.r$ci.lb,m.r$ci.ub)),3)

# Egger's test
regtest(m.r)

# plots
forest(m.r,slab=paste(meta.r$Authors),
       xlab="Hedges' g",mlab=" ",xlim=c(-1.5,2),alim=c(-1,1.5),at=c(-1,-.5,0,.5,1,1.5))

funnel(trimfill(m.r),xlab="Hedges' g",ylim=c(0,.3),steps=4)

trimfill(m.r)

# leave-one-out
print(leave1out(m.r))
summary(leave1out(m.r)$estimate)
sd(leave1out(m.r)$estimate)

# meta-regression with proficiency as moderator
mr.r <- rma(yi = ES, vi = var, method = 'REML', mods = ~ proficiency, data = meta.r)
summary(mr.r)


