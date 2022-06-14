
packages=c('dplyr','stats','splines','lme4','mvtnorm')
lapply(packages, require, character.only=T)


load("data0.rda")



#######################################################################
###--------------------------------------------------------------------

### MODEL SELECTION ###

sel.data = subset(data1522_cvd, subset = state != 'United States')
table(sel.data$state)
#

lme.mod.full = lmer(
  formula = as.formula(
    log(death) ~ 1 
    + ns(x = week, df = 12) + (1+ns(x = week, df = 12) || state)
    + year + (1+year | state)
    + strain0 + (1+strain0 || state)
  ),
  REML = F, offset = log(pop), data = sel.data
)
#summary(lme.mod.full)
AIC(lme.mod.full)




#######################################################################
###--------------------------------------------------------------------

### MAIN RESULT ###

sel.data = subset(data1522_cvd, subset = state0=="US")
gl.mod = glm(death ~ 1+ns(week,df=12) + c(year-2015) + strain0 + offset(log(pop)), family=quasipoisson(link="log"), data=sel.data)


### EXCESS DEATH ###
baseline.data = subset(sel.data, select = c(year, week, pop))
baseline.data$strain0 = 0
predict.gl.mod = predict.glm(object = gl.mod, newdata = baseline.data, se.fit = T, dispersion = NULL)
baseline.data$log.base.est = predict.gl.mod$fit
baseline.data$log.base.se = predict.gl.mod$se.fit

baseline.data$base.est = exp(baseline.data$log.base.est + 0*1.96*baseline.data$log.base.se)
baseline.data$base.lwr = exp(baseline.data$log.base.est - 1*1.96*baseline.data$log.base.se)
baseline.data$base.upr = exp(baseline.data$log.base.est + 1*1.96*baseline.data$log.base.se)

comb.data = data.frame(
  subset(baseline.data, select = c(year, week, pop, base.est, base.lwr, base.upr)),
  subset(sel.data, select = c(death,strain,strain0,strain1,state,state0)))

during.epi.data <- subset(comb.data, subset = (strain0 == 1))
#during.epi.data <- subset(comb.data, subset = (strain1 == var)) FOR PANDEMIC YEAR
#during.epi.data <- subset(comb.data, subset = (strain == var)) FOR PANDEMIC WAVE
during.epi.data <- during.epi.data %>% mutate(ed = death-base.est) %>% mutate(ed.ll = death-base.upr) %>% mutate(ed.ul = death-base.lwr)
during.epi.data <- during.epi.data %>% mutate_if(is.numeric, round, 0)

names(during.epi.data)
observed <- sum(during.epi.data$death)
expected <- round(colSums(during.epi.data[,c(4:6)]),0)
expected <- paste0(expected[1]," (",expected[2],", ",expected[3],")")



### EXCESS DEATH NUMBER ###
ed <- round(colSums(during.epi.data[,c(13:15)]),0)
paste0(ed[1]," (",ed[2],", ",ed[3],")")



### EXCESS DEATH NUMBER PER WEEK ###
ed_mean <- round(colMeans(during.epi.data[,c(13:15)]),0)
paste0(ed_mean[1]," (",ed_mean[2],", ",ed_mean[3],")")



### EXCESS DEATH RATE ###
drate <- round(colSums(during.epi.data[,c(13:15)])/mean(during.epi.data$pop)*1000000,1)
paste0(drate[1]," (",drate[2],", ",drate[3],")")



### EXCESS PERCENTAGE ###
ep = round(ed/sum(during.epi.data$base.est)*100,1)
paste0(ep[1]," (",ep[2],", ",ep[3],")")

