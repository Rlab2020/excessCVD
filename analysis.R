
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
    + pspline(x = week, df = 6) + (1+pspline(x = week, df = 6) || state)
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

sel.data = subset(data1522_cvd, subset = (state0 == "US" & strain == "history"))
baseline.data = subset(data1522_cvd, subset = (state0 == "US" & strain != "history"), select = c(year, week, pop, death, strain, strain0, strain1, state, state0))

gl.mod = glm(death ~ 1 + pspline(week,df=6) + c(year-2015) + strain0 + offset(log(pop)), family=quasipoisson(link="log"), data=sel.data)


predict.gl.mod = predict.glm(gl.mod, newdata = baseline.data, type = "link", se.fit = T)
comb.data = data.frame(baseline.data,
                       base.est = exp(predict.gl.mod$fit),
                       base.lwr = exp(predict.gl.mod$fit - 1.96*predict.gl.mod$se.fit),
                       base.upr = exp(predict.gl.mod$fit + 1.96*predict.gl.mod$se.fit))

during.epi.data <- comb.data %>% mutate(ed = death-base.est) %>% mutate(ed.ll = death-base.upr) %>% mutate(ed.ul = death-base.lwr)
during.epi.data <- during.epi.data %>% mutate_if(is.numeric, round, 0)

state <- unique(during.epi.data$state)
observed <- sum(during.epi.data$death)
expected <- round(colSums(during.epi.data[,c(10:12)]),0)
expected <- paste0(expected[1]," (",expected[2],", ",expected[3],")")


### EXCESS DEATH NUMBER ###
ed <- round(colSums(during.epi.data[,c(13:15)]),0)
ed_sum <- paste0(ed[1]," (",ed[2],", ",ed[3],")")


### EXCESS DEATH NUMBER PER WEEK ###
ed_mean <- round(colMeans(during.epi.data[,c(13:15)]),0)
ed_mean <- paste0(ed_mean[1]," (",ed_mean[2],", ",ed_mean[3],")")


### EXCESS DEATH RATE ###
drate <- round(colSums(during.epi.data[,c(13:15)])/mean(during.epi.data$pop)*1000000,1)
drate <- paste0(drate[1]," (",drate[2],", ",drate[3],")")


### EXCESS PERCENTAGE ###
ep = round(ed/sum(during.epi.data$base.est)*100,1)
ep <- paste0(ep[1]," (",ep[2],", ",ep[3],")")

