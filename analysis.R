
packages=c('dplyr','stats','splines','MASS','HH','FSA')
lapply(packages, require, character.only=T)


load("data0.rda")




#######################################################################
###--------------------------------------------------------------------

### MAIN ###

sel.data = subset(data1522_cvd, subset = (state0 == "US" & strain == "history"))
baseline.data = subset(data1522_cvd, subset = (state0 == "US" & strain != "history"), select = c(year, week, pop, death, strain, strain0, strain1, state, state0))

gl.mod <- glm.nb(death ~ 1 + ns(week,df=4) + c(year-2015) + offset(log(pop)), link = log, data=sel.data)
predict.gl.mod = predict.glm(gl.mod, newdata = baseline.data, type = "link", se.fit = T)

comb.data <- baseline.data %>% mutate(base.est = exp(predict.gl.mod$fit), base.log.est = predict.gl.mod$fit, base.log.se = predict.gl.mod$se.fit)
during.epi.data <- comb.data

state <- unique(during.epi.data$state)
observed <- sum(during.epi.data$death)

expected.est <- round(sum(during.epi.data$base.est),0)
set.seed(1)
mc.sample.mat <- exp(rmvnorm(n = 1000, mean = c(comb.data$base.log.est), sigma = diag(c(comb.data$base.log.se^2))))
expected.ci <- round(quantile(x = rowSums(mc.sample.mat), probs = c(0.025, 0.975)),0)
expected.se <- (expected.ci[2] - expected.ci[1])/3.92
expected <- paste0(expected.est," (",expected.ci[1],", ",expected.ci[2],")")

### EXCESS DEATH NUMBER ###
ed.est <- observed - expected.est; ed.lwr <- observed - expected.ci[2]; ed.upr <- observed - expected.ci[1]
ed <- cbind(ed.est, ed.lwr, ed.upr)
ed_sum <- paste0(ed[1]," (",ed[2],", ",ed[3],")")

### EXCESS DEATH NUMBER PER WEEK ###
ed.mean <- round(ed/nrow(during.epi.data),0)
ed_mean <- paste0(ed.mean[1]," (",ed.mean[2],", ",ed.mean[3],")")

### EXCESS DEATH RATE ###
drate <- round(ed/mean(during.epi.data$pop)*1000000,1)
drate <- format(drate, nsmall=1)
drate <- paste0(drate[1]," (",drate[2],", ",drate[3],")")

### EXCESS PERCENTAGE ###
ep.est <- round(ed[1]/expected.est*100,1)
ep.se <- sqrt(((ed[1])^2/(expected.est)^2)*((expected.se)^2/(ed[1])^2+(expected.se)^2/(expected.est)^2))*100
ep <- round(ep.est + 1.96*c(0,-1,1)*ep.se, 1)
ep <- format(ep, nsmall=1)
ep <- paste0(ep[1]," (",ep[2],", ",ep[3],")")



