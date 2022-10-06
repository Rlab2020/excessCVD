
packages=c('dplyr','stats','splines','MASS','HH','FSA')
lapply(packages, require, character.only=T)


load("data0.rda")




#######################################################################
###--------------------------------------------------------------------

### MAIN ###

sel.data = subset(data1522_cvd, subset = (state0 == "US" & strain == "history"))
baseline.data = subset(data1522_cvd, subset = (state0 == "US" & strain != "history"), select = c(year, week, pop, death, strain, strain0, strain1, state, state0))

gl.mod <- glm.nb(death ~ 1 + pspline(week,df=7) + c(year-2015) + offset(log(pop)), link = log, data=sel.data)

predict.gl.mod = predict.glm(gl.mod, newdata = baseline.data, type = "link", se.fit = T)
comb.data = data.frame(baseline.data,
                       base.log.est = predict.gl.mod$fit,
                       base.log.se = predict.gl.mod$se.fit,
                       base.est = exp(predict.gl.mod$fit),
                       base.lwr = exp(predict.gl.mod$fit - 1.96*predict.gl.mod$se.fit),
                       base.upr = exp(predict.gl.mod$fit + 1.96*predict.gl.mod$se.fit))
comb.data$base.se = sqrt((exp(comb.data$base.log.se^2) -1) * exp(2*comb.data$base.log.est + comb.data$base.log.se^2))

during.epi.data <- comb.data %>% mutate(ed = death-base.est)
during.epi.data <- during.epi.data %>% mutate_if(is.numeric, round, 0)

observed <- sum(during.epi.data$death)
expected.est = sum(during.epi.data$base.est)
expected.se = sqrt(sum(during.epi.data$base.se ^2))
expected = round(expected.est + 1.96*c(0,-1,1)*expected.se, 0)
expected <- paste0(expected[1]," (",expected[2],", ",expected[3],")")


### EXCESS DEATH NUMBER ###
ed <- round(sum(during.epi.data[,16]),0)
ed <- round(ed + 1.96*c(0,-1,1)*expected.se, 0)
paste0(ed[1]," (",ed[2],", ",ed[3],")")


### EXCESS DEATH NUMBER PER WEEK ###
ed_mean <- ed[1]/nrow(during.epi.data)
ed_mean.se <- expected.se/nrow(during.epi.data)
ed_mean <- round(ed_mean + 1.96*c(0,-1,1)*ed_mean.se, 0)
paste0(ed_mean[1]," (",ed_mean[2],", ",ed_mean[3],")")


### EXCESS DEATH RATE ###
drate <- round(ed[1]/mean(during.epi.data$pop)*1000000,1)
drate.se <- round(expected.se/mean(during.epi.data$pop)*1000000,1)
drate <- round(drate + 1.96*c(0,-1,1)*drate.se, 1)
paste0(drate[1]," (",drate[2],", ",drate[3],")")


### EXCESS PERCENTAGE ###
ep <- round(ed[1]/expected.est*100,1)
ep.se <- sqrt(((ed[1])^2/(expected.est)^2)*((expected.se)^2/(ed[1])^2+(expected.se)^2/(expected.est)^2))*100
ep <- round(ep + 1.96*c(0,-1,1)*ep.se, 1)
paste0(ep[1]," (",ep[2],", ",ep[3],")")



