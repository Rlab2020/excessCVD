
packages=c('dplyr','stats','splines','mvtnorm')
lapply(packages, require, character.only=T)

load("data0.rda")


sel.data = subset(data1522_cvd, subset = (state0 == "US" & strain == "history"))
baseline.data = subset(data1522_cvd, subset = (state0 == "US" & strain != "history"), select = c(year, week, pop, death, strain, strain0, strain1, state, state0))

gl.mod <- glm.nb(death ~ 1 + ns(week,df=4) + c(year-2015) + offset(log(pop)), link = log, data=sel.data)
predict.gl.mod = predict.glm(gl.mod, newdata = baseline.data, type = "link", se.fit = T)

comb.data <- baseline.data %>% mutate(base.est = exp(predict.gl.mod$fit), base.log.est = predict.gl.mod$fit, base.log.se = predict.gl.mod$se.fit)

strain.list <- unique(data1522_cvd$strain)
strain.list <- strain.list[order(strain.list)][strain.list!="history"]

data_wave = NULL
for (i in 1:length(strain.list)) {
  
  list = as.character(strain.list[i])
  
  during.epi.data <- comb.data %>% filter(strain %in% list)
  state <- unique(during.epi.data$state)
  observed <- sum(during.epi.data$death)
  
  expected.est <- round(sum(during.epi.data$base.est),0)
  set.seed(3)
  mc.sample.mat <- exp(rmvnorm(n = 1000, mean = c(during.epi.data$base.log.est), sigma = diag(c(during.epi.data$base.log.se^2))))
  expected.ci <- round(quantile(x = rowSums(mc.sample.mat), probs = c(0.025, 0.975)),0)
  expected.se <- (expected.ci[2] - expected.ci[1])/3.92
  expected <- paste0(expected.est," (",expected.ci[1],", ",expected.ci[2],")")
  
  ed.est <- observed - expected.est; ed.lwr <- observed - expected.ci[2]; ed.upr <- observed - expected.ci[1]
  ed <- cbind(ed.est, ed.lwr, ed.upr)
  ed_sum <- paste0(ed[1]," (",ed[2],", ",ed[3],")")
  
  data = cbind(list,observed,expected,ed_sum)
  
  data_wave = rbind(data_wave,data)}
data_wave
