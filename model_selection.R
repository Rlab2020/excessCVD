
packages=c('MASS','survival','splines')
lapply(packages, require, character.only=T)
load("data0.rda")

sel.data = subset(data1522_cvd, subset = (state0 == "US" & strain == "history"))

# for df selection using AIC #
state.array = unique(subset(data1522_cvd, subset = (state0 != "US"))$state0)
df.array = c(1:12)
#
aic.array = NULL
for(temp.df in df.array){#        temp.df = 2
  sel.aic.array = NULL
  for (state.j in 1:length(state.array)) {#         state.j = 1
    sel.state = state.array[state.j]
    sel.data = subset(data1522_cvd, subset = (state0 == sel.state & strain == "history"))
    #
    eval(parse(text=paste("gl.mod <- glm.nb(death ~ 1 + ns(week,df=",temp.df,") + c(year-2015) + offset(log(pop)), link = log, data=sel.data)",sep='')))
    sel.aic.array = c(sel.aic.array, gl.mod$aic)
  }
  aic.array = c(aic.array, sum(sel.aic.array))
}

plot(df.array, aic.array, type = 'l', ylim = min(aic.array)+c(0, 200))


