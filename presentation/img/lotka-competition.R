library(deSolve)

lv <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # r1 = growthrate of Sp. 1; r2 = growthrate of Sp. 2
    # N = population size of Sp. 1; P = Population Size Sp. 2
    # a = competitive impact of Sp. 2 on Sp. 1; b = competitive impact of Sp 1 on Sp 2
    # K1/K2 = carrying capacities
    dN1dt <- (r1*N1*(1-(N1+a*N2)/K1)) 
    dN2dt <- (r2*N2*(1-(N2+b*N1)/K2))
    return(list(c(dN1dt,dN2dt)))
  })
}

# Root functions define the stopping parameters - when this root is zero, the thing will stop.
rootfun <- function(Time, State, Pars) {
  dstate <- unlist(lv(Time, State, Pars))
  sum(abs(dstate)) - 1e-3
}

par(bg = NA, mar = c(0,0,0,0))

init = c(N1 = 10, N2 =10)
params <- c(r1 = .5, r2 = .3, a = .25, b = .2, K1 = 300, K2 = 300)
time <- seq(0, 100, by = .1)
output <- lsodar(func=lv,y=init,parms=params,times=time,rootfun=rootfun)
plot(output[,2],type = "l", ylim = c(0, max(output[,2:3])), lwd = 8, lty = c(4,2), xlab = "", ylab = "", yaxt = "n", xaxt = "n")
lines(output[,3], type = 'l', col = "#701f28", lwd = 8)


params <- c(r1 = .9, r2 = .3, a = 1.01, b = .8, K1 = 50, K2 = 60)
time <- seq(0, 100, by = .1)
output <- lsodar(func=lv,y=init,parms=params,times=time,rootfun=rootfun)
plot(output[,2],type = "l", ylim = c(0, max(output[,2:3])), lwd = 8, lty = c(4,2), xlab = "", ylab = "", yaxt = "n", xaxt = "n")
lines(output[,3], type = 'l', col = "#701f28", lwd = 8)
 
