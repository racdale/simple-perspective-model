#########################################################################
#
# coded by Rick Dale; any errors are attributable to him.
# Based on Duran and Dale (2014) and as part of Dale, Galati et al., Frontiers paper:
# Dale, R., Galati, A., Alviar, C., Kallens, P. A. C., 
#     Ramirez-Aristizabal, A., Tabatabaeian, M. & Vinson, D. W. (2018). Interacting timescales in 
#     perspective-taking. Frontiers in Psychology.
#
#########################################################################

library(ggplot2)
library(grid)
library(cowplot)

setwd('~/Dropbox/new.projects/galati.perspective.review/simulation/')
source('functions.R')

###
# Figure of 1 state variable potential wells
###
pdf(file='sim_figures/1_state_well.pdf',height=4.5,width=8)
par(mfrow=c(1,2))

x = seq(from=-2,to=2,by=.01)
V = (0*x - x^2/2 + x^4/4)
plot(x,V,xlab='x',type='l',lwd=2)
text(-1,.5,'ego')
text(1,.5,'other')

for (i in 1) {
  print(i)
  x_trial = c()
  x = 0
  k = 0
  x_sum = 0
  for (j in 1:1000) {
    #x_trial = rbind(x_trial,data.frame(t=j,x=x,x2=x^2,x3=x^3,k=k))
    x_trial = rbind(x_trial,data.frame(k=k,x=x,x_sum=x_sum))
    lx = x
    x = x + (-k + x - x^3) + rnorm(1)*.05
    x_sum = x_sum + x
    
    if (abs(x_sum)>30) { 
      #points()
      ys = (0*x_trial$x - x_trial$x^2/2 + x_trial$x^4/4)
      points(x_trial$x,ys+.1,col='red',type='o')
      plot(x_trial$x_sum,type='o',xlim=c(0,nrow(x_trial)),ylim=c(-30,30),xlab='Iteration (t)',ylab='Sum(x)',col='red')
      break;
    }
  }
}
dev.off()

###
# Figure of 2-state system; equibiased
###
u_y = .2 # speed at which they update
u_x = .2
k_x = 0 # tilt of systems
k_y = 0
sigm = .01 # noise impulse
alph = .01 # how much they influence each other, as in Eq. 3
bet = .01

pdf(file='sim_figures/2_state_well_equibiased.pdf',height=7,width=7)
run_and_plot(u_x=.2,
             u_y=.2,
             k_x=0,
             k_y=0,
             sigm=.01,
             alph=.0,
             bet=.0)
dev.off()

###
# Ego is really fast, slow has no time to accumulate and win 
###
pdf(file='sim_figures/3_state_well_egofast.pdf',height=7,width=7)
run_and_plot(u_x=.2,
             u_y=.1,
             k_x=.2,
             k_y=0,
             sigm=.01,
             alph=.0,
             bet=.0)
dev.off()

###
# Social can take over entirely if we assume top-down control
###

pdf(file='sim_figures/4_state_well_social_set.pdf',height=7,width=7)
run_and_plot(u_x=.2,
             u_y=.1,
             k_x=.2,
             k_y=-.2,
             sigm=.01,
             alph=.2,
             bet=.0)
dev.off()

###
# Mutually reinforcing responses
###
pdf(file='sim_figures/5_mutually_reinforcing.pdf',height=7,width=7)
run_and_plot(u_x=.2,
             u_y=.1,
             k_x=.2,
             k_y=-.025,
             sigm=.01,
             alph=0.2,
             bet=0.01)
dev.off()

