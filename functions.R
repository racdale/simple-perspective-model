### creates ggplot grid for main figures
run_and_plot = function(u_x,u_y,k_x,k_y,sigm,alph,bet) {
  
  x = seq(from=-1.5,to=1.5,by=.01) # potential well for each variable
  VP = u_x*(k_x*x - x^2/2 + x^4/4)
  VS = u_y*(k_y*x - x^2/2 + x^4/4)
  potentialXP = ggplot(data=data.frame(x,VP),aes(x=x,y=VP))+geom_line(colour='gray',size=2)+xlab(expression('x'[P]))+ylab(expression('V'[P]))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.text.y=element_blank())
  potentialXS = ggplot(data=data.frame(V=VS,x=x),aes(x=x,y=VS))+geom_line(colour='gray',size=2)+xlab(expression('x'[S]))+ylab(expression('V'[S]))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.text.x=element_blank())+coord_flip()
  
  x_seed = seq(from=-1.5,to=1.5,by=.1) # grid to show full vector field
  x = expand.grid(x_seed,x_seed)[,1]
  y = expand.grid(x_seed,x_seed)[,2]
  
  dx = u_x*(-k_x + x - x^3)+alph*(y-x) # how x/y are changing to determine arrow size
  dy = u_y*(-k_y + y - y^3)+bet*(x-y)
  df = data.frame(x,y,dx,dy)
  vectorField = ggplot(data=df, aes(x=x, y=y)) + xlab(expression('x'[P]))+ylab(expression('x'[S]))+
    geom_segment(aes(xend=x+dx, yend=y+dy), 
                 arrow = arrow(length = unit(0.1,"cm")),col='gray')+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  new_plots = run_sim(u_x,u_y,k_x,k_y,sigm,alph,bet,vectorField,potentialXP,potentialXS)
  
  #return(list(vectorField,potentialXP,potentialXS,histoResponse))
  plot_grid(new_plots[[3]],new_plots[[1]],new_plots[[4]],new_plots[[2]])
  
}

# run 50 simulated "decisions", then resae plots
run_sim = function(u_x,u_y,k_x,k_y,sigm,alph,bet,vectorField,potentialXP,potentialXS) {
  
  ln = 100 # how long until it defaults exit
  threshold = 30 # threshold in drift/diffusion
  clrs = c('red','blue')
  choices = c() # perspective choice for histogram
  for (j in 1:50) { # no. of trials
    xy = c()
    x=0;y=0
    sum_x = 0
    sum_y = 0
    ix = 0
    for (i in 1:ln) { # run for at most 100, but keep a running sum and... 
      d_x = alph*(y-x) 
      d_y = bet*(x-y)
      x = x + u_x*(-k_x + x - x^3)+d_x+sigm*(rnorm(1))
      y = y + u_y*(-k_y + y - y^3)+d_y+sigm*(rnorm(1))
      sum_x = sum_x + x
      sum_y = sum_y + y
      if (abs(sum_x)>threshold | abs(sum_y)>threshold) { # ... quit when we are at threshold
        ix_which = which.max(c(abs(sum_x),abs(sum_y)))
        ix = sign(c(x,y)[ix_which])
        xy = rbind(xy,data.frame(x,y,ix,sum_x,sum_y))
        break
      }
      xy = rbind(xy,data.frame(x,y,ix,sum_x,sum_y))
    }
    vectorField=vectorField+
      geom_line(data=xy,aes(x=x,y=y))
    #vectorField<<-vectorField+
    #geom_point(data=xy[1,],aes(x=x,y=y),shape=15,size=5)
    
    Vx = u_x*(k_x*xy$x - xy$x^2/2 + xy$x^4/4)+runif(1)*u_x/7
    potentialXP=potentialXP+geom_line(data=data.frame(y=Vx,x=xy$x),aes(x=x,y=y))
    
    Vy = u_y*(k_y*xy$y - xy$y^2/2 + xy$y^4/4)+runif(1)*u_y/7 
    potentialXS=potentialXS+geom_line(data=data.frame(y=Vy,x=xy$y),aes(x=x,y=y))    
    
    if (ix_which==1) {
      potentialXP=potentialXP+geom_point(data=data.frame(y=Vx,x=xy$x)[nrow(xy),],aes(x=x,y=y),colour='black',size=3,shape=15)
    } else {
      potentialXS=potentialXS+geom_point(data=data.frame(y=Vy,x=xy$y)[nrow(xy),],aes(x=x,y=y),colour='black',size=3,shape=15)
    }
    
    choices = c(choices,ix)
    #points(0,0,pch=15,cex=2,col='green')
    #points(xy[,1],xy[,2],type='l')
    #points(xy[nrow(xy),1],xy[nrow(xy),2],pch=ix+15,cex=2,col='red')
  }
  choice_txt = abs((choices-1)/2)+1 # flips to Ego = 2, Other = 1
  histoResponse = ggplot(data.frame(perspective=as.factor(c('Other','Ego')[choice_txt])), aes(perspective))+geom_bar()
  
  return(list(vectorField,potentialXP,potentialXS,histoResponse))
  
}






