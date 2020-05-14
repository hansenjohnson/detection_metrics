## functions ##

library(tidyverse)
library(pracma)

# functions
sprinkle = function(n = 1e4, max_radius = 40, make_plot = FALSE){
  # sprinkle uniform distribution of n points in a circle radius of max_radius
  
  # choose range
  r = max_radius*sqrt(runif(n = n, min = 0, max = 1))
  
  # choose angle
  a = 2*pi*runif(n = n, min = 0, max = 1)
  
  # convert to xy
  x=r*cos(a)
  y=r*sin(a)
  
  # combine data
  out = tibble(id = seq(from = 1, to = n, by = 1),x,y,r)
  
  # plot
  if(make_plot){
    plt = ggplot(out)+
      geom_point(aes(x=x,y=y), shape = 1)+
      coord_equal()+
      theme_bw()
    print(plt)
  }
  
  return(out)
}

logistic_df = function(x,L=1.045,x0=10,k=-0.3){
  # Construct a detection function using a logistic curve
  # L = maximum Y value
  # x0 = value at midpoint
  # k = logistic growth rate
  y = L/(1+exp(-1*k*(x-x0))) 
  return(y)
}

plot_df = function(max_radius=40,L=1.045,x0=10,k=-0.3,return_data=F){
  # plot detection function
  
  # apply detection function
  df = tibble(
    r = seq(from = 0, to = max_radius, by = 0.1),
    p = logistic_df(r,L=L,x0=x0,k=k)
  )
  
  if(return_data){
    return(df)
  }
  
  # plot detection function
  plt = ggplot(df)+
    geom_path(aes(x=r,y=p))+
    lims(y = c(0,1))+
    theme_bw()
  print(plt)
  
}

empirical_df = function(df){
  # compute logistic regression from empirical detection data
  # df must have columns r and detected
  
  # data for logistic regression
  df_lg = df %>%
    transmute(r=r,nscore=detected)
  df_lg = df_lg[complete.cases(df_lg),]
  
  # create binomial model
  lg = glm(nscore ~ r,family=binomial,data=df_lg)
  
  # create range vector
  rv = seq(from = round(min(df_lg$r,na.rm = T),1), to = round(max(df_lg$r,na.rm = T),1), by = 0.05)
  
  # predict probabilities along range vector using model
  mp = predict(lg, data.frame(r = rv), se.fit = T)
  mp_fit = exp(mp$fit)/(1+exp(mp$fit))
  
  # combine
  df_lg = tibble(
    p = mp_fit,
    r = rv,
    upper = exp(mp$fit+1.96*mp$se.fit)/(1+exp(mp$fit+1.96*mp$se.fit)),
    lower = exp(mp$fit-1.96*mp$se.fit)/(1+exp(mp$fit-1.96*mp$se.fit))
  )
  
  # list for output
  out = list(
    mod = lg,
    data = df_lg
  )
  
  return(out)
}

calc_edr = function(lg,w){
  # calculate the effective detection radius of a logistic function
  # lg is the logistic regression model
  # w is the truncation range (km)
  
  # predictive logistic regression function
  g = function(x,mod=lg){
    mp = predict(mod, data.frame(r = x), se.fit = T)
    return(as.numeric(exp(mp$fit)/(1+exp(mp$fit))))
  }
  
  # product of predictive logistic regression function
  gx = function(x){
    return(x*g(x))
  }
  
  # calculate effective detection area (km2)
  v = 2*pi*trapzfun(f = gx, a = 0, b = w, maxit = 10)$value
  
  # effective detection radius (km)
  rho = sqrt(v/pi)
  
  return(rho)
}
