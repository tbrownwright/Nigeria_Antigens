setwd("~/Documents/Current Projects/CDC measles/Nigeria Serology/Data/cases_by_age")
library(tidyverse)
library(ggplot2)
#dd <- read_csv(list.files()[3])
dd <- read_csv("Data/CDC_Data/Diph_FOI_Base.csv")
# dd %>% 
#   mutate(ppn_pos = Positive/Tested) %>%
#   ggplot(aes(x=Age, y=ppn_pos, size = Tested, col=State)) +
#   geom_point() +
#   facet_wrap(~State)

DTP_coverage <- read_csv("Data/CDC_Data/Diphtheria Tetanus Toxoid and Pertussis (DTP) vaccination coverage.csv")
cvr <- DTP_coverage %>%
  filter(COVERAGE_CATEGORY == "WUENIC") %>%
  select("COVERAGE")
cvr <- c((cvr$COVERAGE/100)[1:38],rep(.2,10))  # extend length 44 to match age classes; just extend past at 1980 levels

gamma_cvr <- -log(1-cvr)[-1] # transform coverage to exponential scale used in the catalytic model


STATE <- unique(dd$State) # get the list of states 
p_age <- function(gamma,theta,delta,age){1-exp(-gamma*gamma_cvr[age+1] * (delta^age) -theta*(age+1))} # function to calculate the probablility of infection by age a (+1 to be the right side of each interval)
# gamma is scaling on historical national covereage to be fit for each state; assumes all states have same trajectory
# delta is the waning of vaccine-based immunity over time; below we will asssum this is constant for all states

LL_binom <- function(par,delta,x,n,age){-sum(dbinom(x,n,p_age(par[1],par[2],delta,age),log=T))} # the binomial negative log likelihood of all the postives in each age class, given probability above
#gamma_save <- numeric() # storage for values 
#theta_save <- numeric() # storage for values 

nll_tot <- numeric() 
decay <- seq(.8,.995,length=100)  # a list of potential delta values
for(yy in 1:100){                 # loop over all values of delta
  nll_save <- numeric()
  ct <- 0 # counter
  for(jj in STATE){ # loop over the states
    ct <- ct+1      # advance the counter
    xx <- dd %>%      # subset to state
      #mutate(ppn_pos = Positive/Tested) %>%
      filter(State == jj) # 
    #fit theta and gamma for a given value of delta
    nLL <- optim(c(.5,.01),LL_binom,x=xx$Positive,n=xx$Tested,age=xx$Age,delta=decay[yy]) #run optimise to find maximum likelihood (minimium negativel log likelihood)
    #gamma_save[ct] <- nLL$par[1]
    #theta_save[ct] <- nLL$par[2]
    nll_save[ct] <-   nLL$value
  }
  nll_tot[yy] <- sum(nll_save)
}

gamma_save <- numeric() # storage for values 
theta_save <- numeric() # storage for values 

decay_fit <- decay[which.min(nll_tot)] # select the level of delta that minimizes likelihood 
# rerun fit for the best fit value of delta
  ct <- 0 # counter
  for(jj in STATE){ # loop over the states
    ct <- ct+1      # advance the counter
    xx <- dd %>%      # subset to state
      mutate(ppn_pos = Positive/Tested) %>%
      filter(State == jj)
    
    nLL <- optim(c(.5,.01),LL_binom,x=xx$Positive,n=xx$Tested,age=xx$Age,delta=decay_fit) #run optimise to find maximum likelihood (minimium negativel log likelihood)
    gamma_save[ct] <- nLL$par[1]
    theta_save[ct] <- nLL$par[2]
    
    # plot each state 
    #plot(xx$Age,xx$ppn_pos,cex=xx$Tested/10,main=jj,ylim=c(0,1),pch=19,col=rgb(1,0,0,.2))
    #lines(xx$Age,p_age(nLL$par[1],nLL$par[2],decay_fit,xx$Age))  # GOES WITH TEH OPTIMISE VERSION

  }
# store values
output_dip <- tibble(state = STATE,f_diphtheria =theta_save,g_diphtheria = gamma_save, d_diphtheria = decay_fit, d_cvr = p_age(gamma_save,theta_save,decay_fit,1))
  
###################################################################################################################
###################################################################################################################
#RUN RUBELLA FOI CODE
source("Rubella_foi.R")
###################################################################################################################
###################################################################################################################
vv <- read_csv("Data/CDC_Data/summary_antigens.csv")
vv <- vv %>% rename(state=NAME_1)
vv <- left_join(vv,output_dip) # combine with diphteria foi output
vv <- left_join(vv,output_rub) # combine with rubella foi output -- need to run Rubella_foi.R first

##################################################################################################################
# plot things
plot(vv$fit_dip_1,1-exp(-vv$g_diphtheria)) # DHS coverage v. fitted starting seroprevalence
abline(0,1)
# they are roughly consistent

# plot(vv$fit_dip_1,vv$f_diphtheria) # DHS coverage v. fitted starting seroprevalence
# plot(1-exp(-vv$g_diphtheria),vv$f_diphtheria,pch=NA) # DHS coverage v. fitted starting seroprevalence
# text(1-exp(-vv$g_diphtheria),vv$f_diphtheria,labels = vv$state)
# plot(vv$g_diphtheria,vv$f_diphtheria) # DHS coverage v. fitted starting seroprevalence
# 
# vv <-vv %>% mutate(p_dip2 = 1-exp(-g_diphtheria))
# 
# summary(lm(f_diphtheria ~ p_dip2,data=vv))
# plot(residuals(lm(f_diphtheria ~ p_dip2,data=vv)),vv$f_rubella)

save.image(file = "Intermediate/foi_data.RData")
