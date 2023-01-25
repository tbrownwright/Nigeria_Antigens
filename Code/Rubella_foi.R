setwd("~/Documents/Current Projects/CDC measles/Nigeria Serology/Data/cases_by_age")
library(tidyverse)
library(ggplot2)
#dd <- read_csv(list.files()[3])
dd <- read_csv("Data/CDC_Data/total_Rub_csv.csv")
dd %>% 
  mutate(ppn_pos = Positive/Tested) %>%
  ggplot(aes(x=Age, y=ppn_pos, size = Tested, col=State)) +
  geom_point() +
  facet_wrap(~State)


STATE <- unique(dd$State) # get the list of states 
p_age <- function(theta,age){1-exp(-theta*(age+1))} # function to calculate the probablility of infection by age a (+1 to be the right side of each interval)
LL_binom <- function(theta,x,n,age){-sum(dbinom(x,n,p_age(theta,age),log=T))} # the binomial negative log likelihood of all the postives in each age class, given probability above
theta_save <- numeric(length(STATE)) # storage for values 
ct <- 0 # counter
for(jj in STATE){ # loop over the states
  ct <- ct+1      # advance the counter
xx <- dd %>%      # subset to state
  mutate(ppn_pos = Positive/Tested) %>%
  filter(State == jj)

  nLL <- optimise(LL_binom,c(0,1),x=xx$Positive,n=xx$Tested,age=xx$Age) #run optimise to find maximum likelihood (minimium negativel log likelihood)
  theta_save[ct] <- nLL$minimum
  # THIS SECTION DOES THIS OLD-SCHOOL BY SEARCHING OVER SPECIFIC VALUES AND FINDING THE BEST
  # ADDED TO PROVE THAT BRUTE FORCE GETS SIMILAR ANSWER AS A CHECK THAT THE OPTIMISE CODE IS WORKING
  # nll <- numeric(1000)
  # th_rg <- seq(0,1,length=1000)
  # for(hh in 1:1000){nll[hh] <- LL_binom(th_rg[hh],x=xx$Positive,n=xx$Tested,age=xx$Age)}
  # theta_save[ct] <- th_rg[which.min(nll)]
 
  # UNCOMMENT THIS SECTION
  plot(xx$Age,xx$ppn_pos,cex=xx$Tested/10,main=jj,ylim=c(0,1))
  lines(xx$Age,p_age(nLL$minimum,xx$Age))  # GOES WITH TEH OPTIMISE VERSION
  #lines(xx$Age,p_age(th_rg[which.min(nll)],xx$Age)) # GOES WITH TEH LOOP VERSION
  #browser()  # UNCOMMENT THIS AND THE LOOP WILL PAUSE AFTER EACH STATE AND PLOT THE DATA AND FITTED CURVE
}

##############################################
# calcluate the probabilty of being infected over a 0.25 year period (~3 months )
# e.g. ~3% chance of infection during a 3 month window in Abia, 5% in Borno
output_rub <- tibble(state = STATE,f_rubella = theta_save, p_rubella =1 - exp(-theta_save * .25) )

# can take this at the state level and multiply thorugh with the # of at risk births to get 
# an approximation of the number of CRS cases


