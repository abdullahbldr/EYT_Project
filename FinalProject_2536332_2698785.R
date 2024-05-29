################################################################################

# Abdullah Bıldır - 2536332

# Alpaslan Doğankollu - 2698785

################################################################################
 
install.packages("readxl")        # We installed the required package.
library("readxl")

################################################################################
# Parameters
################################################################################
  
#setwd()                           # Please change here.

set.seed(26)                      # We set seed to receive the same numbers on each time.

#Demographics
starty <- 1990                    # Start year of the model.
endy <- 2050                      # End year of the model.
years <- c(starty:endy)
youth <- 10                       # Definition of youth population (i.e. age 20-30)

n0 <- 100                         # Initial population for "Generation 1"
J <- 58                           # Life-span
JR_N <- 40                        # Age of retirement normal
JR_E0 <- 25                       # Age or retirement in case of retirement reform.

JR_E <- JR_E0
JR_C <- JR_E

p <- 0.0157                       # Population growth rate
tau <- 0.15                       # Tax rate
sgdp <- 0.075                     # Social security support premium

# Preferences
sigma <- 1.2                      # Coefficient of relative risk aversion
gamma <- 0.45                     # Weight on consumption

# Production
alpha <- 0.4766                     # Production elasticity of capital 
delta <- 0.0375                   # Rate of depreciation
 

################################################################################

# In this section, we integrated age-efficiency profiles into the model and calculated margin of error.

e <- read_excel("AgeEff.xlsx")

e <- e[,2]

e <- e$EffAve/mean(e$EffAve)

mean_e <- mean(e)

sample_e <- length(e)
sample_sd <- sd(e)
sample_se <- sample_sd/sqrt(sample_e)

error <- 0.05
df = sample_e - 1
t_score = qt(p=error/2, df=df,lower.tail=F)

margin_error <- t_score * sample_se


################################################################################

# In this section, we defined some empty matrices, data frames and lists. 

maxcap <- 1

totallaborsup <- data.frame(matrix(data = NA, nrow = (J-1), ncol = 8))

totallaborsup[,1] <- c(1:(J-1))

colnames(totallaborsup) <- c("Generations", "Total Labor Supply","Labor Force","Number of Employed","Number of Full Time Employed",
                             "LF Participation Rate", "Unemployment Rate", "Population Size")

yearsls <- list()

yearsun <- data.frame(matrix(ncol=11, nrow=0))

colnames(yearsun) <- c("Years", "Overall_Unemployment", "Youth_Unemployment",
                       "Overall_LFP", "Youth_LFP", "Productive_LFP", "Target_LFP", "Retired_LFP",
                       "Effective_Labor_Supply", "Labor_Demand", "Wage")

vlaborsupply <- matrix(NA,n0, J-1)

esmat <- matrix(NA,n0, J-1)

generations <- list()

n <- n0
nc <- n0


################################################################################

w0 <- 1                            # Initial wage
r0 <- 0.25                         # Initial interest rate

un0 <- 1                        # Artificial unemployment coefficient to provide frictional unemployment at the beginning (here in case it is 0)            

eflimwage0 <- 0.5*0.17             # Initial effective hours limit (estimated from minimum wage).

eflimwage <- eflimwage0

eflimw <- c(eflimwage)


################################################################################ 
# Working Class
################################################################################

# In this section, we set the initial state of working households for t = 0.

for(j in 1:(JR_N-1)){                                                       # Generations
  
  id <- data.frame(matrix(ncol=5, nrow=0))
  
  
  for(i in 1:n){                                                            # Individuals
  
   ccap_iw <- runif(1, min=0, max=maxcap)                                   # Uniformly distributed initial capital
   e_w <- runif(1, min=(e[j]-margin_error), max=(e[j]+margin_error))        # Uniformly distributed age efficiency given interval for agent i
 

   cons <- function(x){

    (1-tau)*w0*x*e_w + ccap_iw                                              # Budget constraint, b = 0

   }

   utility <- function(x){

    -((((cons(x)^gamma)*((1-x)^(1-gamma)))^(1-sigma))-1)/(1-sigma)          # Utility function

   }


   laborsup_i <- round(optimize(utility, c(-1,1), lower= 0, tol=0.00001)$minimum,digits=4)        # Optimization of the utility function
  
  
   if(((e_w*w0*laborsup_i) <= eflimwage) & (laborsup_i > 0)) {              # We determined if the agent is employed or unemployed given it is in the labor force.
    
      laborsup_i <- 0.00001
      consumption_i <- cons(0.00001)
    
   }
   else {
     laborsup_i <- laborsup_i
     consumption_i <- cons(laborsup_i)
   }
  
  
   if(consumption_i <= cons(laborsup_i)){consumption_i <- consumption_i}    # If unemployed, the agent consumes without an income of wage.
   else {consumption_i <- cons(laborsup_i)}
  
   ccap_iw <- consumption_i - (1-tau)*w0*e_w*laborsup_i                     # Remaining capital of the agent

   id <- rbind(id, c(consumption_i,laborsup_i,ccap_iw,0, e_w))              # Information vector of the agent 

  
   vlaborsupply[i,j] <- laborsup_i                                          # We saved the labor supply of each agent, inside the labor supply matrix.
  
   esmat[i,j] <- e_w*laborsup_i                                             # We saved the "effective" labor supply of each agent, inside the "effective" labor supply matrix.
   
  }
   
  colnames(id) <- c("consumption","labor","currentcap","benefit", "ageeff")
  
  generations[[j]] <- id                                                    # List of the information of the given generation (here in case, only generation 1)
   
  n <- round(n/(1+p))                                                       # Since we start with the generation 1, the population of the next generations declines over time. 

  totallaborsup[j,2] <- sum(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.00001]))           # Column of total labor supply (in hours) for generations (fg)
  totallaborsup[j,3] <- length(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.00000]))        # Column of number of people in the labor force, fg
  totallaborsup[j,4] <- length(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.00001]))        # Column of number of employed, fg
  totallaborsup[j,5] <- length(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.17]))           # Column of number of full-time employed (more than %50), fg
  totallaborsup[j,6] <- totallaborsup[j,3] / length(na.omit(vlaborsupply[,j]))               # Column of labor-force participation rate, fg
  totallaborsup[j,7] <- ifelse(totallaborsup[j,6] == 0, 0, length(na.omit(vlaborsupply[,j][vlaborsupply[,j] == 0.00001])) / totallaborsup[j,3])        # Column of unemployment rate, fg
  totallaborsup[j,8] <- length(na.omit(vlaborsupply[,j]))                                    # Column of population size of the generation
}
  
  b_temp <- tau*w0*sum(na.omit(totallaborsup[,2]))                          # Temporary social security contribution for the retirees of the first period (probably lower than the actual)
  
  nvec <- c(n0)                                                             # Initial calculation of the number of retirees.
  for (k in 1:(J-2))
  {n_track <- round(nvec[k]/(1+p))
  nvec <- c(nvec,n_track)}
  nvec_ret <- nvec[((JR_N):(J-1))]
  
  num_retired <- sum(nvec_ret)
  
  b_r <- b_temp / num_retired                                               # Temporary pension benefit for the retirees of the first period (probably lower than the actual)

  
################################################################################
# Retired People
################################################################################

# In this section, we set the initial state of retired households for t = 0.
  
for(j in JR_N:(J-1)){                                                       # Generations
    
  id <- data.frame(matrix(ncol=5, nrow=0))
  
    
  for(i in 1:n){                                                            # Individuals
 
  ccap_ir <- runif(1, min=0, max=maxcap)                                    # Uniformly distributed initial capital
  e_r <- runif(1, min=(e[j]-margin_error), max=(e[j]+margin_error))         # Uniformly distributed age efficiency given interval for agent i
    
    
  cons <- function(x){

     b_r + (1-tau)*w0*x*e_r*(1-sgdp) + ccap_ir                              # Budget constraint, b > 0 , sgdp > 0
    
    }

  utility <- function(x){

      -((((cons(x)^gamma)*((1-x)^(1-gamma)))^(1-sigma))-1)/(1-sigma)        # Utility function

    }


  laborsup_i <- round(optimize(utility, c(-1,1), lower= 0, tol=0.00001)$minimum,digits=4)         # Optimization of the utility function
   
    
  if(((e_r*w0*laborsup_i) <= eflimwage) & (laborsup_i > 0)) {               # We determined if the agent is employed or unemployed given it is in the labor force.
      
      laborsup_i <- 0.00001
      consumption_i <- cons(0.00001)
      
  }
  else {
    laborsup_i <- laborsup_i
    consumption_i <- cons(laborsup_i)
  }
    
    
  if(consumption_i <= cons(laborsup_i)){consumption_i <- consumption_i}     # If unemployed, the agent consumes without an income of wage.
  else {consumption_i <- cons(laborsup_i)}
    
  ccap_ir <- consumption_i - (1-tau)*w0*e_r*laborsup_i                      # Remaining capital of the agent
    
  id <- rbind(id, c(consumption_i,laborsup_i,ccap_ir,b_r,e_r))              # Information vector of the agent
   
    
  vlaborsupply[i,j] <- laborsup_i                                           # We saved the labor supply of each agent, inside the labor supply matrix.
    
  esmat[i,j] <- e_r*laborsup_i                                              # We saved the "effective" labor supply of each agent, inside the "effective" labor supply matrix.
   
  }
    
  colnames(id) <- c("consumption","labor","currentcap","benefit","ageeff") 
    
  generations[[j]] <- id                                                    # List of the information of the given generation (here in case, only generation 1)
    
  n <- round(n/(1+p))                                                       # Since we start with the generation 1, the population of the next generations declines over time. 
    
  totallaborsup[j,2] <- sum(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.00001]))           # Column of total labor supply (in hours) for generations (fg)
  totallaborsup[j,3] <- length(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.00000]))        # Column of number of people in the labor force, fg
  totallaborsup[j,4] <- length(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.00001]))        # Column of number of employed, fg
  totallaborsup[j,5] <- length(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.17]))           # Column of number of full-time employed (more than 50%), fg
  totallaborsup[j,6] <- totallaborsup[j,3] / length(na.omit(vlaborsupply[,j]))               # Column of labor-force participation rate, fg
  totallaborsup[j,7] <- ifelse(totallaborsup[j,6] == 0, 0, length(na.omit(vlaborsupply[,j][vlaborsupply[,j] == 0.00001])) / totallaborsup[j,3])        # Column of unemployment rate, fg
  totallaborsup[j,8] <- length(na.omit(vlaborsupply[,j]))                                    # Column of population size of the generation
    
}

  
################################################################################

workpop <- sum(na.omit(totallaborsup[(1:(J-1)), 8]))                        # We calculated the whole population.
youthpop <- sum(na.omit(totallaborsup[(1:youth), 8]))                       # We calculated the youth population.
activepop <- sum(na.omit(totallaborsup[(JR_E0:(JR_N-1)), 8]))               # We calculated the active population (always most productive).
targetpop <- sum(na.omit(totallaborsup[(JR_E:(JR_E + 15)), 8]))             # We calculated the target population (most productive at that time and could be retired by the reform).
retpop <- sum(na.omit(totallaborsup[(JR_N:(J-1)), 8]))                      # We calculated the retired population.
  
  
workun <- as.numeric((na.omit(totallaborsup[(1:(J-1)), 8])/workpop) %*% as.matrix(na.omit(totallaborsup[(1:(J-1)), 7])))                           # We calculated the unemployment of the whole population.
youthun <- as.numeric((na.omit(totallaborsup[(1:youth), 8])/youthpop) %*% as.matrix(na.omit(totallaborsup[(1:youth), 7])))                         # We calculated the unemployment of the youth population.
worklfp <- as.numeric((na.omit(totallaborsup[(1:(J-1)), 8])/workpop) %*% as.matrix(na.omit(totallaborsup[(1:(J-1)), 6])))                          # We calculated the labor force participation of the whole population.
youthlfp <- as.numeric((na.omit(totallaborsup[(1:youth), 8])/youthpop) %*% as.matrix(na.omit(totallaborsup[(1:youth), 6])))                        # We calculated the labor force participation of the youth population.
activelfp <- as.numeric((na.omit(totallaborsup[(JR_E0:(JR_N-1)), 8])/activepop) %*% as.matrix(na.omit(totallaborsup[(JR_E0:(JR_N-1)), 6])))        # We calculated the labor force participation of the active population.
targetlfp <- as.numeric((na.omit(totallaborsup[(JR_E:(JR_E + 15)), 8])/targetpop) %*% as.matrix(na.omit(totallaborsup[(JR_E:(JR_E + 15)), 6])))    # We calculated the labor force participation of the targeted population.
retlfp <- as.numeric((na.omit(totallaborsup[(JR_N:(J-1)), 8])/retpop) %*% as.matrix(na.omit(totallaborsup[(JR_N:(J-1)), 6])))                      # We calculated the labor force participation of the retired population.
  

################################################################################

yearsls[[1]] <- totallaborsup       # We stored the results (labor supply decisions) of the agents in the first year inside a list.

yearsun[1,1] <- (starty-1)
yearsun[1,2] <- workun
yearsun[1,3] <- youthun
yearsun[1,4] <- worklfp
yearsun[1,5] <- youthlfp
yearsun[1,6] <- activelfp
yearsun[1,7] <- targetlfp
yearsun[1,8] <- retlfp
yearsun[1,9] <- 0                   # Effective labor supply (0 for the initial year because we will calculate it later.)
yearsun[1,10] <- 0                  # Limit of labor demand (0 for the initial year because we will calculate it later.)
yearsun[1,11] <- w0                 # Wage


################################################################################

# In this section, we calculated the total "effective" labor supplies of agents to update labor demand
# for the purpose of keeping unemployment unaffected by demand.

totalesmatvec <- c()

totalesmat <- sum(na.omit(esmat))

totalesmatvec <- c(totalesmatvec, totalesmat)

joblim <- totalesmat*un0            # Limit of labor demand

yearsun[1,9] <- totalesmat   
yearsun[1,10] <- joblim         

# Here, according to the labor demand, we calculated the "effective hour" of the last agent who is employed in the firm.

jl <- 1
xl <- 0
esmatsort <- c()
esmat_c <- sort(na.omit(esmat), TRUE)[1]
esmatsort <- c(esmatsort, esmat_c)

while((xl <= joblim) & (esmat_c > eflimwage)){         
  
  esmat_c <- sort(na.omit(esmat), TRUE)[jl+1]
  
  if(esmat_c <= eflimwage0) {break}
  
  esmatsort <- c(esmatsort, esmat_c)
  
  xl <- sum(na.omit(esmatsort))
  
  jl <- jl+1
  
}

 eflimwage <- tail(na.omit(esmatsort),1)
 
 
################################################################################

# In this section, we calculated the prices for the next year. 
 
K0 <- sum(unlist(lapply(generations, '[[', 3)))
L0 <- sum(esmatsort)
 

num_retired <- sum(totallaborsup[JR_N:(J-1),8])  
b_base <- tau*w0*L0
b_r <- b_base / num_retired

 
r0 <- alpha*(K0^(alpha-1))*(L0^(1-alpha))-delta         
w0 <- (1-alpha)*(K0^(alpha))*(L0^(-alpha))


################################################################################
# Getting older and population growth
################################################################################

# In this section, agents could start ageing until the end year.

for(t in 1:length(years)){                                                  # Years
  
  if((years[t] >= 2023) &  (JR_C <= JR_N)){JR_C <- JR_E}                    # We defined retirement shock since it will always influence the same group of people.
    else
      {JR_C <- JR_N}
  
  lapply(lapply(generations, '[[', 3), "*",(1+r0))                          # Capital of agens increase by the interest rate.
  
  
  for(r in (J-1):2){                                                        # Here, we carried the generations of individuals to the next generations (i.e. they aged one year). 
    
    generations[[r]] <- generations[[r-1]]
    
  }
  
  
  generations[[1]]$currentcap <- runif(nrow(generations[[1]]), min=0, max=maxcap)        # We distributed uniformly the initial capital of the new first generation.

  ns <- nc
  nc <- round(nc*(1+p))
  
  n_add <- nc-ns
  
  emptymat <- matrix(runif(n_add, min=0, max=maxcap), nrow = n_add, ncol=5)              # We added additional rows for the growing population, with their initial capital.
  
  colnames(emptymat) <- c("consumption","labor","currentcap","benefit","ageeff")
  
  generations[[1]] <- rbind(generations[[1]], emptymat)
  
  vlaborsupply <- matrix(NA,nc, (J-1))
  
  esmat <- matrix(NA,nc, (J-1))
  
  
  for(j in 1:(J - 1)){                                                      # Generations
    
    id <- data.frame(matrix(ncol=5, nrow=0))
    
    
    for(i in 1:nrow(generations[[j]])){                                     # Individuals
   
    ccap_t <- generations[[j]]$currentcap[i]                                # Remaining capital of the agent is transferred to the next year
    e_t <- runif(1, min=(e[j]-margin_error), max=(e[j]+margin_error))       # New age-efficiency according to the new age (might show jumps up or down as randomness suggests.)

    
    if(j < JR_C)                                                            # If the age of agent is lower than the retirement age, he or she could not retire and receive pension benefit (also cannot be fined by SGDP).
      {b_t <- 0}
    else
      {b_t <- b_r}

    if(j < JR_C)
      {sgdp <- 0}
    else
      {sgdp <- sgdp}


    cons <- function(x){

      b_t + (1-tau)*w0*x*e_t*(1-sgdp) + ccap_t                              # Budget constraint

    }

    utility <- function(x){
      
      -((((cons(x)^gamma)*((1-x)^(1-gamma)))^(1-sigma))-1)/(1-sigma)        # Utility function
      
    }
    
    
    laborsup_t <- round(optimize(utility, c(-1,1), lower= 0, tol=0.00001)$minimum,digits=4)         # Optimization of the utility function
    
    if(((e_t*w0*laborsup_t) <= eflimwage) & (laborsup_t > 0)) {             # We determined if the agent is employed or unemployed given it is in the labor force.
      
      laborsup_t <- 0.00001
      consumption_t <- cons(0.00001)
      
    }
    else {
      laborsup_t <- laborsup_t
      consumption_t <- cons(laborsup_t)
    }
    
    
    if(consumption_t <= cons(laborsup_t)){consumption_t <- consumption_t}   # If unemployed, the agent consumes without an income of wage.
    else {consumption_t <- cons(laborsup_t)}
    
    ccap_t <- consumption_t - (1-tau)*w0*e_t*laborsup_t                     # Remaining capital of the agent
    
    id <- rbind(id, c(consumption_t,laborsup_t,ccap_t,b_t,e_t))             # Information vector of the agent
    
    
    vlaborsupply[i,j] <- laborsup_t                                         # We saved the labor supply of each agent, inside the labor supply matrix.
    
    esmat[i,j] <- e_t*laborsup_t                                            # We saved the "effective" labor supply of each agent, inside the "effective" labor supply matrix.
    
  }
  
  colnames(id) <- c("consumption","labor","currentcap","benefit","ageeff") 
  
  generations[[j]] <- id                                                    # List of the information of the given generation (here in case, only generation j)
  
  
  totallaborsup[j,2] <- sum(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.00001]))           # Column of total labor supply (in hours) for generations (fg)
  totallaborsup[j,3] <- length(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.00000]))        # Column of number of people in the labor force, fg
  totallaborsup[j,4] <- length(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.00001]))        # Column of number of employed, fg
  totallaborsup[j,5] <- length(na.omit(vlaborsupply[,j][vlaborsupply[,j] > 0.17]))           # Column of number of full-time employed (more than 50%), fg
  totallaborsup[j,6] <- totallaborsup[j,3] / length(na.omit(vlaborsupply[,j]))               # Column of labor-force participation rate, fg
  totallaborsup[j,7] <- ifelse(totallaborsup[j,6] == 0, 0, length(na.omit(vlaborsupply[,j][vlaborsupply[,j] == 0.00001])) / totallaborsup[j,3])        # Column of unemployment rate, fg
  totallaborsup[j,8] <- length(na.omit(vlaborsupply[,j]))                                    # Column of population size of the generation
  
  }
  
  
################################################################################
  
  workpop <- sum(na.omit(totallaborsup[(1:(J-1)), 8]))                        # We calculated the whole population.
  youthpop <- sum(na.omit(totallaborsup[(1:youth), 8]))                       # We calculated the youth population.
  activepop <- sum(na.omit(totallaborsup[(JR_E0:(JR_N-1)), 8]))               # We calculated the active population (always most productive).
  targetpop <- sum(na.omit(totallaborsup[(JR_E:(JR_E + 15)), 8]))             # We calculated the target population (most productive at that time and could be retired by the reform).
  retpop <- sum(na.omit(totallaborsup[(JR_N:(J-1)), 8]))                      # We calculated the retired population.
 
  # We analyzed target group only for the influence period since it changes until the reform is implemented.
  
  workun <- as.numeric((na.omit(totallaborsup[(1:(J-1)), 8])/workpop) %*% as.matrix(na.omit(totallaborsup[(1:(J-1)), 7])))                           # We calculated the unemployment of the whole population.
  youthun <- as.numeric((na.omit(totallaborsup[(1:youth), 8])/youthpop) %*% as.matrix(na.omit(totallaborsup[(1:youth), 7])))                         # We calculated the unemployment of the youth population.
  worklfp <- as.numeric((na.omit(totallaborsup[(1:(J-1)), 8])/workpop) %*% as.matrix(na.omit(totallaborsup[(1:(J-1)), 6])))                          # We calculated the labor force participation of the whole population.
  youthlfp <- as.numeric((na.omit(totallaborsup[(1:youth), 8])/youthpop) %*% as.matrix(na.omit(totallaborsup[(1:youth), 6])))                        # We calculated the labor force participation of the youth population.
  activelfp <- as.numeric((na.omit(totallaborsup[(JR_E0:(JR_N-1)), 8])/activepop) %*% as.matrix(na.omit(totallaborsup[(JR_E0:(JR_N-1)), 6])))        # We calculated the labor force participation of the active population.
  targetlfp <- as.numeric((na.omit(totallaborsup[(JR_E:(JR_E + 15)), 8])/targetpop) %*% as.matrix(na.omit(totallaborsup[(JR_E:(JR_E + 15)), 6])))    # We calculated the labor force participation of the targeted population.
  retlfp <- as.numeric((na.omit(totallaborsup[(JR_N:(J-1)), 8])/retpop) %*% as.matrix(na.omit(totallaborsup[(JR_N:(J-1)), 6])))                      # We calculated the labor force participation of the retired population.
  
  
  yearsls[[(t+1)]] <- totallaborsup        # We stored the results (labor supply decisions) of the agents in the given year inside a list.
  
  yearsun[(t+1),1] <- years[t]
  yearsun[(t+1),2] <- workun
  yearsun[(t+1),3] <- youthun
  yearsun[(t+1),4] <- worklfp
  yearsun[(t+1),5] <- youthlfp
  yearsun[(t+1),6] <- activelfp
  yearsun[(t+1),7] <- targetlfp
  yearsun[(t+1),8] <- retlfp
  yearsun[(t+1),9] <- totalesmat            # Effective labor supply
  yearsun[(t+1),10] <- joblim               # Limit of labor demand
  yearsun[(t+1),11] <- w0                   # Wage

################################################################################
  
# In this section, we calculated the total "effective" labor supplies of agents to update labor demand
# for the purpose of keeping unemployment unaffected by demand.
  
  totalesmat <- sum(na.omit(esmat))
  totalesmatvec <- c(totalesmatvec, totalesmat)
  
  joblim <- joblim*(1+p)
  
  
  jl <- 1
  xl <- 0
  esmatsort <- c()
  esmat_c <- sort(na.omit(esmat), TRUE)[1]
  esmatsort <- c(esmatsort, esmat_c)
  
  
  while((xl <= joblim) & (esmat_c > eflimwage)){
    
    esmat_c <- sort(na.omit(esmat), TRUE)[jl+1]
    
    if(esmat_c <= eflimwage0) {break}
    
    esmatsort <- c(esmatsort, esmat_c)
    
    xl <- sum(na.omit(esmatsort))
    
    jl <- jl+1
    
  }

  eflimwage <- tail(na.omit(esmatsort),1)
  
  eflimw <- c(eflimw, eflimwage)

################################################################################

# In this section, we calculated the prices for the next year.   
  
 K0 <- sum(unlist(lapply(generations, '[[', 3)))
 L0 <- sum(esmatsort)
  
 
 num_retired <- sum(na.omit(totallaborsup[(JR_C:(J-1)), 8])) - sum(na.omit(totallaborsup[(JR_C:(J-1)), 3]))
 b_base <- tau*w0*L0
 b_r <- b_base / num_retired
  
 
 r0 <- alpha*(K0^(alpha-1))*(L0^(1-alpha))-delta         
 w0 <- (1-alpha)*(K0^(alpha))*(L0^(-alpha))


################################################################################  

# Here, we update the retirement age such that the reform should always influence the same group of people.
 
 if(JR_C < JR_N){
    
   JR_E <- JR_E + 1
    
 }
    
}

