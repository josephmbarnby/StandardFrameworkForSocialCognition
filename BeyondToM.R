# Load libraries
library(tidyverse)
library(patchwork)

#Script for Barnby et al., 2023
#Beyond Theory of Mind: A formal framework for social inference and representation
#Authored by Joe Barnby
#CC-BY

# Generative Steady State (Figure 2) -------------------------------------------------
trials    <- 100
rewards   <- create_social_environ(trials)

#Define self and other starting beliefs
parms     <- rep(NA, 6)
parms[1]  <- -20 #self_mu
parms[3]  <- 20 #other_mu
parms[2]  <- 4 #self_s
parms[4]  <- 2 #other_s
parms[5]  <- 0.4 #l_ins
parms[6]  <- 0.2 #l_con

#resolution of grid and parameter upper limit
args      <- c(30, .03)

testrun.e <- convolution_SVO_h(parms, rewards, opts = args)

testrun.p1 <- testrun.e[[1]] %>%
  dplyr::select(5:8) %>%
  dplyr::slice(-1) %>%
  produce_distributions(lower = -args[1], upper = args[1], res = args[2], iterations = length(rewards[,1])) %>%
  make_data_tidy(iterations = length(rewards[,1]), res = args[2], lower = -args[1], upper = args[1])

ggplot(testrun.p1, aes(Value, Prob, colour = Type, group = iterations, alpha = iterations))+
  geom_point(size = 0.5)+
  geom_vline(xintercept = -args[1])+
  geom_hline(yintercept = 0)+
  labs(x = expression(paste(theta[j])), y = expression(paste('p(',theta[j],')')))+
  scale_alpha_continuous(guide = 'none')+
  scale_color_manual(values = c('#F78E69', '#7389AE'))+
  scale_x_continuous(expand=c(0,0), labels = function(x) ifelse(x == 0, "0", x),
                     breaks = c(-args[1], -args[1]/2, 0, args[1]/2, args[1]))+
  scale_y_continuous(expand=c(0,0), labels = function(x) ifelse(x == 0, "0", x))+
  coord_cartesian(xlim = c(-args[1],args[1]))+
  theme_minimal()+
  theme(text = element_text(size = 22),
        legend.position = 'none',
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_blank()
        )

# Generative Steady State (full bayes; Figure 2) -------------------------------------------------

trials    <- 100
rewards   <- create_social_environ(trials)

#Define self and other starting beliefs
self_mu   <- -20
other_mu  <- 20

#precision/sharpness of utility function
tau       <- 1

#resolution of grid and parameter upper limit
args      <- c(30, .03)

testrun.e <- convolution_SVO(c(self_mu, 2, other_mu, 2, tau), rewards, opts = args)
testrun.c <- convolution_SVO(c(self_mu, 5, other_mu, 1, tau), rewards, opts = args)
testrun.i <- convolution_SVO(c(self_mu, 1, other_mu, 5, tau), rewards, opts = args)

testrun.p1 <- make_data_tidy_dist(testrun.i)

ggplot(testrun.p1, aes(Value, Prob, colour = Type, group = iterations, alpha = iterations))+
  geom_point(size = 0.5)+
  geom_vline(xintercept = -args[1])+
  geom_hline(yintercept = 0)+
  labs(x = expression(paste(theta[j])), y = expression(paste('p(',theta[j],')')))+
  scale_alpha_continuous(guide = 'none')+
  scale_color_manual(values = c('#F78E69', '#7389AE'))+
  scale_x_continuous(expand=c(0,0), labels = function(x) ifelse(x == 0, "0", x),
                     breaks = c(-args[1], -args[1]/2, 0, args[1]/2, args[1]))+
  scale_y_continuous(expand=c(0,0), labels = function(x) ifelse(x == 0, "0", x))+
  coord_cartesian(xlim = c(-args[1],args[1]))+
  theme_minimal()+
  theme(text = element_text(size = 22),
        legend.position = 'none',
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_blank()
        )

testrun.e$outcomes %>%
  as.data.frame() %>%
  mutate(group = 'e', Same = testrun.e$outcomes$Same) %>%
  rbind(testrun.c$outcomes %>% as.data.frame() %>% mutate(group = 'c', Same = testrun.c$outcomes$Same),
        testrun.i$outcomes %>% as.data.frame() %>% mutate(group = 'i', Same = testrun.i$outcomes$Same)) %>%
  group_by(group) %>%
  mutate(trial = c(0, seq(1, nrow(rewards), 1))) %>%
  na.omit() %>%
  ggplot(aes(trial, Same, shape = group, linetype = group))+
  stat_smooth(method = "glm", se = F, method.args = list(family=binomial),
              linewidth = 2, colour = 'black')  +
  geom_jitter(size = 3, height = 0.1, alpha = 0.3, colour = 'black')+
  scale_shape_manual(values = c(21, 22, 23))+
  scale_x_continuous(expand=c(0,0))+
  coord_cartesian(xlim = c(0,nrow(rewards)))+
  scale_y_continuous(breaks = c(0,1))+
  labs(x = 'Iterations', y = expression(paste('p(', a^s, '=', a^o, ' | ', theta[j]^s, ',', theta[j]^o,')')))+
  theme_minimal()+
  theme(text = element_text(size = 22),
        legend.position = 'none',
        panel.grid = element_blank(),
        panel.border = element_blank()
        )

# Belief Updating (Shallow; Figure 3) -----------------------------------------------

reps                <- 100
partner1            <- -10
noise               <- 2
do                  <- 1

if(do==1){

    set.seed(101)
    #Simulate some partner action
    rewards             <- create_social_environ(reps)

    #Priors
    res                 <- 100
    beta_grid           <- seq(-30, 30, 1/res)
    beta_m              <- 30
    beta_sd             <- 5
    pb <- dnorm(beta_grid,beta_m,beta_sd)
    pb <- pb / sum(as.vector(pb));

    #One Person
    partner_decisions <- simulate_phase_decisions(partner1, noise, rewards)

    #Update beliefs
    save_posteriors      <- matrix(NA, length(beta_grid), reps+1)
    save_posteriors[,1]  <- pb
    for(i in 2:(reps+1)){
      if(i==2){
      save_posteriors[,i] <- incremental_fit(pb, data = partner_decisions[i,])
      } else {
      save_posteriors[,i] <- incremental_fit(save_posteriors[,i-1], data = partner_decisions[i-1,])
      }
    }

    #plot
    colnames(save_posteriors) <- c(1:reps, 'prior')
    long_post <- save_posteriors %>%
      as.data.frame()%>%
      mutate(beta = beta_grid) %>%
      pivot_longer(1:reps+1, names_to = 'Trial', values_to = 'prob')%>%
        filter(Trial != 'prior') %>%
        mutate(Trial = as.numeric(Trial))
}

ggplot(long_post, aes(beta, prob, group = Trial, color = Trial))+
  geom_line(linewidth = 1)+
  geom_vline(xintercept = partner1, linewidth = 1.5)+
  scale_color_gradient(low = 'white', high = '#F78E69', breaks = c(2,100), labels = c(0,100))+
  scale_x_continuous(expand=c(0,0.2), labels = function(x) ifelse(x == 0, "0", x))+
  scale_y_continuous(expand=c(0,0.0001), labels = function(x) ifelse(x == 0, "0", x))+
  theme_minimal()+
  theme(text = element_text(size = 16),
        legend.position = c(0.65, 0.75),
        #panel.grid = element_blank(),
        #panel.border = element_blank(),
        #axis.text = element_blank(),
        #axis.title = element_blank(),
        legend.key.height = unit(0.25,'cm')
        )

# Group (Independent; Figure 3) --------------------------------------------------------------

reps                <- 100
partner1            <- -10
partner2            <- -0
noise               <- 2
do                  <- 1

if(do==1){

  rewards             <- create_social_environ(reps)
  res                 <- 100
  beta_grid           <- seq(-30, 30, 1/res)
  beta_m              <- 30
  beta_sd             <- 5

  #Priors
  pb <- dnorm(beta_grid,beta_m,beta_sd)
  pb <- pb / sum(as.vector(pb));

  partner_decisions1 <- simulate_phase_decisions(partner1, noise, rewards)
  partner_decisions2 <- simulate_phase_decisions(partner2, noise, rewards)

  #Update beliefs
  save_posteriors1       <- matrix(NA, length(beta_grid), reps+1)
  save_posteriors1[,1]   <- pb
  save_posteriors2       <- matrix(NA, length(beta_grid), reps+1)
  save_posteriors2[,1]   <- pb

  for(i in 2:(reps+1)){
    if(i==2){
    save_posteriors1[,i] <- incremental_fit(pb, data = partner_decisions1[i-1,])
    save_posteriors2[,i] <- incremental_fit(pb, data = partner_decisions2[i-1,])
    } else {
    save_posteriors1[,i] <- incremental_fit(save_posteriors1[,i-1], data = partner_decisions1[i-1,])
    save_posteriors2[,i] <- incremental_fit(save_posteriors2[,i-1], data = partner_decisions2[i-1,])
    }
  }

  #plot
  save_posteriors_duo <- rbind(save_posteriors1, save_posteriors2) %>%
    as.data.frame() %>%
    mutate(Other=c(rep('O1', length(beta_grid)),rep('O2',length(beta_grid))))

  colnames(save_posteriors_duo) <- c(1:reps, 'prior','Other')
  long_post_duo <- save_posteriors_duo %>%
    mutate(beta = rep(beta_grid,2)) %>%
    pivot_longer(1:reps+1, names_to = 'Trial', values_to = 'prob') %>%
    filter(Trial != 'prior') %>%
    mutate(Trial = as.numeric(Trial))
}

ggplot()+
  geom_line(data = long_post_duo %>% filter(Other == 'O1'), aes(beta, prob, group = Trial, alpha = Trial, colour=Other),
             linewidth = 1)+
  geom_line(data = long_post_duo %>% filter(Other == 'O2'), aes(beta, prob, group = Trial, alpha = Trial, colour=Other),
             linewidth = 1)+
  scale_color_manual(values = c('#F78E69', '#4DAA57'))+
  geom_vline(xintercept = c(partner1, partner2), linewidth = 1.5)+
  scale_x_continuous(expand=c(0,0.2), labels = function(x) ifelse(x == 0, "0", x))+
  scale_y_continuous(expand=c(0,0.0001), labels = function(x) ifelse(x == 0, "0", x))+
  theme_minimal()+
  theme(text = element_text(size = 16),
        legend.position = 'none',
        #panel.grid = element_blank(),
        #panel.border = element_blank(),
        #axis.text = element_blank(),
        #axis.title = element_blank()
        )

# Group (Fixed Parameter; Figure 3) -------------------------------------------------

reps                <- 100
partner1            <- -10
partner2            <- 0
noise               <- 2
do                  <- 1

if(do==1){

  rewards             <- create_social_environ(reps)
  res                 <- 100
  beta_grid           <- seq(-30, 30, 1/res)
  beta_m              <- 30
  beta_sd             <- 5

  #Priors
  pb <- dnorm(beta_grid,beta_m,beta_sd)
  pb <- pb / sum(as.vector(pb));

  partner_decisions1m <- simulate_phase_decisions(partner1, noise, rewards)
  partner_decisions2m <- simulate_phase_decisions(partner2, noise, rewards)
  partner_decisionsM  <- partner_decisions1m %>% mutate(Ac2=partner_decisions2m[,'Ac'])

  #Update beliefs
  save_posteriorsM      <- matrix(NA, length(beta_grid), reps+1)
  save_posteriorsM[,1]  <- pb
  for(i in 2:(reps+1)){
    if(i==2){
    save_posteriorsM[,i] <- incremental_fit_meanfield(pb, data = partner_decisionsM[i-1,])
    } else {
    save_posteriorsM[,i] <- incremental_fit_meanfield(save_posteriorsM[,i-1], data = partner_decisionsM[i-1,])
    }
  }

  #plot
  colnames(save_posteriorsM) <- c(1:reps, 'prior')
  long_postM <- save_posteriorsM %>%
  as.data.frame() %>%
  mutate(beta = beta_grid) %>%
  pivot_longer(1:(reps+1), names_to = 'Trial', values_to = 'prob') %>%
  filter(Trial != 'prior') %>%
  mutate(Trial = as.numeric(Trial))

}

ggplot(long_postM, aes(beta, prob, group = Trial, colour = Trial))+
  geom_line(linewidth = 1, alpha = 1)+
  geom_vline(xintercept = c(partner1, partner2), linewidth = 1.5)+
  labs(y = 'post. prob', x = expression(theta[j]))+
  scale_x_continuous(expand=c(0,0.2), labels = function(x) ifelse(x == 0, "0", x))+
  scale_y_continuous(expand=c(0,0), labels = function(x) ifelse(x == 0, "0", x))+
  scale_colour_gradient2(low = '#D9F7FA', midpoint = reps/2, mid = '#7D8CC4', high = '#726DA8', name = 'Trial',
                         breaks = c(1,100), labels = c(0,100))+
  theme_minimal()+
  theme(text = element_text(size = 16),
        legend.position = c(0.65, 0.75),
        #panel.grid = element_blank(),
        #panel.border = element_blank(),
        #axis.text = element_blank(),
        #axis.title = element_blank(),
        legend.key.height = unit(0.25,'cm')
        )

# Functions ---------------------------------------------------------------

converge_beliefs_h <- function(iterations,
                             mu_self, mu_other,
                             lr_contagion, lr_insertion){

    for (t in 2:iterations) {

        q_self  <- mu_self[t-1]
        q_other <- mu_other[t-1]
        # Update expected mean self
        mu_self[t]  <- q_self  + lr_contagion * (q_other - q_self)
        # Update expected mean other
        mu_other[t] <- q_other + lr_insertion * (q_self - q_other)

    }

  return(list(self = mu_self, other = mu_other))

}

converge_beliefs_b <- function(iterations,
                               mu_selfP, sigma_selfP, mu_otherP, sigma_otherP,
                               rate_con, rate_ins){

      # Initialize matrix to store parameters
      params_matrix           <- matrix(nrow=iterations, ncol=4)
      colnames(params_matrix) <- c("mu_self", "sigma_self",  'mu_other', 'sigma_other')

      # Update parameters over time
      params_matrix[1, ] <- c(mu_selfP, sigma_selfP, mu_otherP, sigma_otherP)

      for (i in 2:iterations) {

        mu_self      <- params_matrix[i-1, 1]
        sigma_self   <- params_matrix[i-1, 2]
        mu_other     <- params_matrix[i-1, 3]
        sigma_other  <- params_matrix[i-1, 4]

        # Update parameters
        self_new  <- update_parameters_bayesian(mu_self,  sigma_self,  mu_other, sigma_other, rate_con)
        other_new <- update_parameters_bayesian(mu_other, sigma_other, mu_self,  sigma_self,  rate_ins)
        # Store parameters

        params_matrix[i, ] <- c(self_new[1], self_new[2], other_new[1], other_new[2])
      }

  return(params_matrix)

}

produce_distributions <- function(lower, upper, res, iterations, params_matrix){

      self <- matrix(NA, ((upper-lower)/res)+1, iterations)
      other<- self

      for(i in 1:iterations){
        self[,i] <- dnorm(seq(lower, upper, res), params_matrix[i,1], params_matrix[i,2])
        self[,i] <- self[,i]/sum(self[,i])
        other[,i]<- dnorm(seq(lower, upper, res), params_matrix[i,3], params_matrix[i,4])
        other[,i] <- other[,i]/sum(other[,i])
      }

  return(list(self = self, other = other))

}

produce_distributions_single <- function(lower, upper, res, mu, sigma){

  self <- dnorm(seq(lower, upper, upper/res), mu, sigma)
  self <- self/sum(self)

  return(self)

}

make_data_tidy <- function(outcomesb, iterations, res, lower, upper){

  colnames(outcomesb[[1]])  <- 1:iterations
  colnames(outcomesb[[2]])  <- 1:iterations

  string_rep <- iterations * (((upper-lower)/res)+1)
  df_long <- outcomesb[[1]] %>%
    as.data.frame() %>%
    mutate(Value = seq(lower, upper, res)) %>%
    pivot_longer(1:all_of(iterations), names_to = 'iterations', values_to = 'Prob') %>%
    mutate(iterations = as.numeric(iterations)) %>%
    rbind(
      outcomesb[[2]] %>%
      as.data.frame() %>%
      mutate(Value = seq(lower, upper, res)) %>%
      pivot_longer(1:all_of(iterations), names_to = 'iterations', values_to = 'Prob') %>%
      mutate(iterations = as.numeric(iterations))) %>%
    mutate(Type = c(rep('Self', string_rep), rep('Other', string_rep)))

  return(df_long)

}

make_data_tidy_dist <- function(outcomes){

  colnames(outcomes[[2]]) <- 1:(ncol(outcomes[[2]]))
  colnames(outcomes[[3]]) <- 1:(ncol(outcomes[[3]]))

  outcomes_edit <- outcomes[[2]] %>%
  as.data.frame() %>%
  mutate(Value = seq(-args[1], args[1], args[2])) %>%
  pivot_longer(1:ncol(testrun.i[[2]]), names_to = 'iterations', values_to = 'Prob') %>%
  mutate(Type = 'Self', iterations = as.numeric(iterations)) %>%
  rbind(
  outcomes[[3]] %>%
    as.data.frame() %>%
    mutate(Value = seq(-args[1], args[1], args[2])) %>%
    pivot_longer(1:ncol(testrun.i[[2]]), names_to = 'iterations', values_to = 'Prob') %>%
    mutate(Type = 'Other', iterations = as.numeric(iterations))
  )

  return(outcomes_edit)

}

# Function to update parameters
update_parameters_bayesian <- function(mu1, sigma1, mu2, sigma2, rate) {

  # Compute the discrepancy between the expected value of Distribution 2 and the actual value of Distribution 2
  discrepancy_mu    <- mu1 - mu2
  discrepancy_sigma <- sigma1 - sigma2

  # Compute the adjustment factor
  adjustment_factor_mu    <- rate * (discrepancy_mu / (sigma1 + sigma2))
  adjustment_factor_sigma <- rate * (discrepancy_sigma / (sigma1 + sigma2))

  # Update parameters
  mu1_new    <- mu1 - adjustment_factor_mu
  sigma1_new <- sigma1 - adjustment_factor_sigma

  return(c(mu1_new, sigma1_new))
}

simulate_phase_decisions <- function(beta_m, beta_sd, data){

  # Initialise
  T1  = nrow(data);  # trials
  res = 100

  #Priors
  beta_grid <- seq(-30, 30, 1/res)
  pb <- dnorm(beta_grid,beta_m,beta_sd)
  pb <- pb / sum(as.vector(pb));

  # initialised dummy values
  decisions        <- data.frame(
    ppt1 = rep(NA, T1),
    par1 = rep(NA, T1),
    ppt2 = rep(NA, T1),
    par2 = rep(NA, T1),
    Ac   = rep(NA, T1)
  )

  # Phase 1 choices of the participant

    for (t in 1:T1){

    s1 = as.numeric(data[t, 1]);
    o1 = as.numeric(data[t, 2]);
    s2 = as.numeric(data[t, 3]);
    o2 = as.numeric(data[t, 4]);

    decisions[t, 1] = s1
    decisions[t, 2] = o1
    decisions[t, 3] = s2
    decisions[t, 4] = o2

    val1 = s1 + beta_grid*max(s1-o1,0) ;
    val2 = s2 + beta_grid*max(s2-o2,0) ;

    pchoose1=(1/(1+exp(-(val1 - val2)))) * pb; # probability of 1
    simA = sample(c(1,2),1, prob = c(sum(as.vector(pchoose1)), 1-sum(as.vector(pchoose1))));

    decisions[t, 5] = simA;

    }
      return(decisions)
} # end of function

incremental_fit <- function(priors, data, stim_u = 1){

    res = 100
    #parameters space of alpha and beta
    grid        <- seq(-30, 30, 1/res);
    posterior   <- priors
    length_data <- nrow(data)

      for (t in 1:length_data){

        s1 = as.numeric(data[t, 1]/10);
        o1 = as.numeric(data[t, 2]/10);
        s2 = as.numeric(data[t, 3]/10);
        o2 = as.numeric(data[t, 4]/10);

        val1 = 1*s1 + grid*max(s1-o1,0) ;
        val2 = 1*s2 + grid*max(s2-o2,0) ;

        subject_choice = data[t,'Ac']

          if (subject_choice==1){
            pchoose2=(1./(1+exp(-(val1 - val2)))); # probability of 1
          } else {
            pchoose2=(1./(1+exp(-(val2 - val1)))); # probability of 2
          }

        init_post = (pchoose2^(1/stim_u)) * posterior; # Bayes rule
        posterior = init_post / sum(as.vector(init_post)); #normalised distribution
        #posterior = init_post^(1/stim_u)
        #posterior = posterior / sum(as.vector(posterior));

      }
    return(posterior)
}

incremental_fit_meanfield <- function(priors, data){

    res = 100
    #parameters space of alpha and beta
    grid        <- seq(-30, 30, 1/res);
    posterior   <- priors
    length_data <- nrow(data)

      for (t in 1:length_data){

        s1 = as.numeric(data[t, 1]/10);
        o1 = as.numeric(data[t, 2]/10);
        s2 = as.numeric(data[t, 3]/10);
        o2 = as.numeric(data[t, 4]/10);

        val1 = 1*s1 + grid*max(s1-o1,0) ;
        val2 = 1*s2 + grid*max(s2-o2,0) ;

        subject_choice1 = data[t,'Ac']
        subject_choice2 = data[t,'Ac2']

          if (subject_choice1==1){
            pchoose2a=(1./(1+exp(-(val1 - val2)))); # probability of 1
          } else {
            pchoose2a=(1./(1+exp(-(val2 - val1)))); # probability of 2
          }

        posterior = pchoose2a * posterior; # Bayes rule
        posterior = posterior / sum(as.vector(posterior)); #normalised distribution

          if (subject_choice2==1){
            pchoose2b=(1./(1+exp(-(val1 - val2)))); # probability of 1
          } else {
            pchoose2b=(1./(1+exp(-(val2 - val1)))); # probability of 2
          }

        posterior = pchoose2b * posterior; # Bayes rule
        posterior = posterior / sum(as.vector(posterior)); # Bayes rule

      }

    return(posterior)
}

create_social_environ <- function(reps, seed = 101){

    set.seed(seed)
    rewards <- matrix(NA, nrow = reps, ncol = 4)
    for(i in 1:4){
      rewards[,i] <- sample(seq(1, 10, 1))
      }
    for(i in 1:reps){
      if(rewards[i,1]<rewards[i,2]){
        a=rewards[i,2]; b=rewards[i,1]; rewards[i,1:2]=c(a,b)
        }
      if(rewards[i,3]<rewards[i,4]){
        a=rewards[i,4]; b=rewards[i,3]; rewards[i,3:4]=c(a,b)
        }
    }

  return(rewards)

}

convolution_SVO <- function(parms, rewards, opts){

  running_avg_length = 20
  re_ac              = rep(NA, running_avg_length)

  # Initialise
  if(is.null(opts) || length(opts) == 0){
    res = 30
    inc = 0.05
  } else {
    res = opts[1]; # bounds of belief grid
    inc = opts[2]; # resolution of grid
  }

  #parameters space of alpha and beta
  grid       <- seq(-res, res, inc);
  beta_grid  <- grid

  trials = length(rewards[,1])

  free_parms         = length(parms)
    if(free_parms<5){
      temp=1
    }else{
        temp=parms[5]
    }

    #output list
  simulatedDF               = init_simulatedDF(rewards, res=length(grid))

  beta                      = parms[1];
  beta_v                    = parms[2];
  beta_par                  = parms[3];
  beta_v_par                = parms[4];

  if(any(parms[c(2,4)]<0) | any(parms[c(1,3)]< -res) | any(parms[c(1,3)]>res)){
    warning('Variances are not within a sensible range. Please redefine')
  }

  #generate standardised grid to form priors for preferences
  pabg       <- dnorm(beta_grid,beta,beta_v);
  pabg       <- pabg / sum(pabg);

  simulatedDF[[2]][,1] <- pabg

  pabg_par   <- dnorm(beta_grid,beta_par,beta_v_par);
  pabg_par   <- pabg_par / sum(as.vector(pabg_par));

  simulatedDF[[3]][,1] <- pabg_par

  # Phase 1 choices of the participant

    for (t in 2:(trials+1)){

      s1      = as.numeric(rewards[t-1, 1]/10);
      o1      = as.numeric(rewards[t-1, 2]/10);
      s2      = as.numeric(rewards[t-1, 3]/10);
      o2      = as.numeric(rewards[t-1, 4]/10);

      val1    = value_function_FS(s1, o1, beta_grid)
      val2    = value_function_FS(s2, o2, beta_grid)

      pchoose1=action_selection_1(val1, val2, temp); # probability of 1

      #likelihood
      tmp_ppt=pchoose1*pabg;
      tmp_par=pchoose1*pabg_par;

      #probability of 1
      subject_netp1 = sum(tmp_ppt);
      partner_netp1 = sum(tmp_par);

      #sim action
      sa = sample(c(1,2), 1, prob = c(subject_netp1, 1-subject_netp1));
      oa = sample(c(1,2), 1, prob = c(partner_netp1, 1-partner_netp1));

      #congruency check and look back

      con           = ifelse(sa==oa, 1, 0)
      re_ac         = c(re_ac[-1], con)
      av_con        = mean(re_ac, na.rm=TRUE)

      if(con == 0 & av_con < 0.95){
       pabg     = bayesian_update(pabg,     pchoose1, oa)
       pabg_par = bayesian_update(pabg_par, pchoose1, sa)
      }

      simulatedDF[[2]][,t]               <- pabg
      simulatedDF[[3]][,t]               <- pabg_par
      simulatedDF$outcomes$PPTActions[t] <- sa
      simulatedDF$outcomes$PARActions[t] <- oa
      simulatedDF$outcomes$Same[t]       <- con

    }

  return(simulatedDF)

}#end of func

convolution_SVO_h <- function(parms, rewards, opts){

  running_avg_length = 20
  re_ac              = rep(NA, running_avg_length)

  # Initialise
  if(is.null(opts) || length(opts) == 0){
    res = 30
    inc = 0.05
  } else {
    res = opts[1]; # bounds of belief grid
    inc = opts[2]; # resolution of grid
  }

  #parameters space of alpha and beta
  grid       <- seq(-res, res, inc);
  beta_grid  <- grid

  trials = length(rewards[,1])

  free_parms         = length(parms)
    if(free_parms<7){
      temp=1
    }else{
        temp=parms[7]
    }

    #output list
  simulatedDF               = init_simulatedDF(rewards, res=length(grid))

  beta                      = parms[1];
  beta_v                    = parms[2];
  beta_par                  = parms[3];
  beta_v_par                = parms[4];
  rate_ins                  = parms[5];
  rate_con                  = parms[6];

  if(any(parms[c(2,4)]<0) | any(parms[c(1,3)]< -res) | any(parms[c(1,3)]>res)){
    warning('Variances are not within a sensible range. Please redefine')
  }

  simulatedDF[[1]]$beta[1]       = beta
  simulatedDF[[1]]$beta_v[1]     = beta_v
  simulatedDF[[1]]$beta_par[1]   = beta_par
  simulatedDF[[1]]$beta_v_par[1] = beta_v_par

  #generate standardised grid to form priors for preferences
  pabg       <- dnorm(beta_grid,beta,beta_v);
  pabg       <- pabg / sum(pabg);

  simulatedDF[[2]][,1] <- pabg

  pabg_par   <- dnorm(beta_grid,beta_par,beta_v_par);
  pabg_par   <- pabg_par / sum(as.vector(pabg_par));

  simulatedDF[[3]][,1] <- pabg_par

  # Phase 1 choices of the participant

    for (t in 2:(trials+1)){

      s1      = as.numeric(rewards[t-1, 1]/10);
      o1      = as.numeric(rewards[t-1, 2]/10);
      s2      = as.numeric(rewards[t-1, 3]/10);
      o2      = as.numeric(rewards[t-1, 4]/10);

      val1    = value_function_FS(s1, o1, beta_grid)
      val2    = value_function_FS(s2, o2, beta_grid)

      pchoose1=action_selection_1(val1, val2, temp); # probability of 1

      #likelihood
      tmp_ppt=pchoose1*pabg;
      tmp_par=pchoose1*pabg_par;

      #probability of 1
      subject_netp1 = sum(tmp_ppt);
      partner_netp1 = sum(tmp_par);

      #sim action
      sa = sample(c(1,2), 1, prob = c(subject_netp1, 1-subject_netp1));
      oa = sample(c(1,2), 1, prob = c(partner_netp1, 1-partner_netp1));

      #congruency check and look back

      con           = ifelse(sa==oa, 1, 0)
      re_ac         = c(re_ac[-1], con)
      av_con        = mean(re_ac, na.rm=TRUE)

      #learning_rate_ins         = (rate)   #+ 0.5 * (0.5-simulatedDF$Same[t])) #if dynamic
      #learning_rate_con         = (1-rate) #+ 0.5 * (0.5-simulatedDF$Same[t])) #if dynamic

      if(con == 0 & av_con < 0.95){

        # Update parameters
        self_new  <- update_parameters_bayesian(simulatedDF[[1]]$beta[t-1],      simulatedDF[[1]]$beta_v[t-1],
                                                simulatedDF[[1]]$beta_par[t-1],  simulatedDF[[1]]$beta_v_par[t-1],
                                                rate_con)
        other_new <- update_parameters_bayesian(simulatedDF[[1]]$beta_par[t-1],  simulatedDF[[1]]$beta_v_par[t-1],
                                                simulatedDF[[1]]$beta[t-1],      simulatedDF[[1]]$beta_v[t-1],
                                                rate_ins)
        # Store parameters

        simulatedDF[[1]]$beta[t]       = self_new[1]
        simulatedDF[[1]]$beta_v[t]     = self_new[2]
        simulatedDF[[1]]$beta_par[t]   = other_new[1]
        simulatedDF[[1]]$beta_v_par[t] = other_new[2]

        pabg     = dnorm(beta_grid, simulatedDF[[1]]$beta[t], simulatedDF[[1]]$beta_v[t])
        pabg     = pabg/sum(as.vector(pabg))
        pabg_par = dnorm(beta_grid, simulatedDF[[1]]$beta_par[t], simulatedDF[[1]]$beta_v_par[t])
        pabg_par = pabg_par/sum(as.vector(pabg_par))

      } else {

        simulatedDF[[1]]$beta[t]       = simulatedDF[[1]]$beta[t-1]
        simulatedDF[[1]]$beta_v[t]     = simulatedDF[[1]]$beta_v[t-1]
        simulatedDF[[1]]$beta_par[t]   = simulatedDF[[1]]$beta_par[t-1]
        simulatedDF[[1]]$beta_v_par[t] = simulatedDF[[1]]$beta_v_par[t-1]

      }

      simulatedDF[[2]][,t]               <- pabg
      simulatedDF[[3]][,t]               <- pabg_par
      simulatedDF[[1]]$PPTActions[t]     <- sa
      simulatedDF[[1]]$PARActions[t]     <- oa
      simulatedDF[[1]]$Same[t]           <- con

    }

  return(simulatedDF)

}#end of func

value_function_FS <- function(s1, o1, param){

  u <- s1 + (param * max(s1-o1, 0))

return(u)

}

action_selection_1 <- function(u1, u2, temp){

  1./(1+exp(-(u1-u2) * temp))

}

init_simulatedDF <- function(rewards, res) {

  list(
  outcomes <- data.frame(
    s1           = rewards[,1],
    o1           = rewards[,2],
    s2           = rewards[,3],
    o2           = rewards[,4],
    beta         = rep(NA, nrow(rewards)),
    beta_v       = rep(NA, nrow(rewards)),
    beta_par     = rep(NA, nrow(rewards)),
    beta_v_par   = rep(NA, nrow(rewards)),
    PPTActions   = rep(NA, nrow(rewards)),
    PARActions   = rep(NA, nrow(rewards)),
    Same         = rep(NA, nrow(rewards))
  ) %>%
    add_row(.before = 1),
  beliefs_ppt <- matrix(
    NA,
    ncol = nrow(rewards)+1,
    nrow = res),
  beliefs_par <- matrix(
    NA,
    ncol = nrow(rewards)+1,
    nrow = res)
  )
}

bayesian_update <- function(b, o, a){

  if(a==1){
      b     = b * o
      b     = b/sum(b)
  } else {
      b     = b * (1-o)
      b     = b/sum(b)
  }

  return(b)

}

# Heuristic Steady state --------------------------------------------------

# Set parameters

iterations    <- 50        # trials/iterations of algorithm
res           <- 1000      # resolution of belief grid
upper         <- 1         # upper limit of belief grid
lower         <- upper/res # lower limit of belief grid

mu_selfP      <- 0.1 * upper #make the real value between 0 and 1, and then the scaling make it relative to the upper bound
mu_otherP     <- 0.9 * upper
sigma_selfP   <- 0.2 * upper
sigma_otherP  <- 0.1 * upper

mu_self       <- rep(0, iterations); mu_self[1]  <- mu_selfP
mu_other      <- rep(0, iterations); mu_other[1] <- mu_otherP

lr_iterations <- seq(0.01, 0.1, 0.001) # vector of rates to test

for(i in lr_iterations){

  lr_contagion  <- i
  lr_insertion  <- i
  outcomesh     <- converge_beliefs_h(iterations,
                                     mu_self, mu_other,
                                     lr_insertion, lr_contagion)

  df_change1    <- data.frame(Value = seq(1, iterations, 1),
                             Self   = outcomesh[['self']],
                             Other  = outcomesh[['other']],
                             lr_i   = lr_insertion,
                             lr_c   = lr_contagion)

  if(i == lr_iterations[1]){
    df_change = df_change1
  }else{
    df_change <- rbind(df_change, df_change1)
  }
}

ggplot(df_change, aes(x = Value, alpha = lr_i, group = lr_c))+
  geom_line(aes(y = Self))+
  geom_line(aes(y = Other))

outcomesb  <- converge_beliefs_b(iterations,
                                mu_selfP, sigma_selfP, mu_otherP, sigma_otherP,
                                rate_con=0.01, rate_ins=0.01) %>%
             produce_distributions(., lower = lower, upper = upper, res = res, iterations = iterations) %>%
             make_data_tidy(iterations = iterations, res = res, lower = lower, upper = upper)
outcomesbI <- converge_beliefs_b(iterations,
                                mu_selfP, sigma_selfP, mu_otherP, sigma_otherP,
                                rate_con=0.01, rate_ins=0.05) %>%
             produce_distributions(lower = lower, upper = upper, res = res, iterations = iterations) %>%
             make_data_tidy(iterations = iterations, res = res, lower = lower, upper = upper)
outcomesbC <- converge_beliefs_b(iterations,
                                mu_selfP, sigma_selfP, mu_otherP, sigma_otherP,
                                rate_con=0.05, rate_ins=0.01) %>%
             produce_distributions(lower = lower, upper = upper, res = res, iterations = iterations) %>%
             make_data_tidy(iterations = iterations, res = res, lower = lower, upper = upper)

bupdate <- ggplot(outcomesb, aes(Value, Prob, colour = Type, group = iterations, alpha = iterations))+
  geom_point(size = 0.5)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  labs(x = expression(paste(theta[j])), y = expression(paste('p(',theta[j],')')))+
  scale_alpha_continuous(guide = 'none')+
  scale_color_manual(values = c('#F78E69', '#7389AE'), name = 'Representation')+
  scale_x_continuous(expand=c(0,0), labels = function(x) ifelse(x == 0, "0", x), breaks = c(0, 0.25, 0.5, 0.75, 1))+
  coord_cartesian(xlim = c(0,1))+
  scale_y_continuous(expand=c(0,0), labels = function(x) ifelse(x == 0, "0", x))+
  guides(color = guide_legend(override.aes = list(shape = 15, size = 5)))+
  theme_minimal()+
  theme(text = element_text(size = 22),
        legend.position = 'none',
        panel.grid = element_blank(),
        panel.border = element_blank()
        )
bupdate

hupdate <- ggplot(df_change, aes(x = Value, alpha = lr_i, group = lr_c))+
  geom_line(aes(y = Self), colour = '#7389AE')+
  geom_line(aes(y = Other), colour = '#F78E69')+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  labs(x = 'Iterations', y = expression(paste(theta[j]^mu)))+
  scale_alpha_continuous(range = c(0.1, 1),guide = 'none')+
  scale_x_continuous(expand=c(0,0), labels = function(x) ifelse(x == 0, "0", x))+
  scale_y_continuous(expand=c(0,0), labels = function(x) ifelse(x == 0, "0", x))+
  theme_bw()+
  theme(text = element_text(size = 22),
        legend.position = 'none',
        panel.grid = element_blank(),
        panel.border = element_blank()
        )

(bupdate+hupdate) &
  plot_layout(widths = c(2,1))

#Derivative

deriv <- rbind(
outcomesb %>%
  group_by(Type, iterations) %>%
  mutate(max = ifelse(Prob == max(Prob), 1,0)) %>%
  filter(max != 0) %>%
  pivot_wider(names_from = Type, id_cols = iterations, values_from = Value) %>%
  mutate(diff = Self - Other, Frame = 'equal'),
outcomesbC %>%
  group_by(Type, iterations) %>%
  mutate(max = ifelse(Prob == max(Prob), 1,0)) %>%
  filter(max != 0) %>%
  pivot_wider(names_from = Type, id_cols = iterations, values_from = Value) %>%
  mutate(diff = Self - Other, Frame = 'con'),
outcomesbI %>%
  group_by(Type, iterations) %>%
  mutate(max = ifelse(Prob == max(Prob), 1,0)) %>%
  filter(max != 0) %>%
  pivot_wider(names_from = Type, id_cols = iterations, values_from = Value) %>%
  mutate(diff = Self - Other, Frame = 'ins')
)

ggplot(deriv, aes(iterations, diff, linetype = Frame))+
  geom_line(linewidth = 2)+
  labs(x = 'Iteration', y = expression(paste('E[',theta[j]^s,'] - E[',theta[j]^o,']')))+
  theme_bw()+
  theme(text = element_text(size = 20),
        axis.title = element_text(size = 18),
        legend.position = 'none',
        panel.grid = element_blank(),
        panel.border = element_blank()
        )
