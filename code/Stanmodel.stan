
data {
  
  int<lower=1> n_birds;
  int<lower=1> n_plants;
  
  int<lower=0> bird_float_Nviab[n_birds];
  int<lower=0> bird_float_Ncollected[n_birds];
  
  int<lower=0> plant_float_Nviab[n_plants];
  int<lower=0> plant_float_Ncollected[n_plants];
  
  int<lower=0> plant_bird_Nconsum[n_plants,n_birds];
  int<lower=0> plant_cropsize[n_plants];
  
}




parameters {
  
  vector[n_birds] bird_Pviab_logit;
  // real bird_Pviab_logit_mu;
  // real<lower=0> bird_Pviab_logit_sigma;
  
  vector[n_plants] plant_Pviab_logit;
  // real plant_Pviab_logit_mu;
  // real<lower=0> plant_Pviab_logit_sigma;

}





transformed parameters {
  
  vector<lower=0, upper=1>[n_plants] plant_Pviab = inv_logit(plant_Pviab_logit);
  vector<lower=0, upper=1>[n_birds] bird_Pviab = inv_logit(bird_Pviab_logit);

}





model {

  // Calculate OVERALL proportion of viable seeds consumed by each bird species
  // using data of floatability from seed traps and prior from videos
  bird_float_Nviab ~ binomial_logit(bird_float_Ncollected, bird_Pviab_logit); 
  bird_Pviab_logit ~ normal(1, 1);
  // bird_Pviab_logit ~ normal(bird_Pviab_logit_mu, bird_Pviab_logit_sigma); 
  // similar to glm(cbind(viable, noviable)) ~ (1 | bird_sp)
  // mu_bird_Pviab would have informative prior around 0.75, based on videos:
  // Probability of consuming black fruit * Prob. of viable being black + 
  // Prob. of consuming red fruit * Prob. viable being red
  // mu_bird_Pviab ~ normal(0.98*0.75 + 0.02*0.27, sigma);
  // 0.98*0.75 + 0.02*0.27 = 0.74
  // plogis(1) = 0.73
  // bird_Pviab_logit_mu ~ normal(1, 1);  
  // logit scale:
  // plogis(2) = 0.88
  // plogis(3) = 0.95
  // plogis(-1) = 0.27
  // plogis(0) = 0.5
  // bird_Pviab_logit_sigma ~ normal(0, 1);
  
  
  
  
  // Calculate proportion of viable seeds produced by each plant
  plant_float_Nviab ~ binomial_logit(plant_float_Ncollected, plant_Pviab_logit);
  plant_Pviab_logit ~ normal(0, 2);
  // plant_Pviab_logit_mu ~ normal(0, 2);
  // plant_Pviab_logit_sigma ~ normal(0, 2);
  // Pedro: 94.2% of black fruits, and 6.6% of red fruits, are viable
  // Juanpe: black fruits 82% viable in early period, 63% medium, 17% late season
  // use % of black vs red fruits from Elena's plants
  
}






generated quantities {
  
  matrix<lower=0>[n_plants,n_birds] plant_bird_Nviab_exp;
  matrix<lower=0>[n_plants,n_birds] plant_bird_consum_nonviab;
  matrix<lower=0, upper=1>[n_plants,n_birds] plant_bird_consum_nonviab_prop;
  matrix<lower=0, upper=1>[n_plants,n_birds] plant_bird_Nviab_exp_prop;
  matrix<lower=0>[n_plants,n_birds] plant_bird_Nviab_consum;
  
  matrix[n_plants,n_birds] plant_bird_Nviab_diff;
  matrix<lower=0>[n_plants,n_birds] plant_bird_Nviab_deficit;
  matrix<lower=0>[n_plants,n_birds] plant_bird_Nviab_avail;
  // matrix<lower=0, upper=1>[n_plants,n_birds] plant_bird_Nviab_pending_prop;
  matrix<lower=0, upper=1>[n_plants,n_birds] plant_bird_Nviab_avail_prop;
  matrix<lower=0>[n_plants,n_birds] plant_bird_Nviab_consum_pending;
  matrix<lower=0>[n_plants,n_birds] plant_bird_Nviab_total;
  
  vector<lower=0> [n_plants] plant_bird_Nviab_exp_sum;
  vector<lower=0> [n_plants] plant_Nviab;
  vector<lower=0> [n_plants] plant_max_consum_viab;
  vector<lower=0> [n_plants] plant_Nviab_excess;
  // vector<lower=0> [n_plants] plant_bird_Nviab_pending_avail_sum;
  vector [n_plants] plant_Nviab_prod_consum_diff;
  
  vector<lower=0> [n_birds] bird_Nviab_deficit;
  vector<lower=0> [n_birds] plant_bird_Nviab_avail_sum;

  

  // First, calculate the expected number of VIABLE seeds consumed by each bird
  // in each plant by just multiplying the total consumption x the average viability
  // in that bird species  
  for (j in 1:n_birds) {
    for (i in 1:n_plants) {
      plant_bird_Nviab_exp[i,j] = plant_bird_Nconsum[i,j] * bird_Pviab[j];
      plant_bird_consum_nonviab[i,j] = plant_bird_Nconsum[i,j] - plant_bird_Nviab_exp[i,j];
    }
  }
  
  
  
  
  // Now sum all the viable seeds consumed on each plant,
  // calculate the number of viable fruits produced by each plant,
  // and compare them
  for (i in 1:n_plants) {
    plant_bird_Nviab_exp_sum[i] = sum(row(plant_bird_Nviab_exp, i));  // sum of viable seeds consumed on each plant
    plant_Nviab[i] = plant_cropsize[i] * plant_Pviab[i];   // viable seeds produced by each plant
    plant_max_consum_viab[i] = fmin(plant_bird_Nviab_exp_sum[i], plant_Nviab[i]);  // max number of viable seeds consumed on each plant
    plant_Nviab_excess[i] = plant_Nviab[i] - plant_max_consum_viab[i]; // how many viable seeds remaining on each plant
  }
  
  
  
  // Calculate the expected proportion and number of viable seeds consumed 
  // by each bird species on each plant
  // accounting for viable seed availability on each plant.
  for (j in 1:n_birds) {
    for (i in 1:n_plants) {
      plant_bird_Nviab_exp_prop[i,j] = plant_bird_Nviab_exp[i,j] / plant_bird_Nviab_exp_sum[i];
      plant_bird_Nviab_consum[i,j] = plant_bird_Nviab_exp_prop[i,j] * plant_max_consum_viab[i];
    }
  }
  
  
  
  
  // Calculate the deficit of viable seeds consumption for each bird on each plant
  for (j in 1:n_birds) {
    for (i in 1:n_plants) {
      plant_bird_Nviab_diff[i,j] = plant_bird_Nviab_exp[i,j] - plant_bird_Nviab_consum[i,j];
      if (plant_bird_Nviab_diff[i,j] > 0)
        plant_bird_Nviab_deficit[i,j] = plant_bird_Nviab_diff[i,j];
      else
        plant_bird_Nviab_deficit[i,j] = 0;
    }
  bird_Nviab_deficit[j] = sum(col(plant_bird_Nviab_deficit, j));
  }

  // How do we distribute those pending viable seeds among those plants
  // that still have excess viable seeds? (plant_Nviab_excess > 0)

  // Calculate maximum number of viable seeds that each bird can consume on each plant
  // The maximum is to be chosen between the number of non-viable seeds consumed on each plant
  // and the number of viable seeds available on each plant, 
  // but the latter has to be shared among all the consuming birds in that plant
  // proportionally to their consumption intensity
  for (j in 1:n_birds) {
    for (i in 1:n_plants) {
      // First calculate the proportion of (apparently) non-viable seeds from each plant
      // consumed by each bird species
      // Some of these plants still have available viable seeds on excess
      // plant_bird_Nviab_pending_prop[i,j] = plant_bird_Nviab_pending[i,j] / sum(row(plant_bird_Nviab_pending, i));
      plant_bird_consum_nonviab_prop[i,j] = plant_bird_consum_nonviab[i,j] / sum(row(plant_bird_consum_nonviab, i));
      plant_bird_Nviab_avail[i,j] = fmin(plant_bird_consum_nonviab_prop[i,j] * plant_Nviab_excess[i], plant_bird_consum_nonviab[i,j]);  
    }
  }
  
  
  // Calculate proportion of viable seeds that each plant could provide
  // to each bird species
  for (j in 1:n_birds) {
    plant_bird_Nviab_avail_sum[j] = sum(col(plant_bird_Nviab_avail, j)); 
  }
  
  
  // Calculate the proportion of viable seeds from each plant that would be consumed
  // by each bird species
  for (j in 1:n_birds) {
    for (i in 1:n_plants) {
      if (plant_bird_Nviab_avail_sum[j] > 0)
        plant_bird_Nviab_avail_prop[i,j] = plant_bird_Nviab_avail[i,j] / plant_bird_Nviab_avail_sum[j];
      else
        plant_bird_Nviab_avail_prop[i,j] = 0;

      // Now calculate the number of viable seeds consumed by each bird species on each plant
      // as the product of the total deficit of viable seeds for that bird species
      // and the proportion of viable seeds that could be taken from each plant
      plant_bird_Nviab_consum_pending[i,j] = bird_Nviab_deficit[j] * plant_bird_Nviab_avail_prop[i,j];
      
      plant_bird_Nviab_total[i,j] = plant_bird_Nviab_consum[i,j] + plant_bird_Nviab_consum_pending[i,j];
    }
  }
  
  // check
  for (i in 1:n_plants) {
    plant_Nviab_prod_consum_diff[i] = plant_Nviab[i] - sum(row(plant_bird_Nviab_total, i));
  }
  
  
  
}
