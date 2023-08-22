#### Master script for the project ####

library(rmarkdown)

# Variation of seed viability on plant's crop along the fruiting season
render("code/crop_viability.Rmd")


# Bayesian models to extract posterior estimates:
#### Calculate consumption of viable fruits
render("code/stan_model_priors.Rmd")
"code/stan_calc_script.R" #run manually
render("code/assign_ID_stan_output.Rmd") #add bird and plant ID to code above


# Calculate prob. of microhabitat use for El Puntal site
#### This model is in Pistacia effectiveness repo - " (analysis_clean/Prob_MH_use_cover.Rmd)
#### Output is in data folder (files "Nseed.mh.cover.post.rds" and "Pdisperse.bird.mh.cover.rds")

# Estimate seed rain by birds at EP site
render("code/Calc_EP_seed_rain_by_birds.Rmd")


# Seedling emergence model
render("code/Prob.emergence.mh.Rmd")

# Seedling survival to 1st summer model
render("code/Prob.surv.1st.summer.Rmd")

# Seedling survival to 2nd summer model
render("code/Prob.surv.2nd.summer.Rmd")


# Calculate overall microhabitat quality
#### concatenate Prob.use + Prob.escape.pred + Prob.emer + Prob.surv1 + Prob.surv2
render("code/prep_mh_quality.Rmd") 


# Estimations propagules at along stages
render("code/Calc_propagules_plant_bird.Rmd") # by plant and bird - ATTENTION: Takes a long time!
render("code/Calc_propagules_POP_bird_mh.Rmd") # by bird and mh for the whole population 

# With these codes we extract the number of propagules passing to each demographic
# stage, we calculate these for plants, birds and microhabitats. The resulting data are 
# the following depending on the level of aggregation, focus and site considered:

# propagules_by_plant_and_bird.rds # focal plants level and for any given site (mh equally likely)
# propagules_by_plant_and_bird_EP.rds # focal plants level (n = 40) and for EP study site (mh cover considered)
# propagules_by_bird_and_MH_general_EP.rds # population level (all plants considered) and for EP study site
# propagules_by_plant.rds # focal plants level and for any given site
# propagules_by_plant_EP.rds # focal plants level and for EP study site
# propagules_by_bird.rds # focal plants level and for any given site
# propagules_by_bird_EP.rds # focal plants level and for EP study site
# propagules_by_bird_general_EP.rds # population level and for EP study site
# propagules_by_MH_general_EP.rds # population level and for EP study site


# Model crop effects on consumption
render("code/model_crop_and_viab_effects_on_dispersal.Rmd")

# Seed rain diversity
render("code/seed_rain_diversity.Rmd")

# General results (medians, CIs, correlations,... etc to communicate in the MS)
# render("code/General_results.Rmd")



# Figures and tables

# Viability in crop per plant
render("figs_code/fig_viab_ind_plant.Rmd")

# Viability of dispersed seed per bird
render("figs_code/fig_viab_birds.Rmd")

# Seed trap diversity
render("figs_code/fig_seed_rain_spatial_hills.Rmd")

# Frugivore's table
render("code/table_frug_seed_rain.Rmd")

# Figure bird's MH use and MH cover
render("figs_code/fig_bird_MH_use.Rmd")

# Transition between demographic stages
render("figs_code/fig_stage_transition_mh_pop.Rmd")
render("figs_code/fig_stage_transition_bird.Rmd")
render("figs_code/fig_stage_transition_plants.Rmd")

# Recruitment by plant and bird
render("figs_code/fig_recruitment_plants.Rmd")