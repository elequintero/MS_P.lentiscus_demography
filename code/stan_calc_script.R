library(arm)
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)


#### Data ####

## Viability of dispersed seeds
disp <- readr::read_csv("data/dispersed_seed_viability.csv") |> 
  filter(bird_sp != "Alectoris rufa", !is.na(bird_sp)) |> 
  dplyr::select(-via.p) |> 
  arrange(bird_sp) |> 
  mutate(collected = sink + float,
         viab = round(sink / collected, 2)) |> 
  mutate(viab.logit.init = ifelse(is.na(viab), 0.3, viab),
         viab.logit.init = ifelse(viab.logit.init < 0.001, 0.001, viab.logit.init),
         viab.logit.init = logit(viab.logit.init)) 
disp


## Viability of seeds on plants

viab.plant <- readr::read_csv("data/crop_viab.csv") |> 
  dplyr::select(id_plant, total.fruits, viab, non.viab) |> 
  # add rows for 3 individual plants without floatability data
  add_row(id_plant = 334, total.fruits = 0, viab = 0, non.viab = 0) |> 
  add_row(id_plant = 340, total.fruits = 0, viab = 0, non.viab = 0) |> 
  add_row(id_plant = 371, total.fruits = 0, viab = 0, non.viab = 0) |> 
  #### Remove floatability data for some plants ####
  # filter(id_plant != 342) |>
  # add_row(id_plant = 342, total.fruits = 0, viab = 0, non.viab = 0) |>
  # filter(id_plant != 362) |>
  # add_row(id_plant = 362, total.fruits = 0, viab = 0, non.viab = 0) |>
  # filter(id_plant != 364) |>
  # add_row(id_plant = 364, total.fruits = 0, viab = 0, non.viab = 0) |>
  # filter(id_plant != 375) |>
  # add_row(id_plant = 375, total.fruits = 0, viab = 0, non.viab = 0) |>
  # filter(id_plant != 378) |>
  # add_row(id_plant = 378, total.fruits = 0, viab = 0, non.viab = 0) |>
  ##################
  arrange(id_plant) |> 
  mutate(prop = round(viab / total.fruits, 2)) |> 
  mutate(prop.logit.init = ifelse(is.na(prop), 0.3, prop)) |> 
  mutate(prop.logit.init = ifelse(prop.logit.init < 0.001, 0.001, prop.logit.init),
         prop.logit.init = logit(prop.logit.init))


## Consumption matrix

consum <- readr::read_csv("data/consumption_mat.csv") |> 
  arrange(id_plant) |> 
  dplyr::select(-1, -id_plant) %>%
  dplyr::select(sort(names(.))) |> 
  mutate(across(.cols = everything(), .fns = ~ round(.x))) 

names(consum) <- gsub("_", " ", names(consum))
all.equal(names(consum), disp$bird_sp)



## Plant crop size

crop <- readr::read_csv2("data/crop_ind_plants.csv", skip = 10) |> 
  arrange(plant_id) |> 
  mutate(cropsize = initcrop) 

all.equal(crop$plant_id, viab.plant$id_plant)


standata <- list(
  n_birds = ncol(consum),
  n_plants = nrow(consum),
  bird_float_Nviab = disp$sink,
  bird_float_Ncollected = disp$collected,
  plant_float_Nviab = viab.plant$viab,
  plant_float_Ncollected = viab.plant$total.fruits,
  plant_bird_Nconsum = consum,  
  plant_cropsize = crop$cropsize
)


#### Parameters ####

params <- c(
  "plant_Pviab",
  "bird_Pviab",
  # "bird_Pviab_logit_mu",
  # "bird_Pviab_logit_sigma",
  "plant_Nviab",
  "plant_Nviab_excess",
  "plant_bird_Nviab_exp",
  "plant_bird_Nviab_consum",
  "plant_bird_Nviab_deficit",
  "bird_Nviab_deficit",
  "plant_bird_Nviab_avail",
  "plant_bird_Nviab_consum_pending",
  "plant_bird_Nviab_total",
  "plant_Nviab_prod_consum_diff"
)



#### inits

init <- list(
  list(bird_Pviab_logit = disp$viab.logit.init,
       plant_Pviab_logit = viab.plant$prop.logit.init),
  list(bird_Pviab_logit = disp$viab.logit.init,
       plant_Pviab_logit = viab.plant$prop.logit.init),
  list(bird_Pviab_logit = disp$viab.logit.init,
       plant_Pviab_logit = viab.plant$prop.logit.init),
  list(bird_Pviab_logit = disp$viab.logit.init,
       plant_Pviab_logit = viab.plant$prop.logit.init)
)


#### Model 

mod <- stan(
  file = "code/Stanmodel.stan",
  data = standata,
  pars = params,
  # pars = "plant_Nviab_prod_consum_diff",
  init = init,
  iter = 4000,
  cores = 4,
  # control = list(adapt_delta = 0.99, max_treedepth = 15),
  thin = 8,
  verbose = TRUE
  )


#########################

mod

print(mod, "plant_Pviab")
print(mod, "bird_Pviab")
# print(mod, "bird_Pviab_logit_mu")
print(mod, "plant_Nviab")
print(mod, "plant_Nviab_excess")
print(mod, "plant_bird_Nviab_exp")
print(mod, "plant_bird_Nviab_consum")
print(mod, "plant_bird_Nviab_deficit")
print(mod, "bird_Nviab_deficit")
print(mod, "plant_bird_Nviab_avail")
print(mod, "plant_bird_Nviab_consum_pending")
print(mod, "plant_bird_Nviab_total")
print(mod, "plant_Nviab_prod_consum_diff")

# pending <- as.data.frame(mod, pars = "plant_bird_Nviab_pending")
# 
# prod <- as.data.frame(mod, pars = "Nviab_plant")
# prod <- apply(prod, 2, median)*100
# 
# cons <- as.data.frame(mod, pars = "Nviab_plant_bird_sum")
# cons <- apply(cons, 2, median)*100
# 
# which(prod < cons)




library(shinystan)
# launch_shinystan(mod)


pdf <- as.data.frame(mod)
pdf.redux = pdf
# pdf.redux <- slice_sample(pdf, n = 1000)
plant.Pviab <- pdf.redux |> 
  dplyr::select(starts_with("plant_Pviab"))
bird.Pviab <- pdf.redux |> 
  dplyr::select(starts_with("bird_Pviab["))
plant.Nviab <- pdf.redux |> 
  dplyr::select(starts_with("plant_Nviab["))
plant.bird.Nviab.total <- pdf.redux |> 
  dplyr::select(starts_with("plant_bird_Nviab_tot")) |> 
  mutate(across(.cols = everything(), .fns = ~ round(.x)))

# bird.Pviab.plant <- pdf.redux |> 
#   select(starts_with("bird_Pviab_plant"))
# Nviab.plant.bird <- pdf.redux |> 
#   select(starts_with("Nviab_plant_bird")) |> 
#   mutate(across(.fns = ~ .x * 100))


## Save
# saveRDS(mod, file = "analysis/stanfit.rds")  # too big. Thin!!
saveRDS(plant.Pviab, file = "data/plant.Pviab.rds")
saveRDS(bird.Pviab, file = "data/bird.Pviab.rds")
saveRDS(plant.bird.Nviab.total, file = "data/plant.bird.Nviab.total.rds")

