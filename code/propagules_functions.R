
# Function to calculate propagules

calc_propagules_func <- function(dataset, site_filter = NULL, include_mh_cover = TRUE) {
  
  if (!is.null(site_filter)) {
    dataset <- dataset %>% filter(site == site_filter)
  }
  
  dataset <- dataset %>%
    ungroup() %>%
    mutate(Prob.escape.bird.pred = ifelse(bird_sp %in% 
                                            c("Cyanistes_caeruleus", "Parus_major",
                                              "Columba_palumbus"), 0, 
                                          Prob.escape.bird.pred))
  dataset <- dataset %>% 
    rowwise() %>%
    mutate(fru.disp = fruits.consumed.sp * Prob.escape.bird.pred) %>%
    mutate(fru.viab.disp = fru.cons.viab * Prob.escape.bird.pred)
  
  if (include_mh_cover) {
    
    dataset <- dataset %>% 
      mutate(P.escape = sum(EP.Prob.bird.mh_FR * Prob.escaping.pred_FR,
                            EP.Prob.bird.mh_NF * Prob.escaping.pred_NF,
                            EP.Prob.bird.mh_OA * Prob.escaping.pred_OA,
                            EP.Prob.bird.mh_PL * Prob.escaping.pred_PL,
                            EP.Prob.bird.mh_PP * Prob.escaping.pred_PP),
             viab.seed.surv.pred = fru.viab.disp * P.escape) %>%
      mutate(P.emer = sum(EP.Prob.bird.mh_FR * Prob.escaping.pred_FR * Prob.emergence_FR,
                          EP.Prob.bird.mh_NF * Prob.escaping.pred_NF * Prob.emergence_NF,
                          EP.Prob.bird.mh_OA * Prob.escaping.pred_OA * Prob.emergence_OA,
                          EP.Prob.bird.mh_PL * Prob.escaping.pred_PL * Prob.emergence_PL,
                          EP.Prob.bird.mh_PP * Prob.escaping.pred_PP * Prob.emergence_PP),
             viab.seed.emer = fru.viab.disp * P.emer) %>%
      mutate(P.surv1 = sum(EP.Prob.bird.mh_FR * Prob.escaping.pred_FR * Prob.emergence_FR * 
                             Prob.surv.1summer_FR,
                           EP.Prob.bird.mh_NF * Prob.escaping.pred_NF * Prob.emergence_NF * 
                             Prob.surv.1summer_NF,
                           EP.Prob.bird.mh_OA * Prob.escaping.pred_OA * Prob.emergence_OA * 
                             Prob.surv.1summer_OA,
                           EP.Prob.bird.mh_PL * Prob.escaping.pred_PL * Prob.emergence_PL * 
                             Prob.surv.1summer_PL,
                           EP.Prob.bird.mh_PP * Prob.escaping.pred_PP * Prob.emergence_PP * 
                             Prob.surv.1summer_PP),
             viab.seed.surv1 = fru.viab.disp * P.surv1) %>%
      mutate(P.surv2 = sum(EP.Prob.bird.mh_FR * Prob.escaping.pred_FR * Prob.emergence_FR * 
                             Prob.surv.1summer_FR * Prob.surv.2summer_FR,
                           EP.Prob.bird.mh_NF * Prob.escaping.pred_NF * Prob.emergence_NF * 
                             Prob.surv.1summer_NF * Prob.surv.2summer_NF,
                           EP.Prob.bird.mh_OA * Prob.escaping.pred_OA * Prob.emergence_OA * 
                             Prob.surv.1summer_OA * Prob.surv.2summer_OA,
                           EP.Prob.bird.mh_PL * Prob.escaping.pred_PL * Prob.emergence_PL * 
                             Prob.surv.1summer_PL * Prob.surv.2summer_PL,
                           EP.Prob.bird.mh_PP * Prob.escaping.pred_PP * Prob.emergence_PP * 
                             Prob.surv.1summer_PP * Prob.surv.2summer_PP),
             recruits = fru.viab.disp * P.surv2) %>%
      arrange(id_plant)
    
  } else {
    
    dataset <- dataset %>% 
      mutate(P.escape = sum(Prob.bird.mh_FR * Prob.escaping.pred_FR,
                            Prob.bird.mh_NF * Prob.escaping.pred_NF,
                            Prob.bird.mh_OA * Prob.escaping.pred_OA,
                            Prob.bird.mh_PL * Prob.escaping.pred_PL,
                            Prob.bird.mh_PP * Prob.escaping.pred_PP),
             viab.seed.surv.pred = fru.viab.disp * P.escape) %>%
      mutate(P.emer = sum(Prob.bird.mh_FR * Prob.escaping.pred_FR * Prob.emergence_FR,
                          Prob.bird.mh_NF * Prob.escaping.pred_NF * Prob.emergence_NF,
                          Prob.bird.mh_OA * Prob.escaping.pred_OA * Prob.emergence_OA,
                          Prob.bird.mh_PL * Prob.escaping.pred_PL * Prob.emergence_PL,
                          Prob.bird.mh_PP * Prob.escaping.pred_PP * Prob.emergence_PP),
             viab.seed.emer = fru.viab.disp * P.emer) %>%
      mutate(P.surv1 = sum(Prob.bird.mh_FR * Prob.escaping.pred_FR * Prob.emergence_FR * 
                             Prob.surv.1summer_FR,
                           Prob.bird.mh_NF * Prob.escaping.pred_NF * Prob.emergence_NF * 
                             Prob.surv.1summer_NF,
                           Prob.bird.mh_OA * Prob.escaping.pred_OA * Prob.emergence_OA * 
                             Prob.surv.1summer_OA,
                           Prob.bird.mh_PL * Prob.escaping.pred_PL * Prob.emergence_PL * 
                             Prob.surv.1summer_PL,
                           Prob.bird.mh_PP * Prob.escaping.pred_PP * Prob.emergence_PP * 
                             Prob.surv.1summer_PP),
             viab.seed.surv1 = fru.viab.disp * P.surv1) %>%
      mutate(P.surv2 = sum(Prob.bird.mh_FR * Prob.escaping.pred_FR * Prob.emergence_FR * 
                             Prob.surv.1summer_FR * Prob.surv.2summer_FR,
                           Prob.bird.mh_NF * Prob.escaping.pred_NF * Prob.emergence_NF * 
                             Prob.surv.1summer_NF * Prob.surv.2summer_NF,
                           Prob.bird.mh_OA * Prob.escaping.pred_OA * Prob.emergence_OA * 
                             Prob.surv.1summer_OA * Prob.surv.2summer_OA,
                           Prob.bird.mh_PL * Prob.escaping.pred_PL * Prob.emergence_PL * 
                             Prob.surv.1summer_PL * Prob.surv.2summer_PL,
                           Prob.bird.mh_PP * Prob.escaping.pred_PP * Prob.emergence_PP * 
                             Prob.surv.1summer_PP * Prob.surv.2summer_PP),
             recruits = fru.viab.disp * P.surv2) %>%
      arrange(id_plant)
    
  }
  
  return(dataset)
}

# Function to aggregate propagules by plants
plant_func <- function(dataset, var_col) {
  dataset %>%
    group_by(id_plant, bird_sp) %>%
    mutate(iter = c(1:1000)) %>%
    group_by(id_plant, iter) %>%
    summarise(var = sum({{var_col}}, na.rm = TRUE)) %>%
    arrange(id_plant) 
}

# Function to aggregate propagules by birds
bird_func <- function(dataset, var_col) {
  dataset %>%
    group_by(id_plant, bird_sp) %>%
    mutate(iter = c(1:1000)) %>%
    group_by(bird_sp, iter) %>%
    summarise(var = sum({{var_col}}, na.rm = TRUE)) %>%
    arrange(bird_sp)
}

# Function to aggregate propagules by microhabitats
mh_func <- function(dataset, var_col) {
  dataset %>%
    group_by(mh, bird_sp) %>%
    mutate(iter = c(1:1000)) %>%
    group_by(mh, iter) %>%
    summarise(var = sum({{var_col}}, na.rm = TRUE)) %>%
    arrange(mh)
}
