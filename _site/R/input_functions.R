## These functions will be used as input functions into summary table functions, 
## barcharts, boxplots, etc.

# -----------------------------------------------------------------------------

## if one variabalbe
## this means that you compare ability by age, or gender, or worker status

group1_nhts <- function(data1, data2, join_by, by_var, age1 = 18, age2 = 65){
  
  quote_var <- enquo(by_var)

  
  data1 %>% left_join(data2, by = join_by) %>%
    mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                               medcond6 == "02" | medcond6 == "03" ~ "Disabled",  
                               medcond == "02" ~ "Abled")) %>% 
    filter(r_age >= age1 & r_age < age2,
           !!quote_var > 0,
           Ability != "NA") %>%
    group_by(Ability)
  
  
}



# -----------------------------------------------------------------------------

## if two variabalbes
## this means that you compare ability by age and gender, or by worker status and income

group2_nhts <- function(data1, data2, join_by, by_var_quant, by_var_qual, age1 = 18, age2 = 65){
  
  quote_var <- enquo(by_var_quant)
  quote_var1 <- enquo(by_var_qual)
  
  data1 %>% left_join(data2, by = join_by) %>%
    mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                               medcond6 == "02" | medcond6 == "03" ~ "Disabled",  
                               medcond == "02" ~ "Abled")) %>% 
    filter(r_age >= age1 & r_age < age2,
           !!quote_var > 0,
           !!quote_var1 > 0,
           Ability != "NA") %>%
    group_by(!!quote_var1, Ability)
  
  
}

#test
group2_nhts(nhts_trips, nhts_persons, join_by = c("houseid", "personid"),worker,  by_var_qual = educ)
group1_nhts(nhts_trips, nhts_persons, join_by = c("houseid", "personid"), worker)