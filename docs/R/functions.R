


###________________________________________________________

##     Function 1 - data clean - qualitative data

# This function joins and cleans two data sets into a small tibble that
# is grouped by Ability and a variable input. It reports the survey count
# the population count (weighted counts) and the grouped distribution
# It ALSO FILTERS AGE!!!
clean_nhts <- function(data1, data2, join_by, by_var){
  
  quote_var <- enquo(by_var)
  
  
  data1 %>% left_join(data2, by = join_by) %>%
    mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                               medcond6 == "02" | medcond6 == "03" ~ "Disabled",
                               medcond == "02" ~ "Abled"))  %>%
    filter(r_age >= 18 & r_age < 65) %>%              # filtered by ages 18-64
    group_by(Ability, !!quote_var) %>% 
    filter(Ability != "NA",
           !!quote_var > 0) %>%
    summarise(Survey = n(),
              Population = sum(wtperfin)) %>%
    as_factor() %>%
    mutate(Distribution = Population/sum(Population))
  
} 


###__________________________________________________________


##     Function 2 - Data clean UT - qualitative data

# same function as function 1, however, it also groups by the utah population
# a challenge is that the UT variable is only available in the nhts_households
# file. Note that there are not enough utah wheelchair users to make data
# valid in my opinion.
clean_nhts_ut <- function(data1, data2, join_by, by_var){
  
  quote_var <- enquo(by_var)

  
  data1 %>% left_join(data2, by = join_by) %>%
    mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                               medcond6 == "02" | medcond6 == "03" ~ "Disabled",
                               medcond == "02" ~ "Abled"),
           UT = ifelse(hhstfips == "49", "Utah", "U.S."))  %>%          ## I could also use the hhstate variable, its exactly the same
    filter(r_age >= 18 & r_age < 65) %>%                          ## filtered by ages 18-64
    group_by(UT, Ability, !!quote_var) %>% 
    filter(Ability != "NA",
           !!quote_var > 0) %>%
    summarise(Survey = n(),
              Population = sum(wtperfin)) %>%
    as_factor() %>%
    mutate(Distribution = Population/sum(Population))

} 



###___________________________________________________________

##  Function 3 - Bar Plot in Plotly - qualitative data


# Write function to form plotly barchart (qualitative data)
# if fed utah specific data, it will graph utah data...
plotly_bar_nhts <- function(data, x_var, y_var, title, subtitle = NULL, xlab = NULL, ylab = NULL){
  
  quote_varx <- enquo(x_var)
  quote_vary <- enquo(y_var)
  
  plot <- ggplot(data, aes(x = !!quote_varx, y = !!quote_vary)) + 
    geom_col(aes(fill = Ability), colour = "Black", position = "dodge") +
    ggtitle(title, subtitle) +
    labs(x = xlab, y = ylab) +
    scale_fill_brewer(palette = "PuBuGn", direction = 2) + 
    theme(axis.text.x = element_text(size  = 10, 
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1))
    #scale_x_discrete(labels = abbreviate)
    
  
  plot %>% ggplotly(tooltip = c("x", "y", "fill"))
  
}


###______________________________________________________________


##   Function 4 - Bar Plot in plotly UT - Qualitative data

# This function works the same as function 3 but adds a facet_wrap at the end to compare
# to the utah data set.
plotly_bar_nhts_ut <- function(data, x_var, y_var, title, subtitle = NULL, xlab = NULL, ylab = NULL){
  
  quote_varx <- enquo(x_var)
  quote_vary <- enquo(y_var)
  
  plot1 <- ggplot(data, aes(x = !!quote_varx, y = !!quote_vary)) + 
    geom_col(aes(fill = Ability), colour = "Black", position = "dodge") +
    ggtitle(title, subtitle) +
    labs(x = xlab, y = ylab) +
    scale_fill_brewer(palette = "PuBuGn", direction = 2) + 
    theme(axis.text.x = element_text(size  = 10, 
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1)) +
    facet_wrap(~UT)
  #scale_x_discrete(labels = abbreviate)
  
  
  plot1 %>% ggplotly(tooltip = c("x", "y", "fill"))
  
}



###___________________________________________________________

##  Function 5 - Data summary - Quantitative data sets (ability, var_quant, var_qual)


# this function is used to summarize the quantitative data sets (trip miles, age,
# trip time, etc) The function takes one quantitative variable and groups it by
# another qualitative varaibel i.e. trip distance by ability and gender


summary_nhts <- function(data1, data2, join_by, by_var_quant, by_var_qual, title = NULL){
  
  quote_var <- enquo(by_var_quant)
  quote_var1 <- enquo(by_var_qual)
  
  data1 %>% left_join(data2, by = join_by) %>%
    mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                               medcond6 == "02" | medcond6 == "03" ~ "Disabled",  
                               medcond == "02" ~ "Abled")) %>% 
    filter(r_age >= 18 & r_age < 65,
           !!quote_var > 0) %>%
    group_by(!!quote_var1, Ability) %>% 
    filter(Ability != "NA",
           !!quote_var1 > 0) %>%
    summarise(median = median(!!quote_var),
              mean = weighted.mean(!!quote_var, wtperfin),
              sd = sqrt(wtd.var(!!quote_var, wtperfin)),
              max = max(!!quote_var)) %>% 
    pander(caption = title)
  
  
}





###___________________________________________________________

##    Function 6 - Boxplot in plotly - Quantitative data


# Write function to form plotly boxplot (quantitative data)

# data example. erase when done
nhts_trips %>% left_join(nhts_persons, by = c("houseid", "personid")) %>%
  mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                             medcond6 == "02" | medcond6 == "03" ~ "Disabled",  
                             medcond == "02" ~ "Abled")) %>% 
  group_by(Ability,)


plotly_box_nhts <- function(){
  
  
}











###################################################################################


###################################################################################

###___________________________________________________________


# I stopped working on this for now...
# Write practice function to join and clean data using multiple variables
practice_clean_nhts <- function(data1, data2, join_by, by_var, by_var2, by_var3){
  
  quote_var <- enquo(by_var)
  quote_var2 <- enquo(by_var2)
  quote_var3 <- enquo(by_var3)
  
  data1 %>% left_join(data2, by = join_by) %>%
    mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                               medcond6 == "02" | medcond6 == "03" ~ "Disabled",
                               medcond == "02" ~ "Abled"),
           mutate(quote_var2 = ifelse(is_empty(by_var2)==TRUE,NA,quote_var2),
                  quote_var3 = ifelse(is_empty(by_var3)==TRUE,NA,quote_var3))) %>%           # this is the experiment line
    group_by(Ability, !!quote_var, !!quote_var2, !!quote_var3) %>% 
    filter(Ability != "NA",
           !!quote_var > 0,
           !!quote_var2 > 0,
           !!quote_var3 > 0) %>%
    summarise(Survey = n(),
              Population = sum(wtperfin)) %>%
    mutate(Distribution = Population/sum(Population),
           x_var = as.factor(!!quote_var),
           x_var2 = as.factor(!!quote_var2),
           x_var3 = as.factor(!!quote_var3))
  
} 


