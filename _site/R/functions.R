# I want to write a histogram function
# I want to write a boxplot function too.



###________________________________________________________


# This  practice function creates a barchart

practice_barchart <- function(by_var, data1, data2, join_by, title, subtitle = NULL){
  
  quote_var <- enquo(by_var)
  
  data1 %>% left_join(data2, by = join_by) %>%
    mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                               medcond6 == "02" | medcond6 == "03" ~ "Disabled",
                               medcond == "02" ~ "Abled")) %>%
    group_by(Ability, !!quote_var) %>% 
    filter(Ability != "NA",
           !!quote_var > 0) %>%
    summarise(Survey = n(),
              Population = sum(wtperfin)) %>%
    mutate(Distribution = Population/sum(Population),
           x_var = as.factor(!!quote_var)) %>%
  ggplot(aes(x = x_var, y = Distribution)) + 
    geom_col(aes(fill = Ability), position = "dodge") +
    ggtitle(title, subtitle = subtitle)

}


###__________________________________________________________


# Write function to join and clean data
clean_nhts <- function(data1, data2, join_by, by_var){
  
  quote_var <- enquo(by_var)

  
  data1 %>% left_join(data2, by = join_by) %>%
    mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                               medcond6 == "02" | medcond6 == "03" ~ "Disabled",
                               medcond == "02" ~ "Abled"))  %>%          
    group_by(Ability, !!quote_var) %>% 
    filter(Ability != "NA",
           !!quote_var > 0) %>%
    summarise(Survey = n(),
              Population = sum(wtperfin)) %>%
    as_factor() %>%
    mutate(Distribution = Population/sum(Population))

} 

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

###___________________________________________________________


# Write function to form plotly barchart
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
    #facet_wrap(~Ability)
  
  plot %>% ggplotly(tooltip = c("x", "y", "fill"))
  
}




###___________________________________________________________


# Write function to form plotly boxplot







