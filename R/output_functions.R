## This functions sheet holds one data cleaning function. Then the following 
## function are output functions that first group the data by variables then output
## a summary or a plot.



# -----------------------------------------------------------------------------
# First the simple data cleaning function
# All this does is join the data frames, creates the ability column, and gives you the 
# ability to filter by age. Default is 18-65.
# Ideally you could then mutate another column if you wanted (UT) or group it by another...

nhts_clean <- function(data1, data2, join_by, age1 = 18, age2 = 65){
  
  
  
  data1 %>% left_join(data2, by = join_by) %>%
    mutate(Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                               medcond6 == "02" | medcond6 == "03" ~ "Disabled",  
                               medcond == "02" ~ "Abled")) %>% 
    filter(r_age >= age1 & r_age < age2,
           Ability != "NA")
  
}

#______________________________________________________________________
#______________________________________________________________________



## When input is using one variable in comparison
## the output is distribution summary (counts, population, and percentages)
## or a barplot from plotly


# Distribution summary (1 variable)
# This function will spread a table with variable in rows and Ability in columns 
# fill = Distribution, Population, Survey

nhts_distribution <- function(data, by_var, fill = Distribution){
  
  quote_var <- enquo(by_var)
  quote_fill <- enquo(fill)
  
  data %>% filter(!!quote_var >0) %>%
    group_by(Ability, !!quote_var) %>%
    summarise(Survey = n(),
              Population = sum(wtperfin)) %>%
    as_factor() %>%
    mutate(Distribution = Population/sum(Population)) %>%
    select(Ability, !!quote_var, !!quote_fill) %>%
    spread(Ability, !!quote_fill) %>%
    pander()
  
}



# Barplot from plotly (1 variable)
nhts_barplot <- function(data, x_var, title, subtitle = NULL, xlab = NULL, ylab = NULL){
  
  quote_varx <- enquo(x_var)
  #quote_vary <- enquo(y_var)
  
  table <- data %>% group_by(Ability, !!quote_varx ) %>%
    filter(!!quote_varx > 0) %>%
    summarise(Population = sum(wtperfin)) %>%
    as_factor() %>%
    mutate(Distribution = Population/sum(Population))
  
  plot <- ggplot(table, aes(x = !!quote_varx, y = Distribution)) + 
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


# Summary stats table (quantitative data - 1 varaible)
nhts_stat_summary <- function(data, by_var, title = NULL){
  
  quote_var <- enquo(by_var)
  
  data %>% 
    filter(!!quote_var > 0) %>%
    group_by(Ability) %>%
    summarise(median = median(!!quote_var),
              mean = weighted.mean(!!quote_var, wtperfin),
              sd = sqrt(wtd.var(!!quote_var, wtperfin)),
              max = max(!!quote_var)) %>% 
    pander(caption = title)
  
  
}


# Single variable boxplot (quantitative)

nhts_boxplot <- function(data, by_var, title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL){
  
  quote_var <- enquo(by_var)
  
  table <- data %>%
    group_by(Ability) %>%
    filter(!!quote_var > 0) %>%
    as_factor() %>%
    mutate(quantitative = as.double(!!quote_var))
  
  plot <- ggplot(table, aes(x = Ability, y = quantitative, fill = Ability)) +
    geom_boxplot(position = position_dodge(1)) +
    ggtitle(title, subtitle) +
    labs(x = xlab, y = ylab) +
    scale_fill_brewer(palette = "PuBuGn", direction = 2) + 
    theme(axis.text.x = element_text(size  = 10, 
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1)) 
  
  plot %>% ggplotly()
  
}


#______________________________________________________________________
#______________________________________________________________________


## When input is two variables (either quant or qual)
## the output is stats summary table (quant), boxplot comparison (quant)
## or wrapped barplot


# Stats Summary Table (quantitative data) - 2 variables (1 quantitative)

nhts2_stat_summary <- function(data, by_var, by_var_qual, title = NULL){
  
  quote_var <- enquo(by_var)
  quote_var_qual <- enquo(by_var_qual)
  
  data %>% 
    filter(!!quote_var > 0,
           !!quote_var_qual >0) %>%
    group_by(!!quote_var_qual, Ability) %>%
    summarise(median = median(!!quote_var),
              mean = weighted.mean(!!quote_var, wtperfin),
              sd = sqrt(wtd.var(!!quote_var, wtperfin)),
              max = max(!!quote_var)) %>% 
    pander(caption = title)
  
  
}



# Boxplot comparison (quantitative data)

nhts2_boxplot <- function(data, by_var, by_var_qual, title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL){
  
  quote_var <- enquo(by_var)
  quote_var_qual <- enquo(by_var_qual)
  
 table <- data %>%
    group_by(Ability) %>%
    filter(!!quote_var > 0,
           !!quote_var_qual > 0) %>%
    as_factor() %>%
    mutate(quantitative = as.double(!!quote_var))
  
  plot <- ggplot(table, aes(x = !!quote_var_qual, y = quantitative, fill = Ability)) +
    geom_boxplot(position = position_dodge(1)) +
    ggtitle(title, subtitle) +
    labs(x = xlab, y = ylab) +
    scale_fill_brewer(palette = "PuBuGn", direction = 2) + 
    theme(axis.text.x = element_text(size  = 10, 
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1))
  
  return(plot)
  
  #plot %>% ggplotly(tooltip = c("x", "y", "fill"))
  
}



# Barplot from plotly (qualitative data) 

nhts_barplot2 <- function(data, by_var, facet_var, title, subtitle = NULL, xlab = NULL, ylab = NULL){ 
  
  quote_var <- enquo(by_var)
  facet_quote <- enquo(facet_var)
  facet_quote1 <- enexpr(facet_quote)
  
  table <- data %>% group_by(!!facet_quote, Ability, !!quote_var) %>% 
    filter(!!facet_quote > 0,
           !!quote_var > 0) %>%
    summarise(Population = sum(wtperfin)) %>%
    as_factor() %>%
    mutate(Distribution = Population/sum(Population))
  
  plot <- ggplot(table, aes(x = !!quote_var, y = Distribution)) + 
    geom_col(aes(fill = Ability), colour = "Black", position = "dodge") +
    ggtitle(title, subtitle) +
    labs(x = xlab, y = ylab) +
    scale_fill_brewer(palette = "PuBuGn", direction = 2) + 
    theme(axis.text.x = element_text(size  = 10, 
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1)) + 
    facet_wrap(facet_quote)                        
  #scale_x_discrete(labels = abbreviate)
  
  #return(plot)
  plot %>% ggplotly(tooltip = c("x", "y", "fill"))
  
}  


