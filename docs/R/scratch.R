library(tidyverse)
library(knitr)
library(haven)
library(nhts2017)


new <- nhts_households  %>%
  filter(hhfaminc %in% c("03","07","09"))%>%
  mutate(Vehicles = ifelse(hhvehcnt > 3, 4, hhvehcnt),
         Income = case_when(hhfaminc == "03" ~ "Low",
                            hhfaminc == "07" ~ "Mid",
                            hhfaminc == "09" ~ "High"),
         Income = as_factor(Income)) %>%
  ggplot(aes(x = Income, y = Vehicles, weight = wthhfin, fill = Income))+
  geom_violin(trim = FALSE)+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name = "Income groups")
