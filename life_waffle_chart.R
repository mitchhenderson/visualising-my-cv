library(tidyverse)
library(lubridate)
library(tmaptools)
library(prismatic)


# Create the data-----
life_data <-
  tibble(months = factor(rep(month.abb[1:12], 29), levels=month.abb[1:12]),
         age = rep(0:28, each = 12),
         era = "text")  %>%
  rowid_to_column("row_name") %>%
  mutate(era = fct_inorder(case_when(row_name < 287 ~ "School",
                                     row_name < 297 ~ "Physical Preparation Assistant",
                                     row_name < 338 ~ "Sports Performance Scientist",
                                     row_name <= 343 ~ "Athletic Performance Coach"))) %>%
  mutate(ed = fct_inorder(case_when(row_name < 62 ~ "Nothing",
                                    row_name < 228 ~ "School",
                                    row_name < 255 ~ "Nothing",
                                    row_name < 301 ~ "B Sport & Exercise Science",
                                    row_name < 313 ~ "B Sport & Exercise Science (Hons)",
                                    row_name <= 343 ~ "PhD Sport & Exercise Science"))) %>%
  filter(row_name > 5 & row_name < 344)


#plot on job experience

ggplot(life_data) +
  geom_point(aes(x = months, y = age, color = era), size = 5, shape = 15, show.legend = FALSE) +
  scale_color_manual(values = c("#B0B0B0", "#f68b1f" , "#171674", "#a2ff01")) +
  theme_void() +
  ggsave(filename = "job_plot.png", height = 15, width = 6, units = "cm") 


#plot on education

ggplot(life_data) +
  geom_point(aes(x = months, y = age, color = ed), size = 5, shape = 15, show.legend = FALSE) +
  scale_color_manual(values = c("#B0B0B0","#4DD68B", "#0f4beb", "#ff2305", "#000000"))+
  theme_void() + 
  ggsave(filename = "ed_plot.png", height = 15, width = 6, units = "cm") 

# post processing and design will be done in inkscape