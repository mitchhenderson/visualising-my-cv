library(tidyverse)
library(lubridate)
library(tmaptools)
library(prismatic)


# Create the data-----
life_data <-
  tibble(
    months = factor(rep(month.abb[1:12], 35), levels = month.abb[1:12]),
    age = rep(0:34, each = 12),
    era = "text"
  ) %>%
  rowid_to_column("row_name") %>%
  mutate(
    era = fct_inorder(case_when(
      row_name < 287 ~ "School",
      row_name < 297 ~ "Physical preparation assistant",
      row_name <= 364 ~ "Sports scientist and athletic performance coach",
      row_name <= 371 ~ "Sports scientist",
      row_name <= 381 ~ "Senior business insights analyst",
      .default = "Senior data scientist"
    )),
    ed = fct_inorder(case_when(
      row_name < 62 ~ "Nothing",
      row_name < 228 ~ "School",
      row_name < 254 ~ "Nothing",
      row_name < 300 ~ "B Sport & Exercise Science",
      row_name < 302 ~ "Nothing",
      row_name < 312 ~ "B Sport & Exercise Science (Hons)",
      row_name < 314 ~ "Nothing",
      row_name <= 379 ~ "PhD Sport & Exercise Science",
      .default = "Nothing"
    ))
  ) %>%
  filter(row_name > 5 & row_name <= 406) # UPDATE


#plot on job experience

ggplot(life_data) +
  geom_point(
    aes(x = months, y = age, color = era),
    size = 5,
    shape = 15,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("#B0B0B0", "#f68b1f", "#a2ff01", "#8fc5eb", "#ad7dc7", "#d10d35")
  ) +
  theme_void()


ggsave(
  filename = paste0(today(), "_new_job_plot.png"),
  height = 16.6,
  width = 6,
  units = "cm"
)


#plot on education

ggplot(life_data) +
  geom_point(
    aes(x = months, y = age, color = ed),
    size = 5,
    shape = 15,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("#B0B0B0", "#4DD68B", "#0f4beb", "#ff2305", "#000000")
  ) +
  theme_void()

ggsave(
  filename = paste0(today(), "_new_ed_plot.png"),
  height = 16.6,
  width = 6,
  units = "cm"
)

# post processing and design will be done in inkscape
