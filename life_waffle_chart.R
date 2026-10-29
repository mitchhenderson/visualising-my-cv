library(tidyverse)

# =========================================================================
# EDIT THESE SECTIONS
# =========================================================================

timeline_start <- ymd("1992-06-01")

# Number of future months to include in the visual.
# 0 = current month only
# 1 = include next month
# 2 = include the next two months
months_ahead <- 1

timeline_end <- floor_date(today(), "month") %m+%
  months(months_ahead)


career_history <- tribble(
  ~start_date  , ~role                                             ,
  "1992-06-01" , "School"                                          ,
  "2015-11-01" , "Physical preparation assistant"                  ,
  "2016-09-01" , "Sports scientist and athletic performance coach" ,
  "2022-05-01" , "Sports scientist"                                ,
  "2022-12-01" , "Senior business insights analyst"                ,
  "2023-10-01" , "Senior data scientist"
) |>
  mutate(start_date = ymd(start_date))


education_history <- tribble(
  ~start_date  , ~education                          ,
  "1992-06-01" , "Nothing"                           ,
  "1997-02-01" , "School"                            ,
  "2010-12-01" , "Nothing"                           ,
  "2013-02-01" , "B Sport & Exercise Science"        ,
  "2016-12-01" , "Nothing"                           ,
  "2017-02-01" , "B Sport & Exercise Science (Hons)" ,
  "2017-12-01" , "Nothing"                           ,
  "2018-02-01" , "PhD Sport & Exercise Science"      ,
  "2023-08-01" , "Nothing"
) |>
  mutate(start_date = ymd(start_date))


job_colors <- c(
  "School" = "#B0B0B0",
  "Physical preparation assistant" = "#f68b1f",
  "Sports scientist and athletic performance coach" = "#a2ff01",
  "Sports scientist" = "#8fc5eb",
  "Senior business insights analyst" = "#ad7dc7",
  "Senior data scientist" = "#d10d35"
)


education_colors <- c(
  "Nothing" = "#B0B0B0",
  "School" = "#4DD68B",
  "B Sport & Exercise Science" = "#0f4beb",
  "B Sport & Exercise Science (Hons)" = "#ff2305",
  "PhD Sport & Exercise Science" = "#000000"
)


output_dir <- "outputs"


# =========================================================================
# EVERYTHING BELOW HERE SHOULD RARELY NEED EDITING
# =========================================================================

build_timeline <- function(
  start_date,
  end_date,
  career_history,
  education_history
) {
  tibble(
    date = seq(start_date, end_date, by = "month")
  ) |>
    mutate(
      month = factor(
        month(date),
        levels = 1:12,
        labels = month.abb
      ),
      life_year = year(date) - year(start_date)
    ) |>
    left_join(
      career_history,
      join_by(closest(date >= start_date))
    ) |>
    left_join(
      education_history,
      join_by(closest(date >= start_date)),
      suffix = c("_career", "_education")
    )
}


make_timeline_plot <- function(
  data,
  variable,
  colors
) {
  ggplot(
    data,
    aes(
      x = month,
      y = life_year,
      color = {{ variable }}
    )
  ) +
    geom_point(
      size = 5,
      shape = 15,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = colors,
      drop = FALSE
    ) +
    theme_void()
}


save_timeline_plot <- function(
  plot,
  filename,
  output_dir = "."
) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    height = 16.6,
    width = 6,
    units = "cm"
  )
}


life_data <- build_timeline(
  start_date = timeline_start,
  end_date = timeline_end,
  career_history = career_history,
  education_history = education_history
)


job_plot <- life_data |>
  mutate(
    role = factor(
      role,
      levels = names(job_colors)
    )
  ) |>
  make_timeline_plot(
    variable = role,
    colors = job_colors
  )


education_plot <- life_data |>
  mutate(
    education = factor(
      education,
      levels = names(education_colors)
    )
  ) |>
  make_timeline_plot(
    variable = education,
    colors = education_colors
  )


job_plot
education_plot


save_timeline_plot(
  plot = job_plot,
  filename = paste0(today(), "_job_plot.png"),
  output_dir = output_dir
)


save_timeline_plot(
  plot = education_plot,
  filename = paste0(today(), "_education_plot.png"),
  output_dir = output_dir
)
