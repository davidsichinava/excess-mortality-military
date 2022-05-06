library(tidyverse)
library(mgcv)
library(lubridate)

# Russia


# Mortality data: https://github.com/akarlinsky/world_mortality

weekly_data <- read_csv("https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv")%>%
  filter(country_name == "Russia")

train <- weekly_data[weekly_data$year < 2020, ] %>%
  mutate(period = as.numeric(row.names(.)))

test <- weekly_data[weekly_data$year >= 2020, ] %>%
  mutate(period = as.numeric(row.names(.)))

# Adjust for seasonality as in Weinberger et al, 2020 and predict potential number of deaths
# https://github.com/weinbergerlab/excess_pi_covid
# https://github.com/weinbergerlab/excess_pi_covid/blob/master/us_pi_excess.Rmd

## Predict deaths using nonparametric GAM model (poisson)

pred_ed_mod <- gam(deaths~s(time, bs='cc')+year, data=train, family='poisson')

pred_ed <- predict(pred_ed_mod, type='response', newdata=test) %>%
  data.frame()%>%
  setNames("deaths_pred") %>%
  mutate(
         time=as.numeric(c(1:12, 1:12, 1:3)),
         year=c(rep(2020, 12), rep(2021, 12), rep(2022, 3)),
         #time = as.numeric(c(1:3)),
         #year = c(rep(2022, 3)),
         deaths_pred=round(deaths_pred, 0))

weekly_data %>%
  filter(year>=2015)%>%
  # mutate(weeks=paste0(year, stringi::stri_pad_left(time, 2, 0)))%>%
  
  left_join(pred_ed, by=c("year", "time"))%>%
  mutate(period=as.numeric(row.names(.)),
         dif = deaths-deaths_pred,
         dif_color = ifelse(dif>=0, 1, 2),
         week_dates=as.Date(paste0(time, "/1/", year), "%m/%d/%Y"),
         which_is_more = ifelse(deaths>deaths_pred, deaths, deaths_pred),
         location = which_is_more-abs(dif/2),
         dif_color = factor(dif_color, levels=c(1, 2), labels=c("Predicted", "Empirical")),
  )%>%
  select(week_dates, deaths_pred, deaths, dif_color, year) %>%
  pivot_longer(-c("week_dates", "dif_color", "year"), names_to = "variable", values_to = "counts") %>%
  mutate(
    variable = factor(variable, levels=c("deaths_pred", "deaths"), labels=c("Predicted", "Empirical")),
  ) %>%
  filter(variable == "Empirical") %>%
  ggplot(aes(week_dates, counts, fill = variable, color = factor(year)))+
  # geom_area(alpha = 0.5)+
  # geom_point()+
  geom_line(aes(group = factor(year)))+
  # scale_fill_manual(values = c("#e07a5f", "#81b29a"))+
  scale_x_date(date_labels = "%m/%y")+
  scale_y_continuous(labels = scales::comma, limits = c(0, 300000))+
  theme_bw()+
  labs(
    title = "Excess mortality, January 2020 - March 2022",
    y = "Deaths",
    x = "Months")+
  theme(
    text = element_text(family= "FiraGO"),
    legend.position = "none",
    axis.title.x = element_blank()
  )


weekly_data %>%
  filter(year>=2015)%>%
  left_join(pred_ed, by=c("year", "time"))%>%
  mutate(period=as.numeric(row.names(.)),
         dif = deaths-deaths_pred,
         dif_color = ifelse(dif>=0, 1, 2),
         week_dates=as.Date(paste0(time, "/1/", year), "%m/%d/%Y"),
         which_is_more = ifelse(deaths>deaths_pred, deaths, deaths_pred),
         location = which_is_more-abs(dif/2),
         # week_dates=as.Date("2020-01-01")+months(time),
         dif_color = factor(dif_color, levels=c(1, 2), labels=c("Predicted", "Empirical")),
  )%>%
  filter(year %in% c(2021, 2022) & time %in% c(2, 3)) %>%
  group_by(year)%>%
  summarize(
    deaths = sum(deaths),
    deaths_pred = sum(deaths_pred),
  ) %>%
  summarize(
    excess_deaths = deaths-deaths_pred,
  )

#### Factor in covid

train <- weekly_data[weekly_data$year <= 2021, ] %>%
  mutate(period = as.numeric(row.names(.)))

test <- weekly_data[weekly_data$year >= 2022, ] %>%
  mutate(period = as.numeric(row.names(.)))

# Adjust for seasonality as in Weinberger et al, 2020 and predict potential number of deaths
# https://github.com/weinbergerlab/excess_pi_covid
# https://github.com/weinbergerlab/excess_pi_covid/blob/master/us_pi_excess.Rmd

## Predict deaths using nonparametric GAM model (poisson)

pred_ed_mod <- gam(deaths~s(time, bs='cc')+year, data=train, family='poisson')

pred_ed <- predict(pred_ed_mod, type='response', newdata=test) %>%
  data.frame()%>%
  setNames("deaths_pred") %>%
  mutate(time=as.numeric(c(1:3)),
         year=c(rep(2022, 3)),
         deaths_pred=round(deaths_pred, 0))

weekly_data %>%
  filter(year>=2020)%>%
  # mutate(weeks=paste0(year, stringi::stri_pad_left(time, 2, 0)))%>%
  
  left_join(pred_ed, by=c("year", "time"))%>%
  mutate(period=as.numeric(row.names(.)),
         dif = deaths-deaths_pred,
         dif_color = ifelse(dif>=0, 1, 2),
         week_dates=as.Date(paste0(time, "/1/", year), "%m/%d/%Y"),
         which_is_more = ifelse(deaths>deaths_pred, deaths, deaths_pred),
         location = which_is_more-abs(dif/2),
         dif_color = factor(dif_color, levels=c(1, 2), labels=c("Predicted", "Empirical")),
  )%>%
  ggplot()+
  geom_area(aes(week_dates, deaths_pred, fill = "Predicted deaths"), alpha = 0.5)+
  geom_area(aes(week_dates, deaths, fill = "Actual deaths"), alpha = 0.5)+
  scale_fill_manual(values=c("red", "blue"))+
  geom_text(aes(week_dates, location, label=dif, family = "FiraGO"), nudge_x = 10, size=3)+
  scale_x_date(date_labels = "%m/%y")+
  scale_y_continuous(labels = scales::comma, limits = c(0, 300000))+
  theme_bw()+
  labs(
    title = "Excess mortality, January 2020 - March 2022",
    y = "Deaths",
    x = "Months")+
  theme(
    text = element_text(family= "FiraGO"),
    legend.position = "none",
    axis.title.x = element_blank()
  )


weekly_data %>%
  filter(year>=2020)%>%
  left_join(pred_ed, by=c("year", "time"))%>%
  mutate(period=as.numeric(row.names(.)),
         dif = deaths-deaths_pred,
         dif_color = ifelse(dif>=0, 1, 2),
         week_dates=as.Date(paste0(time, "/1/", year), "%m/%d/%Y"),
         which_is_more = ifelse(deaths>deaths_pred, deaths, deaths_pred),
         location = which_is_more-abs(dif/2),
         # week_dates=as.Date("2020-01-01")+months(time),
         dif_color = factor(dif_color, levels=c(1, 2), labels=c("Predicted", "Empirical")),
  )%>%
  filter(year %in% c(2021, 2022) & time %in% c(2, 3)) %>%
  group_by(year)%>%
  summarize(
    deaths = sum(deaths),
    deaths_pred = sum(deaths_pred),
  )


### regional data


fed_sub <- read_csv("https://raw.githubusercontent.com/dkobak/excess-mortality/main/russian-data/russia-monthly-deaths.csv") %>%
  rename(
    "subject" = "...1"
  ) %>%
  filter(!subject %in% c("Российская Федерация"))%>%
  pivot_longer(
    -subject, names_to = "months", values_to = "deaths"
  ) %>%
  separate(
    months, into = c("year", "time")
  ) %>%
  mutate(
    year = as.numeric(year),
    time = as.numeric(time),
    deaths = as.numeric(deaths),
  )

unique(fed_sub$subject) %>% data.frame() %>%setNames("subject") %>% mutate(id = factor(row.names(.))) -> names

### exploratory analysis

fed_sub %>%
  group_by(year) %>%
  filter (time %in% c(1, 2, 3))%>%
  summarize(
    deaths = sum(deaths, na.rm = T)
  ) %>%
  ggplot(
    aes(
      year, deaths, label = round(deaths/1000, 1)
    )
  )+
  geom_col()+
  scale_y_continuous(labels = scales::comma, limits = c(0, 700000))+
  geom_text(family = "FiraGO", nudge_y = 10000, size=4)+
  theme_bw()+
  labs(
    title = "Mortality in January-March, 1990-2022",
    y = "Deaths",
    x = "Months")+
  theme(
    text = element_text(family= "FiraGO"),
    legend.position = "none",
    axis.title.x = element_blank()
  )

fed_sub %>%
  filter (year %in% c(2015:2022))%>%
  group_by(year, time) %>%
  summarize(
    deaths = sum(deaths, na.rm = T)
  ) %>%
  mutate(
    week_dates=as.Date(paste0(time, "/1/", year), "%m/%d/%Y"),
  ) %>%
  ggplot(
    aes(
      week_dates, deaths, label = round(deaths/1000, 1), fill = "#81b29a"
    )
  )+
  geom_col()+
  scale_x_date(date_labels = "%m/%y", date_breaks = "6 months")+
  scale_fill_manual(values = "#81b29a")+
  scale_y_continuous(labels = scales::comma, limits = c(0, 300000))+
  annotate("rect", xmin = as.Date("2022-01-15"), xmax = as.Date("2022-03-15"),
           ymin = 0, ymax = 300000, alpha = .1,fill = "blue")+
  annotate("text", x = as.Date("2022-02-15"), #xmax = as.Date("2022-03-15"),
           y = 250000, angle = 90,
           fill = "blue", label = "February/March, 2022")+
  annotate("text", x = as.Date("2021-02-15"), #xmax = as.Date("2022-03-15"),
           y = 250000, angle = 90,
           fill = "blue", label = "February/March, 2021")+
  annotate("rect", xmin = as.Date("2021-01-15"), xmax = as.Date("2021-03-15"),
           ymin = 0, ymax = 300000, alpha = .1,fill = "blue")+
  theme_bw()+
  labs(
    title = "Monthly mortality, 2015-2022",
    y = "Deaths",
    x = "Months")+
  theme(
    text = element_text(family= "FiraGO"),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("montly_mortality.png", height = 9, width = 16)

fed_sub %>%
  filter (year %in% c(2021:2022) & time %in% c(2, 3))%>%
  group_by(year) %>%
  summarize(
    deaths = sum(deaths, na.rm = T)
  )

# 27708 extra deaths compared to the same time period in 2021

train <- fed_sub[fed_sub$year < 2022 & fed_sub$year >=2015, ] %>%
  left_join(names, by = "subject")%>%
  mutate(period = as.numeric(row.names(.)))

test <- fed_sub[fed_sub$year >= 2022, ] %>%
  left_join(names, by = "subject")%>%
  mutate(period = as.numeric(row.names(.)))

# Adjust for seasonality as in Weinberger et al, 2020 and predict potential number of deaths
# https://github.com/weinbergerlab/excess_pi_covid
# https://github.com/weinbergerlab/excess_pi_covid/blob/master/us_pi_excess.Rmd

## Predict deaths using nonparametric GAM model (poisson), federation subject level REs

pred_ed_mod <- mgcv::gam(deaths~s(time, bs='cc')+year+s(id, bs="re"), data=train, family='poisson')

fed_sub %>%
  left_join(names, by = "subject")%>%
  mutate(period = as.numeric(row.names(.))) %>%
  filter(year == 2022) -> actual_deaths

pred_ed <- predict(pred_ed_mod, type='response', newdata=test) %>%
  data.frame()%>%
  setNames("deaths_pred") %>%
  mutate(# time=as.numeric(c(1:12, 1:12, 1:3)),
         # year=c(rep(2020, 12), rep(2021, 12), rep(2022, 3)),
          subject = rep(1:85, each = 3),
          # subject = rep(unique(names), ),
          time = as.numeric(rep(c(1:3), 85)),
          year = c(rep(2022, 3*85)),
         deaths_pred=round(deaths_pred, 0),
         subject = as.factor(subject)
         )%>%
  left_join(actual_deaths, by = c("year", "time", "subject"="id")) %>%
  rename(
    "subject_name" = "subject.y"
  )

pred_ed %>%
  filter (time == 1) %>%
  summarize(
    deaths_pred = sum(deaths_pred),
    deaths = sum(deaths)
  ) # -10668, died fewer

pred_ed %>%
  filter (time == 2) %>%
  summarize(
    deaths_pred = sum(deaths_pred),
    deaths = sum(deaths)
  ) # +8482, died more

pred_ed %>%
  filter (time == 3) %>%
  summarize(
    deaths_pred = sum(deaths_pred),
    deaths = sum(deaths)
  ) # +15400, died more

### Overall, died 23,882 more than predicted while factoring in the COVID-19 situation


### Where?


excess_deaths <- pred_ed %>%
  group_by(subject_name, time) %>%
  summarize(
    excess_deaths = deaths-deaths_pred
  ) %>%
  mutate(
    excess_deaths = case_when(
      excess_deaths < 0 ~ NA_real_,
      T ~ as.numeric(excess_deaths)
    )
  ) %>%
  filter(time != 1) %>% ## Better to filter by month, or replace negatives with NA
  summarize(
    excess_deaths = sum(excess_deaths, na.rm=T)
  ) %>%
  setNames(c("fed_sub", "excess_deaths"))

write_csv(excess_deaths, "excess_deaths.csv")

pop_data <- readxl::read_excel("district_compatibility.xlsx")


pop_data %>%
  left_join(excess_deaths, by = "fed_sub") %>%
  select(fed_sub, excess_deaths, iso_names, cas_apr06_2022, pop) %>%
  mutate(
    per_100 = (excess_deaths/pop)*100000,
    per_100_bbc = (cas_apr06_2022/pop)*100000,
  ) -> population_merged

write_csv(population_merged, "population_merged.csv")


