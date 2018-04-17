library(tidyverse)
library(readxl)
library(janitor)
library(broom)
library(ggridges)

global_mortality <- read_xlsx("data/global_mortality.xlsx")

tidy_mortality <- global_mortality %>% 
  clean_names %>% 
  gather("cause", "percent_mortality", -country, -country_code, -year) %>% 
  # we don't need "percent" all the time
  mutate(cause = cause %>% str_replace("_percent", ""))

# how does mortality change over time? 
safe_mortality_models <- tidy_mortality %>% 
  group_by(country, country_code, cause) %>% 
  nest %>% 
  mutate(change_model = map(data, ~ safely(lm)(log(percent_mortality) ~ year, data = .)))

tidy_mortality_models <- safe_mortality_models %>% 
  mutate(model_result = map(change_model, "result"),
         model_output = map(model_result, tidy)) %>% 
  unnest(model_output)

tidy_mortality_models %>% 
  filter(term == "year") %>% 
  # how is that different among countries?
  group_by(cause) %>% 
  mutate(sd_cause = sd(estimate, na.rm = TRUE),
         med_cause = median(estimate, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(cause = fct_reorder2(cause, med_cause, sd_cause, .desc = TRUE)) %>% 
  ggplot(aes(x = estimate, y = cause, fill = sd_cause)) + 
  geom_density_ridges() + scale_fill_viridis_c() + geom_vline(xintercept = 0) +
  labs(x = "Change with time")
