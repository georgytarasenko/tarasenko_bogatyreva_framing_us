# Load required libraries
library(srvyr)
library(afex)
library(ggstatsplot)
library(tidyverse)

# Load data
tap <- readRDS("2024-110_client.rds")

# Assign treatment groups
tap <- tap %>%
  mutate(
    treatment = case_when(
      q86_text == unique(q86_text)[1] ~ "Positive",
      q86_text == unique(q86_text)[2] ~ "Negative",
      q86_text == unique(q86_text)[3] ~ "Neutral",
      TRUE ~ NA_character_
    ),
    treatment1 = case_when(
      q86_text %in% unique(q86_text)[1:2] ~ "Neutral",
      q86_text == unique(q86_text)[3] ~ "Non-Neutral",
      TRUE ~ NA_character_
    )
  )

# Rename and filter columns
tap <- tap %>%
  rename(
    att_t = q86,
    rep_dem = vs_pidlean,
    state = vs_state
  ) %>%
  filter(
    !is.na(treatment),
    !is.na(att_t),
    !is.na(state),
    !is.na(rep_dem)
  )

# Load and process presidential election data
pres20 <- read.csv('president_county_candidate.csv')

pres20_agg <- pres20 %>%
  group_by(state, candidate) %>%
  summarize(cand_vote = sum(total_votes), .groups = "drop") %>%
  pivot_wider(names_from = candidate, values_from = cand_vote, values_fill = 0) %>%
  rename(rep = `Donald Trump`, dem = `Joe Biden`) %>%
  mutate(all_votes = rep + dem)

# Map state names to abbreviations
state_mapping <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
  "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
  "District of Columbia" = "DC", "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI",
  "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS",
  "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
  "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
  "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV",
  "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
  "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK",
  "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC",
  "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
  "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV",
  "Wisconsin" = "WI", "Wyoming" = "WY"
)

pres20_agg <- pres20_agg %>%
  mutate(state = state_mapping[state])

# Add voting metrics
pres20_agg <- pres20_agg %>%
  mutate(
    rep_share = rep / all_votes,
    dem_share = dem / all_votes,
    share_diff = abs(rep_share - dem_share),
    dem_won = as.numeric(dem_share > rep_share),
    rep_won = as.numeric(dem_share < rep_share)
  )

# Merge anti-trans law data (n anti-trans laws by state)
state_values <- tibble(
  state = c("OK", "MO", "SC", "IA", "TN", "WV", "MS", "MN", "ID", "NH", 
            "KS", "WI", "KY", "FL", "GA", "OH", "AZ", "HI", "UT", "VA", "WY", 
            "NJ", "MI", "NC", "PA", "IL", "AK", "AL", "IN", "WA", "NE", "NY", 
            "RI", "LA", "MD", "OR", "DE", "NM", "SD", "CA", "CO", "MA", "VT"),
  anti_t_laws = c(60, 47, 37, 35, 35, 35, 26, 20, 19, 18, 17, 17, 15, 14, 14, 14, 12, 
                  11, 11, 11, 11, 10, 9, 9, 9, 8, 7, 6, 6, 6, 5, 5, 5, 4, 3, 3, 2, 2, 
                  2, 1, 1, 1, 1)
)

pres20_agg <- pres20_agg %>%
  left_join(state_values, by = "state") %>%
  mutate(anti_t_laws = replace_na(anti_t_laws, 0))

#split states by quartiles of n anti-tans laws
pres20_agg <- pres20_agg %>% 
  mutate(anti_t_laws_q = as.numeric(findInterval(anti_t_laws, unique(quantile(anti_t_laws, seq(0, 1, 0.26)),
                                    rightmost.closed = TRUE))-1))

pres20_agg <- pres20_agg %>% 
  mutate(share_diff_q = as.numeric(findInterval(share_diff, unique(quantile(share_diff, seq(0, 1, 0.26)),
                                    rightmost.closed = TRUE))-1))

# Merge with tap and add calculated variables
tap <- tap %>%
  left_join(pres20_agg, by = "state") %>%
  mutate(
    pos_fr = as.numeric(treatment == "Positive"),
    neg_fr = as.numeric(treatment == "Negative"),
    fr = as.numeric(treatment1 == "Non-Neutral"),
    att_t_ex = case_when(
      att_t %in% c("Strongly Oppose", "Strongly Support") ~ 3,
      att_t %in% c("Oppose", "Support") ~ 2,
      att_t == "Neutral" ~ 1,
      TRUE ~ 1
    ),
    p_minority = case_when(
      (rep_dem %in% c("Dem/lean Dem") & rep_won == 1) | 
        (rep_dem %in% c("Rep/lean Rep") & dem_won == 1) ~ 1,
      rep_dem == "Ind" ~ 0.5,
      TRUE ~ 0
    ),
    ideology_ex = case_when(
      vs_ideology %in% c("Very liberal", "Very conservative") ~ 3,
      vs_ideology %in% c("Liberal", "Conservative") ~ 2,
      vs_ideology %in% c("Somewhat liberal", "Somewhat conservative") ~ 1,
      vs_ideology == "Moderate or middle of the road" ~ 0,
      TRUE ~ NA_real_
    ),
    minority_scale = case_when(
      vs_ideology %in% c("Somewhat conservative", "Very conservative", "Conservative") & rep_won == 0 ~ ideology_ex + share_diff_q,
      vs_ideology %in% c("Somewhat liberal", "Very liberal", "Liberal") & dem_won == 0 ~ ideology_ex + share_diff_q,
      vs_ideology == "Moderate or middle of the road" ~ share_diff_q,
      TRUE ~ 0
    ),
    min_maj_scale = case_when(
      vs_ideology %in% c("Somewhat conservative", "Very conservative", "Conservative") & rep_won == 0 ~ ideology_ex + share_diff_q,
      vs_ideology %in% c("Somewhat conservative", "Very conservative", "Conservative") & rep_won == 1 ~ -(ideology_ex + share_diff_q),
      vs_ideology %in% c("Somewhat liberal", "Very liberal", "Liberal") & dem_won == 0 ~ ideology_ex + share_diff_q,
      vs_ideology %in% c("Somewhat liberal", "Very liberal", "Liberal") & dem_won == 1 ~ -(ideology_ex + share_diff_q),
      vs_ideology == "Moderate or middle of the road" ~ share_diff_q,
      TRUE ~ 0
    )
  )

#rescalking from 0 to 1 for easier interpretation
library(scales)
tap <- tap %>%
  mutate(
    min_maj_scale = rescale(min_maj_scale),
    ideology_ex = rescale(ideology_ex),
    anti_t_laws_q = rescale(anti_t_laws_q)
  )

#creating quadratic term (add 1, since values less then 1, when squared will be smaller then linear ones)
tap <- tap %>%
  mutate(
    min_maj_scale_1 = min_maj_scale+1,
    min_maj_scale_sq = (min_maj_scale+1)^2
  )

### VISUAL INSPECTION
##### WHOLE SAMPLE

ggplot(subset(tap), aes(x = as.numeric(min_maj_scale), y = as.numeric(att_t_ex), color = as.factor(fr)))+
    geom_point()+
    theme_minimal()+
    geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) + 
xlab('Majority-Minority Scale') + ylab('Extremity of Attitude Towards Transgender Peopel')+
labs(colour="Treated")

##### BY PARTISANSHIP

ggplot(subset(tap), aes(x = min_maj_scale, y = as.numeric(att_t_ex), color = as.factor(fr))) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x + I(x^2), se = F) + facet_wrap(~rep_dem)+ 
xlab('Majority-Minority Scale') + ylab('Extremity of Attitude Towards Transgender Peopel')+
labs(colour="Treated")

#No Independent
ggplot(subset(tap, as.numeric(rep_dem) !='Ind'), aes(x = min_maj_scale, y = as.numeric(att_t_ex), color = as.factor(fr)))+
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x + I(x^2),) 


# I DO Frequentist stuff here cuz it is faster to check

# random interecept for states
fit_clmm1 <- clmm(
  as.factor(att_t_ex) ~ min_maj_scale_1 * fr + min_maj_scale_sq * fr + (1 | state),
  data = subset(tap),
  link = "probit"
)

# random interecept for states
# random slope for Framing (fr) depending on anti_t_laws_q -- number of anti-trans laws (quartile)
# since we hypothesized that prior exposure to anti-tans rhethoric can impact our framinf effect

fit_clmm2 <- clmm(
  as.factor(att_t_ex) ~ min_maj_scale_1 * fr + min_maj_scale_sq * fr + (1 | state) + (1 + fr| anti_t_laws_q),
  data = subset(tap),
  link = "probit"
)

library(modelsummary)
modelsummary(list(fit_clmm1, fit_clmm2), 
  estimate = "{estimate}{stars}",
  stars = c('*' = .1, '**' = .05, '***' = 0.01))



### Bayesian Temaplate


fit1 <- brm(
  as.numeric(att_t_ex) ~ 1 + min_maj_scale_1*fr + min_maj_scale_sq*fr,
  family = cumulative(link = "probit"),
  data = subset(tap),
  seed = 1234,
  warmup = 1000, 
  iter   = 4000, 
  chains = 3, 
  cores  = 6
  #control = list(adapt_delta = 0.95)
)



