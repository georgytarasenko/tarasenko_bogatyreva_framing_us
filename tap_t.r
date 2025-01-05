# Load required libraries
library(srvyr)
library(afex)
library(ggstatsplot)
library(tidyverse)

# Load data
tap <- readRDS("/Users/georgetarasenko/Dropbox/us_framing_project/rep_test/2024-110_client.rds")

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
      q86_text %in% unique(q86_text)[1:2] ~ "Non-Neutral", #Гош тут у тебя было Neutral, хотя мне кажется ты хотел Non-Neutral
      q86_text == unique(q86_text)[3] ~ "Neutral", #А тут Neutral
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
pres20 <- read.csv('/Users/georgetarasenko/Dropbox/us_framing_project/rep_test/president_county_candidate.csv')

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
  mutate(anti_t_laws_q = as.numeric(findInterval(anti_t_laws, unique(quantile(anti_t_laws, seq(0, 1, 0.26)), #Тут не 0,25 должно быть?
                                    rightmost.closed = TRUE))-1))

pres20_agg <- pres20_agg %>% 
  mutate(share_diff_q = as.numeric(findInterval(share_diff, unique(quantile(share_diff, seq(0, 1, 0.26)), #И тут
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

names(tap)

ggplot(subset(tap), aes(x = as.numeric(min_maj_scale), y = as.numeric(att_t_ex), color = as.factor(fr)))+
    geom_jitter(alpha = 0.5, size = 1, 
      width = 0.25, 
      height = 0.5)+
    xlim(0,1)+
    theme_minimal()+
    geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) + 
xlab('Respondents Ideological Congruence With The State Aggregate (High-Low)') + ylab('Extremity of Attitude Towards Transgender Legislation')+
labs(colour="Framed") +
scale_color_manual(values = c("grey20", "darkviolet"),
                    labels = c("NO", "YES")) +
theme_bw()+
  theme(
    legend.position = "top",                  # Move legend to the top
    legend.direction = "horizontal",          # Align legend items horizontally
    legend.title = element_text(angle = 0, hjust = 0.5), # Center legend title above
    legend.text = element_text(size = 10),    # Adjust size of legend text
  )
##### BY PARTISANSHIP

ggplot(subset(tap), aes(x = min_maj_scale, y = as.numeric(att_t_ex), color = as.factor(fr))) +
    geom_jitter(alpha = 0.3, size = 1, width = 0.3, height = 0.5)+
    geom_smooth(method = 'lm', formula = y ~ x + I(x^2), se = F) + facet_wrap(~rep_dem)+ 
xlab('Majority-Minority Scale') + ylab('Extremity of Attitude Towards Transgender Peopel')+
labs(colour="Treated")

#No Independent
ggplot(subset(tap, as.numeric(rep_dem) !='Ind'), aes(x = min_maj_scale, y = as.numeric(att_t_ex), color = as.factor(fr)))+
    geom_jitter(alpha = 0.3, size = 1, width = 0.3, height = 0.5)+
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

library(brms)
library(here)
model1 <- brm(
  as.numeric(att_t_ex) ~ 1 + min_maj_scale*as.factor(fr) + (1 | state),
  family = cumulative(link = "probit"),
  data = tap,
  seed = 1234,
  warmup = 1000, 
  iter   = 4000, 
  chains = 3, 
  cores  = 6,
  file = here("Output", "Models", "model1")
  #control = list(adapt_delta = 0.95)
)

pp_check(model1, type = "bars")
summary(model1)

priors <- c(
  # For main effects and interactions (standardized predictors)
  prior(normal(0, 2), class = "b"),
  # For the standard deviation of random effects
  prior(exponential(1), class = "sd")
)

model2 <- tap %>% 
   mutate(fr = as.factor(fr),
          att_t_ex = as.numeric(as.character(att_t_ex))) %>% 
    brm(att_t_ex ~ 1 + min_maj_scale_1*fr + min_maj_scale_sq*fr + (1 | state),
  family = cumulative(link = "probit"),
  data = .,
  seed = 1234,
  warmup = 1000, 
  iter   = 4000, 
  chains = 3, 
  cores  = 6,
  prior = priors,
  file = here("Output", "Models", "model2")
)

pp_check(model2, type = "bars")



summary(model2)


model1 <- readRDS("/Users/georgetarasenko/Dropbox/us_framing_project/rep_test/Output/Models/model1.rds")

model2 <- readRDS("/Users/georgetarasenko/Dropbox/us_framing_project/rep_test/Output/Models/model2.rds")

library(brms)

library(marginaleffects)
library(insight)
library(ggdist)





conditions <- data.frame(min_maj_scale_1 = c(1, 1.25, 1.5, 1.75, 2), min_maj_scale_sq = c(1, 1.5625, 1.75, 3.0625, 4))

plot(conditional_effects(model2, 
                          effects = "fr", conditions = conditions), ncol = 5)

avg_comparisons(model2)
stop()


table(tap$min_maj_scale_1)




grid <- data.frame(fr = 1, min_maj_scale_1 = 1, min_maj_scale_sq = 1, state = "NY")



pred <- predictions(model = model2,
                    newdata = datagrid(fr = c(0,1), 
                    min_maj_scale_1 = c(1, 1.5, 2), 
                    min_maj_scale_sq = c(1, 2.25, 4)), type = 'link')
pred <- get_draws(pred)
pred$d <- ifelse(pred$min_maj_scale_sq == pred$min_maj_scale_1^2, 1, 0)

df <- datagrid(fr = c(0,1), 
                    min_maj_scale_1 = c(1, 1.5, 2), 
                    min_maj_scale_sq = c(1, 2.25, 4))


preds <- predictions(model = model2,
                    newdata = tap, type = 'link')

aggregated <- preds %>%
  group_by(fr, min_maj_scale_1) %>%
  summarise(
    estimate = mean(estimate, na.rm = TRUE),
    conf.low = mean(conf.low, na.rm = TRUE),
    conf.high = mean(conf.high, na.rm = TRUE)
  )



differences <- aggregated %>%
  group_by(min_maj_scale_1) %>%
  summarise(
    estimate = mean(estimate[fr == 1]- estimate[fr == 0], na.rm = T),
    conf.low = mean(conf.low[fr == 1] - conf.low[fr == 0], na.rm = T),
    conf.high = mean(conf.high[fr == 1] - conf.high[fr == 0], na.rm = T))

  
ggplot(differences, aes(x = min_maj_scale_1, y =  estimate, ymin=conf.low, ymax=conf.high)) +
    geom_line() + 
    geom_ribbon(alpha=0.5) +
    geom_hline(yintercept = 0, linetype = 'dashed', color ='darkgrey')+
    xlab('Respondents Ideological Congruence With The State (High-Low)')+
    ylab('Framing Effect')
     




nd1 <- datagrid(model = model2,
              fr = 1, min_maj_scale_1 = 1, min_maj_scale_sq = 1)


nd2 <- datagrid(model = model2,
              fr = 1, min_maj_scale_1 = 1.5, min_maj_scale_sq = 1.75)


nd3 <- datagrid(model = model2,
              fr = 1, min_maj_scale_1 = 2, min_maj_scale_sq = 4)

p1 <- predictions(model2, type = "link", newdata = nd1) |>
    get_draws() |>
    transform(type = "Type1")

p2 <- predictions(model2, type = "link", newdata = nd2) |>
    get_draws() |>
    transform(type = "Type2")

p3 <- predictions(model2, type = "link", newdata = nd3) |>
    get_draws() |>
    transform(type = "Type3")

pred <- rbind(p1,p2,p3)


ggplot(pred, aes(x = draw, fill = fr)) +
    stat_halfeye(alpha = .5)+
    facet_wrap(~ type) 


# Counterfactual predictions
g_1_p <- predict(model2, newdata = g_1, type = "response")
g_0_p <- predict(model2, newdata = g_0, type = "response")

comparisons(model2, variables = "fr", newdata = grid)

# Counterfactual comparison
g_1_p - g_0_p
