library(dplyr)
library(lubridate)

df <- read.csv("/Users/vijaysharma/Downloads/argus_assn/input_data/Argus_Media_Optim_Test_Data.csv")

df$tariff_p_kwh <- df$tariff_p_kwh / 100

df$timestamp <- dmy_hm(df$timestamp)
df$time_part <- format(df$timestamp, "%H%M")
df$time_part <- as.integer(df$time_part)
df$time_part <- rank(df$time_part, ties.method = "dense")

# Group by time_part and aggregate
aggr_df <- df %>%
  group_by(time_part) %>%
  summarise(
    avg_demand_w = mean(demand_w, na.rm = TRUE),
    avg_solar_w = mean(solar_w, na.rm = TRUE),
    avg_tarrif_kwh = mean(tariff_p_kwh, na.rm = TRUE)
  )

aggr_df <- aggr_df %>%
  mutate(across(where(is.numeric), round, 2))

# similar to to_dict in Python
dct <- as.list(aggr_df)

print(dct)

library(ompr)
library(ROI.plugin.cbc)

battery_cost <- 0.5
pannel_cost <- 300
total_ts = 48
delta = 0.5

model <- MIPModel()

# Add variables
# model <- add_variable(model, "x", type = "integer", lb = 0)  
for (t in 1:total_ts) {
  model <- add_variable(model, paste0("xc_", t), type = "continuous", lb = 0)
  model <- add_variable(model, paste0("xd_", t), type = "continuous", lb = 0)
  model <- add_variable(model, paste0("sc_", t), type = "continuous", lb = 0)
  model <- add_variable(model, paste0("sd_", t), type = "continuous", lb = 0)
  model <- add_variable(model, paste0("z_", t), type = "continuous", lb = 0)
  model <- add_variable(model, paste0("i_", t), type = "continuous", lb = 0)
}
model <- add_variable(model, "yb", type = "integer", lb = 0)
model <- add_variable(model, "ys", type = "integer", lb = 0)

# Set the objective function (minimize cost)
tarrif_cost <- 0
for (t in 1:total_ts) {
  tarrif_cost <- tarrif_cost + dct$avg_tarrif_kwh[t] * (paste0("xc_", t)+paste0("xd_", t))
}
objective_expr <- tarrif_cost + battery_cost*'yb' + pannel_cost*'yc'
model <- set_objective(model, objective_expr, sense = "min")

# charging constraint
for (t in 1:total_ts){
  constraint_expr <- paste("i",t-1) + delta*(paste0('xc_',t)+paste0('sc_',t)) - delta*paste0('z_',t) - paste("i",t)
  model <- add_constraint(model, constraint_expr <= 0)
}




