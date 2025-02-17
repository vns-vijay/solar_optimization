library(dplyr)
library(lubridate)

library(ompr)
# library(ROI.plugin.cbc)
library(ROI)
library(ROI.plugin.glpk)
library(ompr.roi)

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




battery_cost <- 0.5
pannel_cost <- 300
total_ts <- 48
delta <- 0.5
bat_disch_limit <- 500
bat_charg_limit <- 500

model <- MIPModel()

# Add variables
# model <- add_variable(model, "x", type = "integer", lb = 0)  


model <- add_variable(model, yb, type = "integer", lb = 0)
model <- add_variable(model, ys, type = "integer", lb = 0)
model <- add_variable(model, xc[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, xd[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, sc[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, sd[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, z[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, i[t], t=0:total_ts, type = "continuous", lb = 0)


# Set the objective function (minimize cost)
# tarrif_cost <- 0
# for (t in 1:total_ts) {
#   tarrif_cost <- tarrif_cost + dct$avg_tarrif_kwh[t] * (xc[t]+xd[t])
# }
# objective_expr <- tarrif_cost + battery_cost*yb + pannel_cost*ys

model <- set_objective(model, battery_cost*yb + pannel_cost*ys + 
                         sum_expr(dct$avg_tarrif_kwh[t]*(xc[t]+xd[t]), t=1:total_ts),
                       sense = "min")

# charging constraint
# for (t in 1:total_ts){
#   constraint_expr1 <- paste("i",t-1) + delta*(paste0('xc_',t)+paste0('sc_',t)) - delta*paste0('z_',t) - paste("i",t)
#   model <- add_constraint(model, constraint_expr1 <= 0)
# }
model <- add_constraint(model, i[t-1]+delta*(xc[t]+sc[t])-delta*z[t]==i[t], t=1:total_ts)


# bat_disch_limit constriant
# for (t in 1:total_ts){
#   model <- add_constraint(model, paste0("z_",t) <= bat_disch_limit)
# }
model <- add_constraint(model, z[t]<=bat_disch_limit, t=1:total_ts)


# bat_charg_limit constraint
# for (t in 1:total_ts){
#   model <- add_constraint(model, paste0('xc_',t)+paste0('sc_',t) <= bat_charg_limit)
# }
model <- add_constraint(model, xc[t]+sc[t]==bat_charg_limit, t=1:total_ts)

# demand constraint
# for (t in 1:total_ts){
#   model <- add_constraint(model, paste0('sd_',t)+paste0('xd_',t)+paste0("z_",t) == dct$avg_demand_w[t])
# }
model <- add_constraint(model, xd[t]+sd[t]+delta*z[t]==dct$avg_demand_w[t], t=1:total_ts)

# solar supply constraint
# for (t in 1:total_ts){
#   model <- add_constraint(model, paste0("sc_",t)+paste0("sd_",t) == dct$avg_solar_w[t])
# }
model <- add_constraint(model, xc[t]+sd[t]==dct$avg_solar_w[t], t=1:total_ts)

# battery capacity constraint
# for (t in 1:total_ts){
#   model <- add_constraint(model, paste0("i_",t) <= "y_b")
# }
model <- add_constraint(model, i[t]<=yb, t=1:total_ts)

# Solve the model using CBC solver
result <- solve_model(model, with_ROI(solver = "glpk"))

# Get the optimal solution
solution <- result$solution


