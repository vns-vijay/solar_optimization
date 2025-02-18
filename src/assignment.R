library(dplyr)
library(lubridate)

library(ompr)
# library(ROI.plugin.cbc)
library(ROI)
library(ROI.plugin.glpk)
library(ompr.roi)
library(ggplot2)

df <- read.csv("/Users/vijaysharma/Downloads/argus_assn/input_data/Argus_Media_Optim_Test_Data.csv")

df$tariff_p_kwh <- df$tariff_p_kwh / 100

df$timestamp <- dmy_hm(df$timestamp)
df$time_part <- format(df$timestamp, "%H%M")
df$time_part <- as.integer(df$time_part)
# df$time_part <- rank(df$time_part, ties.method = "dense")

# group by time_part and aggregate
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

# parametrs
battery_cost <- 0.5
pannel_cost <- 300
total_ts <- 48
delta <- 0.5
bat_disch_limit <- 500
bat_charg_limit <- 500

model <- MIPModel()

# variables
model <- add_variable(model, yb, type = "integer", lb = 0)
model <- add_variable(model, ys, type = "integer", lb = 0)
model <- add_variable(model, xc[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, xd[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, sc[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, sd[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, z[t], t=1:total_ts, type = "continuous", lb = 0)
model <- add_variable(model, i[t], t=0:total_ts, type = "continuous", lb = 0)


# objective function: minimize cost
model <- set_objective(model, battery_cost*yb + pannel_cost*ys + 
                         sum_expr(dct$avg_tarrif_kwh[t]*(xc[t]+xd[t]), t=1:total_ts),
                       sense = "min")

# charging constraint
model <- add_constraint(model, i[t-1]+delta*(xc[t]+sc[t])-delta*z[t]==i[t], t=1:total_ts)
model <- add_constraint(model, i[0]==0)


# bat_disch_limit constriant
model <- add_constraint(model, z[t]<=bat_disch_limit, t=1:total_ts)


# bat_charg_limit constraint
model <- add_constraint(model, xc[t]+sc[t]<=bat_charg_limit, t=1:total_ts)

# demand constraint
model <- add_constraint(model, xd[t]+sd[t]+z[t]==dct$avg_demand_w[t], t=1:total_ts)

# solar supply constraint
model <- add_constraint(model, sc[t]+sd[t]<=dct$avg_solar_w[t]*ys, t=1:total_ts)

# battery capacity constraint
model <- add_constraint(model, i[t]<=yb, t=1:total_ts)

# Solve the model using CBC solver
result <- solve_model(model, with_ROI(solver = "glpk"))

# get the optimal solution
solution <- result$solution

# create dataframe
solution_df <- data.frame(
  Variable = names(solution), 
  Value = unname(solution)
)

# filter different variables
df_xc <- solution_df[grepl("^xc", solution_df$Variable), ]
df_xd <- solution_df[grepl("^xd", solution_df$Variable), ]
df_sc <- solution_df[grepl("^sc", solution_df$Variable), ]
df_sd <- solution_df[grepl("^sd", solution_df$Variable), ]
df_i <- solution_df[grepl("^i", solution_df$Variable), ]
df_z <- solution_df[grepl("^z", solution_df$Variable), ]


# plot the results
df_xc <- df_xc %>%
  mutate(Timestamp = as.numeric(sub("xc\\[(\\d+)\\]", "\\1", Variable)))
ggplot(df_xc, aes(x = Timestamp, y = Value)) +
  geom_bar(stat = "identity") +  # 'identity' because we're using pre-calculated values
  theme_minimal() +  # Clean theme for the plot
  labs(title = "Charging battery from grid", x = "Timestamp", y = "Charging (W)") +
  scale_x_continuous(breaks = seq(2,48, by=2))


df_xd <- df_xd %>%
  mutate(Timestamp = as.numeric(sub("xd\\[(\\d+)\\]", "\\1", Variable)))
ggplot(df_xd, aes(x = Timestamp, y = Value)) +
  geom_bar(stat = "identity") +  # 'identity' because we're using pre-calculated values
  theme_minimal() +  # Clean theme for the plot
  labs(title = "Direct supply from grid", x = "Timestamp", y = "Electricity (W)") +
  scale_x_continuous(breaks = seq(2,48, by=2))


df_sc <- df_sc %>%
  mutate(Timestamp = as.numeric(sub("sc\\[(\\d+)\\]", "\\1", Variable)))
ggplot(df_sc, aes(x = Timestamp, y = Value)) +
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  labs(title = "Charging battery from solar panel", x = "Timestamp", y = "Charging (W)") +
  scale_x_continuous(breaks = seq(2,48, by=2))

df_sd <- df_sd %>%
  mutate(Timestamp = as.numeric(sub("sd\\[(\\d+)\\]", "\\1", Variable)))
ggplot(df_sd, aes(x = Timestamp, y = Value)) +
  geom_bar(stat = "identity") +  
  theme_minimal() +  
  labs(title = "Direct supply from solar panel", x = "Timestamp", y = "Electricity (W)") +
  scale_x_continuous(breaks = seq(2,48, by=2))

df_z <- df_z %>%
  mutate(Timestamp = as.numeric(sub("z\\[(\\d+)\\]", "\\1", Variable)))
ggplot(df_z, aes(x = Timestamp, y = Value)) +
  geom_bar(stat = "identity") +  
  theme_minimal() +  
  labs(title = "Supply from battery", x = "Timestamp", y = "Electricity (W)") +
  scale_x_continuous(breaks = seq(2,48, by=2))

df_i <- df_i %>%
  mutate(Timestamp = as.numeric(sub("i\\[(\\d+)\\]", "\\1", Variable)))
ggplot(df_i, aes(x = Timestamp, y = Value)) +
  geom_bar(stat = "identity") +  
  theme_minimal() +  
  labs(title = "Battery status ", x = "Timestamp", y = "Charge (Wh)") +
  scale_x_continuous(breaks = seq(2,48, by=2))

