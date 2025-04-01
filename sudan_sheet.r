
library("ggplot2")
library("dplyr")
library("magrittr")
library("tidyr")
library(purrr)
library(readr)
library(cmdstanr)
library(bayesplot)
library("scales")
library("sf")




#Some of the steps in this code are not necessary for Sudan, but reflects that the code was originally written for other places.
# This is things we do in case of missing values, where Sudan actually does not have a lot of.


load("full_displaced.RDS") # All displacement data for Sudan and neighboring countries
load("violent_events_sdn.RDS") # Conflict data for Sudan
load("neighbor_df_sdn.RDS") # Conflict data for neighboring admins in Sudan
load("total_fews.RDS") # FEWS Net data for Sudan



#neighbour_events per country - neighbouring countries are not in the shapefile and the "neighbor" values for those areas are calculated seperately:
#Egypt
neighbour_egypt <- violent_events_sdn %>% filter(ADMIN1 %in% c("Northern","River Nile","Red Sea","Khartoum","Kassala","North Kordofan")) %>%
  group_by(MONTH, YEAR) %>%
  summarise(nbrs_n_mean = mean(n), nbrs_deaths_mean = mean(deaths), .groups = "drop")
#Ethiopia
neighbour_ethi <- violent_events_sdn %>% filter(ADMIN1 %in% c("White Nile","Sennar","Blue Nile","Gedaref","Aj Jazirah")) %>%
  group_by(MONTH, YEAR) %>%
  summarise(nbrs_n_mean = mean(n), nbrs_deaths_mean = mean(deaths), .groups = "drop")
#CAR
neighbour_car <- violent_events_sdn %>% filter(ADMIN1 %in% c("Central Darfur","South Darfur")) %>%
  group_by(MONTH, YEAR) %>%
  summarise(nbrs_n_mean = mean(n), nbrs_deaths_mean = mean(deaths), .groups = "drop")
#CHad
neighbour_chad <- violent_events_sdn %>% filter(ADMIN1 %in% c("Central Darfur","South Darfur","West Darfur","North Darfur")) %>%
  group_by(MONTH, YEAR) %>%
  summarise(nbrs_n_mean = mean(n), nbrs_deaths_mean = mean(deaths), .groups = "drop")
#SSD
neighbour_ssd <- violent_events_sdn %>% filter(ADMIN1 %in% c("South Darfur","East Darfur","West Kordofan","South Kordofan","White Nile","North Kordofan")) %>%
  group_by(MONTH, YEAR) %>%
  summarise(nbrs_n_mean = mean(n), nbrs_deaths_mean = mean(deaths), .groups = "drop")


# We do not use food security data for the neighboring countires, but for the admin areas close to.
#Ethiopia
neighbour_ethi_fews <- total_fews %>% filter(ADM1_EN %in% c("White Nile","Sennar","Blue Nile","Gedaref","Aj Jazirah")) %>%
  group_by(month, year) %>%
  summarise(weighted_avg_indicator = mean(weighted_avg_indicator), above_threshold= any(above_threshold), .groups = "drop")
#CAR
neighbour_car_fews <- total_fews %>% filter(ADM1_EN %in% c("Central Darfur","South Darfur")) %>%
  group_by(month, year) %>%
  summarise(weighted_avg_indicator = mean(weighted_avg_indicator), above_threshold= any(above_threshold), .groups = "drop")
#CHad
neighbour_chad_fews <- total_fews %>% filter(ADM1_EN %in% c("Central Darfur","South Darfur","West Darfur","North Darfur")) %>%
  group_by(month, year) %>%
  summarise(weighted_avg_indicator = mean(weighted_avg_indicator), above_threshold= any(above_threshold), .groups = "drop")
#SSD
neighbour_ssd_fews <- total_fews %>% filter(ADM1_EN %in% c("South Darfur","East Darfur","West Kordofan","South Kordofan","White Nile","North Kordofan")) %>%
  group_by(month, year) %>%
  summarise(weighted_avg_indicator = mean(weighted_avg_indicator), above_threshold= any(above_threshold), .groups = "drop")




# Remove Egypt
all_sdn_provinces <- c("Aj Jazirah","Blue Nile","Central Darfur","Kassala","Khartoum","North Darfur",
                       "North Kordofan","Northern","Red Sea","River Nile","Sennar","South Darfur",
                       "West Darfur","White Nile","East Darfur","Gedaref","South Kordofan","West Kordofan",
                       "Ethiopia","CAR","Chad","South Sudan")
H <- length(all_sdn_provinces)
all_sdn_months <- data.frame(Month = rep(1:12, 3*H), year = rep(rep(2023:2025, each = 12),H), admin1Name = rep(all_sdn_provinces, each = 36))
all_sdn_months %<>% filter(!(year==2023 & Month<4))
all_sdn_months %<>% filter(!(year==2025 & Month>1))



# Construct data.frame with data:
all_sdn_idp <- left_join(all_sdn_months, full_displaced, by = c("Month","year","admin1Name")) %>% arrange(admin1Name, year, Month)
all_sdn_idp <- all_sdn_idp %>% group_by(admin1Name) %>% mutate(time = row_number()) %>% ungroup()

#Attach conflict
all_sdn_idp <- left_join(all_sdn_idp, violent_events_sdn, by = c("admin1Name" = "ADMIN1","Month" = "MONTH", "year" = "YEAR")) %>% rename(violent_events = n) 

full_neighbor <- rbind(neighbor_df_sdn,
                       neighbour_egypt %>% mutate(ADM1_EN = "Egypt"),
                       neighbour_ethi %>% mutate(ADM1_EN = "Ethiopia"),
                       neighbour_car %>% mutate(ADM1_EN = "CAR"),
                       neighbour_chad %>% mutate(ADM1_EN = "Chad"),
                       neighbour_ssd %>% mutate(ADM1_EN = "South Sudan"))

#Attach neighbor conflict
all_sdn_idp <- left_join(all_sdn_idp, full_neighbor, by = c("admin1Name" = "ADM1_EN","Month" = "MONTH", "year" = "YEAR")) %>% rename(neighbour_events = nbrs_n_mean, neighbour_deaths = nbrs_deaths_mean) 

full_fews <- rbind(total_fews,
                   neighbour_ethi_fews %>% mutate(ADM1_EN = "Ethiopia"),
                   neighbour_car_fews %>% mutate(ADM1_EN = "CAR"),
                   neighbour_chad_fews %>% mutate(ADM1_EN = "Chad"),
                   neighbour_ssd_fews %>% mutate(ADM1_EN = "South Sudan"))

#Attach FEWS NET data
all_sdn_idp <- left_join(all_sdn_idp, 
                         full_fews %>% select(ADM1_EN, weighted_avg_indicator, above_threshold, Month = month, year), 
                         by = c("admin1Name" = "ADM1_EN","Month","year")) 


all_sdn_idp %<>% mutate(violent_events = replace_na(violent_events, 0), neighbour_events = replace_na(neighbour_events, 0),
                        deaths = replace_na(deaths, 0), neighbour_deaths = replace_na(neighbour_deaths, 0))




all_sdn_idp %<>% mutate(log_violence = log(violent_events+1), log_neighbour = log(neighbour_events+1),
                        log_deaths = log(deaths+1), log_neighbour_d = log(neighbour_deaths+1))

all_sdn_idp %<>% mutate(log_comp_violence = ifelse(admin1Name %in% c("South Sudan","CAR","Chad","Ethiopia"), 
                                                   log(neighbour_events + 1),
                                                   log(violent_events + 1)))

#We scale each variable for a better fit:
all_sdn_idp %<>% mutate(log_violence_scaled = as.vector(scale(log_violence)))
all_sdn_idp %<>% mutate(log_neighbour_scaled = as.vector(scale(log_neighbour)))
all_sdn_idp %<>% mutate(log_comp_scaled = as.vector(scale(log_comp_violence)))


all_sdn_idp %<>% mutate(capitol = ifelse(admin1Name=="Khartoum", 1, 0))
all_sdn_idp %<>% mutate(outside = ifelse(admin1Name %in% c("Egypt","Ethiopia","CAR","Chad","South Sudan"), 1, 0))

all_sdn_idp %<>% mutate(log_outside_scaled = log_neighbour_scaled*outside)
all_sdn_idp %<>% mutate(log_outside_comp_scaled = log_comp_scaled*outside)

all_sdn_idp %<>% mutate(scaled_fews = as.vector(scale(weighted_avg_indicator)))
all_sdn_idp %<>% mutate(outside_scaled_fews = scaled_fews*outside)
all_sdn_idp %<>% mutate(famine = ifelse(above_threshold,1,0))
all_sdn_idp %<>% mutate(outside_famine = famine*outside)


admin_areas <- all_sdn_idp %>% distinct(admin1Name) %>% unlist() %>% unname()


S = H
TT = 19 #Last time step used.
gourma_df <- all_sdn_idp %>% filter( time<TT+1) %>% group_by(County = admin1Name) %>% mutate(internal_time = row_number()) %>% ungroup()
real_data <- gourma_df %>% select(County, idp, time, internal_time, log_violence_scaled,
                                  log_neighbour_scaled, capitol, outside, log_outside_scaled,
                                  scaled_fews, outside_scaled_fews, log_comp_scaled, log_outside_comp_scaled) %>% mutate(rn = row_number()) 



data_missing <- which(is.na(real_data$idp))

idp_matrix <- as.matrix(real_data %>% select(County, time, idp) %>% mutate(idp = replace_na(idp, -1)) %>% pivot_wider(names_from = County, values_from = idp) %>% select(-time))
# Violent events
first_indicator_mat <- as.matrix(real_data %>% select(County, time, variable = log_violence_scaled) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))
second_indicator_mat <- as.matrix(real_data %>% select(County, time, variable = outside) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))
third_indicator_mat <- as.matrix(real_data %>% select(County, time, variable = scaled_fews) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))
fourth_indicator_mat <- as.matrix(real_data %>% select(County, time, variable = outside_scaled_fews) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))
#fifth_indicator_mat <- as.matrix(real_data %>% select(County, time, variable = log_outside_comp_scaled) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))
#sixth_indicator_mat <- as.matrix(real_data %>% select(County, time, variable = log_outside_scaled) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))

dim_event <- dim(idp_matrix)

N_missing <- nrow(real_data %>% filter(is.na(idp)))
missing_count <- 1
Jmi <- matrix(0,N_missing, 2)
county_missing <- data.frame(County = rep("empty",N_missing), time = rep(0, N_missing))


for(i in 1:dim_event[1]){
  for(j in 1:dim_event[2]){
    if(idp_matrix[i,j]<0){
      Jmi[missing_count, 1] <- i
      Jmi[missing_count, 2] <- j
      county_missing$County[missing_count] <- admin_areas[j]
      county_missing$time[missing_count] <- i
      missing_count <- missing_count + 1
    }
  }
}



#Compile the Bayesian model:
sdn_simple <- cmdstan_model("sdn_model_simple.stan")


data_list <- list(TT = TT, K = 4, S = S,
                  N_missing = N_missing, Jmi = Jmi,
                  y = idp_matrix, 
                  x1 = first_indicator_mat, 
                  x2 = second_indicator_mat,
                  x3 = third_indicator_mat,
                  x4 = fourth_indicator_mat)

# Fit the model:
fit <- sdn_simple$sample(
  data = data_list,
  chains = 4,
  parallel_chains = 4,
  refresh = 200 # print update every 200 iters
)

fitz <- fit$summary(c("beta","theta","khi","tau","gamma","phi","y_mis"))
fitz

draws_df <- fit$draws(format = "df")
ggplot(data=draws_df) + geom_point(aes(x=`beta[1]`, y=`beta[5]`))
ggplot(data=draws_df) + geom_point(aes(x=`khi`, y=`theta`))




#Plot the fitted values for an admin area:
imputed_df <- fitz %>% filter(grepl("mis", variable))
imputed_df$time <- county_missing$time

province_order <- gourma_df %>% distinct(County)
imputed_df$County <- county_missing$County

plot_province = "South Sudan"
real_data %>% filter(County==plot_province) %>% ggplot() + geom_point(aes(x=time, y=idp)) + 
  geom_rect(data=imputed_df %>% filter(County==plot_province), aes(xmin=time-0.5, xmax=time+.5, ymin=q5, ymax=q95), fill="red", alpha=0.25) + 
  geom_point(data=imputed_df %>% filter(County==plot_province), aes(x=time, y=median), color="red") + ggtitle(plot_province) + theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text = element_text(face="bold", size = 10), panel.grid.minor = element_blank())




# We draw from the model using new_df - From the model we have 4000 samples of the values of the parameters. 
# For each of those 4000 instances we will draw values for the displacement to give us a distribution of those.


TT_new <- 3
new_df <- all_sdn_idp %>% filter( time>TT, time < TT+4) %>% group_by(County = admin1Name) %>% 
  mutate(internal_time = row_number()) %>% ungroup() %>% 
  select(County, idp, time, internal_time, log_violence_scaled,
         log_neighbour_scaled, capitol, outside,log_outside_scaled, 
         scaled_fews, outside_scaled_fews, log_comp_scaled, log_outside_comp_scaled)

new_df %<>% arrange(County, time)

new_first_variable <- as.matrix(new_df %>% select(County, time, variable = log_violence_scaled) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))
new_second_variable <- as.matrix(new_df %>% select(County, time, variable = outside) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))
new_third_variable <- as.matrix(new_df %>% select(County, time, variable = scaled_fews) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))
new_fourth_variable <- as.matrix(new_df %>% select(County, time, variable = outside_scaled_fews) %>% pivot_wider(names_from = County, values_from = variable) %>% select(-time))



y_samp <- list()

final_time <- real_data %>% filter(time==TT)

phi_mat <- as.matrix(draws_df %>% select(`phi[1]`:`phi[22]`))
beta_mat <- as.matrix(draws_df %>% select(`beta[1]`:`beta[5]`))
theta_mat <- as.matrix(draws_df %>% select(theta))
khi_mat <- as.matrix(draws_df %>% select(khi))

mu_log_next <- matrix(NA, TT_new, S)

mu_log_mat_last <- as.matrix(unname(draws_df %>% select(contains(paste0("mu_log[",TT)))))

Yl_mat_last <- as.matrix(unname(draws_df %>% select(contains(paste0("Yl[",TT)))))
Yl_mat_nexttolast <- as.matrix(unname(draws_df %>% select(contains(paste0("Yl[",TT-1)))))

y_new <- matrix(NA, TT_new, S)



for(j in 1:4000){
  phi_draw <- unname(phi_mat[j,])
  beta_draw <- beta_mat[i,]
  
  khi_draw <- khi_mat[j]
  theta_draw <- theta_mat[j]
  last_error <- log(Yl_mat_last[j, ]) - log(mu_log_mat_last[j, ])
  last_change <- log(Yl_mat_last[j, ]) - log(Yl_mat_nexttolast[j, ])
  
  #mu_next <- x_new_mat%*%beta_draw
  
  for(s in 1:S){
    mu_log_next[1, s] = exp(beta_draw[1]*new_first_variable[1,s] + beta_draw[2]*new_second_variable[1,s] + 
                              beta_draw[3]*new_third_variable[1,s] + beta_draw[4]*new_fourth_variable[1,s] +
                              log(final_time$idp[s]+1) + khi_draw*last_change[s] + theta_draw*last_error[s])
    y_new[1, s] = rgamma(1, mu_log_next[1, s]/phi_draw[s], 1/phi_draw[s])
  }
  next_error <- log(y_new[1, ]) - log(mu_log_next[1, ])
  next_change <- log(y_new[1, ]) - log(final_time$idp)
  
  
  for(t in 2:TT_new){
    for(s in 1:S){
      mu_log_next[t, s] = exp(beta_draw[1]*new_first_variable[t,s] + beta_draw[2]*new_second_variable[t,s] + 
                                beta_draw[3]*new_third_variable[t,s] + beta_draw[4]*new_fourth_variable[t,s] +
                                log(y_new[t-1, s]) + khi_draw*next_change[s] + theta_draw*next_error[s])
      y_new[t, s] = rgamma(1, mu_log_next[t, s]/phi_draw[s], 1/phi_draw[s])
    }
    next_error <- log(y_new[t, ]) - log(mu_log_next[t, ])
    next_change <- log(y_new[t, ]) - log(y_new[t-1,])
  } 
  
  colnames(y_new) <- admin_areas
  
  output_df <- data.frame(y_new = matrix(y_new, nrow = TT_new*S, ncol = 1), 
                          draw = rep(i, TT_new*S), rn = 1:(TT_new*S),
                          County = rep(admin_areas, each = TT_new))
  y_samp[[j]] <- output_df 
}


y_draws <- bind_rows(y_samp)

summa_y <- y_draws %>% group_by(rn) %>% summarise(median = median(y_new), q5 = quantile(y_new, 0.05), q95 = quantile(y_new, 0.95), .groups = "drop") 
summa_y$County <- new_df$County
summa_y$time <- new_df$time





# Now we have the samples and can calculate values:
summa_y <- y_draws %>% group_by(rn) %>% summarise(median = median(y_new), q5 = quantile(y_new, 0.05), q95 = quantile(y_new, 0.95), .groups = "drop") 


imputed_df <- fitz %>% filter(grepl("mis", variable))
imputed_df$time <- real_data$time[data_missing]

province_order <- gourma_df %>% distinct(County)
imputed_df$County <- real_data$County[data_missing]

summa_y$County <- new_df$County
summa_y$time <- new_df$time


# Example plot:
plot_province = "North Darfur"
real_data %>% filter(County==plot_province) %>% ggplot() + geom_point(aes(x=time, y=idp)) + 
  geom_rect(data=imputed_df %>% filter(County==plot_province), aes(xmin=time-0.5, xmax=time+.5, ymin=q5, ymax=q95), fill="grey", alpha=0.2) + 
  geom_point(data=imputed_df %>% filter(County==plot_province), aes(x=time, y=median), color="grey") + 
  geom_rect(data=summa_y %>% filter(County==plot_province), aes(xmin=time-0.5, xmax=time+.5, ymin=q5, ymax=q95), fill="red", alpha=0.15) + 
  geom_point(data=summa_y %>% filter(County==plot_province), aes(x=time, y=median), color="red") +
  ggtitle(plot_province) + theme_minimal() +
  scale_y_continuous(labels = comma, name = "IDP in Area") + scale_x_continuous(name="",breaks = c(0.75,9.75, 15.75, 21.75), labels = c("Apr 2023","Jan 2024","Jul 2024","Jan 2025")) +
  theme(axis.text = element_text(face="bold", size = 10), panel.grid.minor = element_blank(), plot.background = element_rect(fill="white"))



# All admins
real_data %>% ggplot() + geom_point(aes(x=time, y=idp)) +
  geom_rect(data=summa_y , aes(xmin=time-0.5, xmax=time+.5, ymin=q5, ymax=q95), fill="red", alpha=0.15) + 
  geom_point(data=summa_y , aes(x=time, y=median), color="red") + facet_wrap(.~County, ncol = 5, scales = "free_y") + theme_minimal() +
  scale_y_continuous(labels = comma, name = "Displaced in Area") + scale_x_continuous(name="",breaks = c(0.75,9.75,15.75, 21.75), labels = c("Apr 23","Jan 24","Jul 24","Jan 25")) +
  theme(axis.text = element_text(face="bold", size = 10), panel.grid.minor = element_blank(), plot.background = element_rect(fill="white")) 












