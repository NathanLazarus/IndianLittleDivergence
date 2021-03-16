library(Hmisc)
library(data.table)
library(foreach)

true_mean = 0
n_big_group = 200
n_small_group = 40
n_groups_big_group = 10
n_groups_small_group = 50
n_groups = n_groups_big_group + n_groups_small_group
within_group_sd = 0.1

estimates =
  foreach(seed = 1:500, .combine = rbind) %do% {
    set.seed(seed)
    data = data.table(id = c(rep(1:n_groups_big_group, times = n_big_group),
                             rep((n_groups_big_group + 1):n_groups, times = n_small_group))
                    )[data.table(id = 1:n_groups, within_group_noise = rnorm(n_groups, sd = within_group_sd)), on = .(id)
                    ][, val := true_mean + within_group_noise + rnorm(n_big_group * n_groups_big_group + n_small_group * n_groups_small_group)]

    data.table(unbinned = mean(data$val),
               binned = mean(data[, .(val = mean(val)), id]$val),
               my_estimate = data[, .(my_weight = sqrt(.N), val = mean(val)), id
                                ][, Hmisc::wtd.mean(val, weights = my_weight)])
  }

# RMSE
estimates[, lapply(.SD, function(x) sqrt(mean((x - true_mean)^2)))]
