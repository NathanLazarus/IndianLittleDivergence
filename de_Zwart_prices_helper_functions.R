# de_Zwart_prices_helper_functions.R

log_linearly_interpolate = function(data, var, newvar = FALSE) {
  data[, var_before := nafill(get(var), "locf")
                     ][, var_after := nafill(get(var), "nocb")
                     ][, rle := rleid(get(var))
                     ][, missings := max(.N +  1 , 2), by = rle
                     ]
  if (newvar != FALSE) {
    data[, (newvar) := is.na(get(var))]
  }
  data[is.na(get(var)), (var) := exp(log(var_before) + .SD[,.I] *
                        (log(var_after) - log(var_before))/(missings)), by = rle
     ][, c('var_before', 'var_after', 'rle', 'missings') := NULL]
}


deZwart_basket_names = function(x) {
  data.table(clean_name = x, ind = 1:length(x)
           )[, actual_name := gsub('_[0-9][0-9]?', '', clean_name)
           ][, n_occurences := .N, actual_name
           ][, instance := frank(ind), actual_name
           ][n_occurences - instance == 2 & n_occurences %in% c(2,3) & actual_name != 'cpi',
             my_name := paste0(actual_name, '_unitprice')
           ][n_occurences - instance == 1 & n_occurences %in% c(2,3) & actual_name != 'cpi',
             my_name := paste0(actual_name, '_basketprice')
           ][n_occurences - instance == 0 & n_occurences %in% c(2,3) & actual_name != 'cpi',
             my_name := paste0(actual_name, '_basketshare')
           ][is.na(my_name), my_name := clean_name
           ][, my_name]
}


read_excel_with_gap_between_colnames_and_data = function(file, sheet, colnamerow, datastartrow, deZwartNames) {
  colnames = make_clean_names(names(read_excel_dt(file, sheet, skip = colnamerow - 1)))
  if (deZwartNames == TRUE) colnames = deZwart_basket_names(colnames)
  return(read_excel_dt(file, sheet, skip = datastartrow - 1, col_names = colnames))
}

read_de_Zwart = function(regionname) {
  read_excel_with_gap_between_colnames_and_data('deZwartLucassen/5.price_series_north_india.xlsx',
                                                regionname,
                                                colnamerow = 2,
                                                datastartrow = 4,
                                                deZwartNames = T) %>%
  .[, .SD, .SDcols = c(1, grep('_basketprice', names(.)))] %>%
  setnames(., c('year', gsub('_basketprice', '', names(.)[2:length(names(.))]))) %>%
  .[!is.na(year)]
}


# Unused
my_de_Zwart_fix_nearest_only = function(data, yr, product) {
  thisyr = data[year == yr]
  notmissing = names(thisyr)[thisyr[, colSums(is.na(thisyr)) == 0] & names(thisyr) != 'year']
  if (length(notmissing) > 0) {
    if (thisyr[, is.na(get(product))]) {
      otherproduct = 'rice'
      joined_to_nearest = setnames(data[!is.na(get(product)) & eval(parse(text = paste0('(', paste0('!is.na(', notmissing, ')', collapse = ' | '), ')')))],
                                   paste0(names(data), '___forImputation')
                                 )[setnames(copy(thisyr), paste0(names(thisyr), '___preImputation')),
                                   on = .(year___forImputation = year___preImputation),
                                   roll = 'nearest']
      joined_long = melt(joined_to_nearest, variable.name = 'varAndYear', id = 'year___forImputation', value.name = 'basketprice'
                       )[, c('var', 'year') := tstrsplit(varAndYear, '___')
                       ][, use := sum(is.na(basketprice)) == 0 & var != 'year', var
                       ]
      return(joined_long[, ratio := sum(joined_long[use == TRUE & year == 'preImputation', basketprice]) / sum(joined_long[use == TRUE & year == 'forImputation', basketprice])
                       ][var == product & year == 'forImputation', basketprice * ratio])
    } else {
      thisyr[, get(product)]
    }
  } else {
    return(NA_real_)
  }
}


my_de_Zwart_fix = function(data, yr, product) {
  thisyr = data[year == yr]
  if (thisyr[, is.na(get(product))]) {
    notmissing = names(thisyr)[thisyr[, colSums(is.na(thisyr)) == 0] & names(thisyr) != 'year']
    if (length(notmissing) > 0) {
      imputation_relative_prices =
        foreach(otherproduct = notmissing, .combine = rbind) %do% {
          hasboth = data[!is.na(get(product)) & !is.na(get(otherproduct)),
                         .(year,
                           nearest_non_missing = get(product),
                           price_of_otherproduct_then = get(otherproduct),
                           nearest_year = year)]

          if (nrow(hasboth) > 0) {
            nearest_relative_price =
              hasboth[thisyr[, .(year, price_of_otherproduct_thisyr = get(otherproduct))],
                      on = .(year),
                      roll = 'nearest'
                    ][, weight := 1/abs(year - nearest_year)
                    ][, logPredictedPrice := log(nearest_non_missing * (price_of_otherproduct_thisyr / price_of_otherproduct_then))
                    ][, .(weight, logPredictedPrice)] #, otherproduct = otherproduct
          } else {
            nearest_relative_price = data.table()
          }
        }
      imputation_relative_prices[, exp(sum(logPredictedPrice * weight)/sum(weight))]
    } else {
      NA_real_
    }
  } else {
    thisyr[, get(product)]
  }
}


progbar = function(n) setTxtProgressBar(txtProgressBar(min = 1, max = 311, style = 3), n)

impute = function(data) {
  all_prices_filled_in =
    foreach(year = unique(data$year), .combine = rbind, .packages = 'foreach',
            .export = 'my_de_Zwart_fix', .options.snow = list(progress = progbar)) %dopar% {
      foreach(product = names(data), .combine = cbind) %do% {
        setnames(data.table(my_de_Zwart_fix(data, year, product)), product)
      }
    }
  all_prices_filled_in[, cpi := rowSums(all_prices_filled_in[, .SD, .SDcols = !'year'])]

  log_linearly_interpolate(all_prices_filled_in, 'cpi', newvar = 'logLinearInterpolation')
}