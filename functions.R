# functions.R

read_excel_dt = function(...) {
  data.table(suppressMessages(read_excel(...)))
}

na0 = function(x) fifelse(!is.na(x), x, 0)
na1 = function(x) fifelse(!is.na(x), x, 1)
NaNtoNA = function(x) fifelse(!is.nan(x), x, NA_real_)

harmonicmean = function(x, y) {
  fifelse(is.na(x) & is.na(y), NA_real_, (2 * na1(x) * na1(y)) / (na0(x) + na0(y)))
}

MA_with_partial_window = function(data, MA_var, windowsize_or_vec, groupvar = NULL, align = 'center', na.rm = TRUE) {
  if (align == 'custom') {
    window = windowsize_or_vec
  }
  if (align == 'center') {
    stopifnot(windowsize_or_vec %% 2 == 1)
    window = (-(windowsize_or_vec - 1) / 2):((windowsize_or_vec - 1) / 2)
  }
  data_local = copy(data)
  # data[, MA_var := get(MA_var)]
  eval(parse(text = paste0('data_local[, MA_var := ', MA_var, ']')))
  eval(parse(text = paste0('data_local[, c("MA_varLag_', window, '") := shift(MA_var, ', window, '), ', groupvar, ']', collapse = '; ')))
  return(NaNtoNA(data_local[, rowMeans(.SD, na.rm = na.rm), .SDcols = paste0('MA_varLag_', window)]))
  data_local[, c('MA_var', paste0('MA_varLag_', window)) := NULL]
}