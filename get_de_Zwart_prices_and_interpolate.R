# get_de_Zwart_prices_and_interpolate.R

get_de_Zwart_prices_and_interpolate = function(write_prices = FALSE) {

  east_de_Zwart_prices = read_de_Zwart('BasketNEI')

  north_de_Zwart_prices = read_de_Zwart('BasketNOI')

  west_de_Zwart_prices = read_de_Zwart('BasketNWI')


  clusters = makeCluster(7)
  registerDoSNOW(clusters)
  east_de_Zwart_prices_imputed = impute(east_de_Zwart_prices)
  north_de_Zwart_prices_imputed = impute(north_de_Zwart_prices)
  west_de_Zwart_prices_imputed = impute(west_de_Zwart_prices)
  stopCluster(clusters)



  de_Zwart_prices = rbind(east_de_Zwart_prices_imputed[, .(year, region = 'east', prices = cpi)],
                          north_de_Zwart_prices_imputed[, .(year, region = 'north', prices = cpi)],
                          west_de_Zwart_prices_imputed[, .(year, region = 'west', prices = cpi)])


  prices_of_goods_Allen_didnt_interpolate_that_are_in_de_Zwart_basket =
    north_de_Zwart_prices_imputed[, .(year, northBajra = millet * grams_of_silver_in_a_rupee / 60, northBeansPeas = beans_peas * grams_of_silver_in_a_rupee / 10)] %>%
      log_linearly_interpolate(., 'northBeansPeas') %>%
      log_linearly_interpolate(., 'northBajra') %>%
      .[log_linearly_interpolate(west_de_Zwart_prices_imputed[, .(year, bajra = millet * grams_of_silver_in_a_rupee / 60)], 'bajra'),
        on = .(year), westBajra := i.bajra]

  if (write_prices == TRUE) write.xlsx(prices_of_goods_Allen_didnt_interpolate_that_are_in_de_Zwart_basket, 'prices_of_goods_Allen_didnt_interpolate.xlsx')

  return(de_Zwart_prices)
}