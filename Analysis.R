# Brings together the Allen and de Zwart and Lucassen wages and prices

allen_hh_size = 3.15
de_Zwart_hh_size = 4.1
hh_size = de_Zwart_hh_size
grams_of_silver_in_a_rupee = 10.78
# This number is wrong, but I just use it to get the rupee values from Allen's data.
# It doesn't matter anyway, both prices and wages are originally in rupees.


allen = get_Allen_wages(hh_size = hh_size)

de_Zwart_prices = get_de_Zwart_prices_and_interpolate()

de_Zwart_wages = get_de_Zwart_wages()

allen[, realWageAllenBasket := nominalWages * 360 / (hh_size * pricesAllenBasket)
    ][, realWageDeZwartBasket := nominalWages * 360 / (hh_size * pricesDeZwartBasket)
    ]

allen[de_Zwart_prices, on = .(year, region), deZwartPrices := i.prices
    ][region == 'south', deZwartPrices := pricesDeZwartBasket / grams_of_silver_in_a_rupee
    ][, realWageDeZwartPrices := nominalWages * 360 / (deZwartPrices * grams_of_silver_in_a_rupee * hh_size)
    ]



myColors = viridis(n = 7)[c(1,3,6,7)]
myColors[4] = '#EDC924FF'
names(myColors) = c('south', 'west', 'east', 'north')
colScale = scale_colour_manual(name = 'region', values = myColors)




ggplot(data = allen, aes(x = year, color = region)) +
  geom_point(aes(y = realWageAllenBasket), alpha = 0.6) +
  geom_smooth(aes(y = realWageAllenBasket), se = F) +
  geom_point(aes(y = realWageDeZwartPrices), alpha = 0.6) +
  geom_smooth(aes(y = realWageDeZwartPrices), se = F) +
  theme_cowplot() +
  colScale +
  labs(title = 'Welfare Ratio', subtitle = '(Real Wage / Subsistence Wage)', y = NULL, x = 'Year') +
  theme(plot.title = element_text(hjust = 0))






setkey(de_Zwart_prices, region, year)
de_Zwart_prices[, prices_MA3 := MA_with_partial_window(de_Zwart_prices, 'prices', 3, 'region')]
de_Zwart_prices[, prices_MA5 := MA_with_partial_window(de_Zwart_prices, 'prices', 5, 'region')]


allen_to_add = allen[!is.na(nominalWages) & ((region == 'west' & year >= 1824) | (region == 'north' & year >= 1873) |
                                             (region == 'east' & year >= 1873) | region == 'south'),
                     .(region, year, dwage = nominalWages / 10.78,
                       southPriceIndex = fifelse(region == 'south', prices / 10.78, NA_real_),
                       data_creator = 'allenstuder')]


setkey(allen_to_add, region, year)
allen_to_add[, southPriceIndex_MA3 := MA_with_partial_window(allen_to_add, 'southPriceIndex', 3, 'region')]
allen_to_add[, southPriceIndex_MA5 := MA_with_partial_window(allen_to_add, 'southPriceIndex', 5, 'region')]

withAllen = rbind(de_Zwart_wages, allen_to_add,
                  fill = T)
withAllen[de_Zwart_prices, on = .(region, year), `:=`(prices = prices, prices_MA3 = prices_MA3, prices_MA5 = prices_MA5)]
withAllen[region == 'south', `:=`(prices = southPriceIndex, prices_MA3 = southPriceIndex_MA3, prices_MA5 = southPriceIndex_MA5)]
withAllen[, realWage360 := dwage * 360 / (prices_MA5 * hh_size)]

withAllen[realWage360 > 0, logRealWage := log(realWage360)]

# Put everything in urban unskilled terms

withAllen[, can_calculate_skill_premium := uniqueN(hisclass) >= 2, .(city2, decade)]
withAllen[, id := .GRP, .(city2, decade)]
withAllen[, hisclassFactor := relevel(factor(hisclass), ref = '11')]
skill_premium = coef(lm(logRealWage ~ hisclassFactor + factor(id), data = withAllen[can_calculate_skill_premium == TRUE])
                   )[c('hisclassFactor6', 'hisclassFactor9', 'hisclassFactor12')]
withAllen[, skillAdjustedLogRealWage := logRealWage - skill_premium['hisclassFactor6'] * (hisclass == 6 & !is.na(hisclass))
                                                    - skill_premium['hisclassFactor9'] * (hisclass == 9 & !is.na(hisclass))
                                                    - skill_premium['hisclassFactor12'] * (hisclass == 12 & !is.na(hisclass))]


withAllen[, can_calculate_rural_premium := uniqueN(rural) >= 2, .(state, decade)]
withAllen[, id2 := .GRP, .(state, decade)]
rural_premium = coef(lm(skillAdjustedLogRealWage ~ factor(rural) + factor(id2), data = withAllen[can_calculate_rural_premium == TRUE])
                   )[c('factor(rural)1')]
withAllen[, AdjustedLogRealWage := skillAdjustedLogRealWage - rural_premium['factor(rural)1'] * (rural == 1 & !is.na(rural))]
withAllen[, adjustedRealWage := exp(AdjustedLogRealWage)]

withAllen[, id2 := paste0(region, decade)]
summary(lm(logRealWage ~ factor(id2), data = withAllen))

source_year_bins = withAllen[, .(realWage = exp(mean(AdjustedLogRealWage))), .(data_creator, year, region)]
ggplot(data = source_year_bins, aes(x = year, y = realWage, color = region)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = F) +
  theme_cowplot() +
  colScale +
  labs(title = 'Welfare Ratio', subtitle = '(Real Wage / Subsistence Wage)', y = NULL, x = 'Year') +
  theme(plot.title = element_text(hjust = 0))



decade_bins_with_each_source_year_binned = source_year_bins[, decade := year - year %% 10
                                                          ][, .(realWage = exp(mean(log(realWage)))), .(decade, region)]

ggplot(data = decade_bins_with_each_source_year_binned, aes(x = decade, y = realWage, color = region)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = F) +
  theme_cowplot() +
  colScale +
  labs(title = 'Welfare Ratio', subtitle = '(Real Wage / Subsistence Wage)', y = NULL, x = 'Year') +
  theme(plot.title = element_text(hjust = 0))






















































de_Zwart_prices_decadal =
  de_Zwart_prices[, decade := year - year %% 10
                ][, .(prices = exp(NaNtoNA(mean(log(prices), na.rm = TRUE)))),
                  .(decade, region)]


de_Zwart_decadal_wages_noheader =
  read_excel_dt('deZwartLucassen/6.real_wage_calculations.xlsx',
                'unskilled_stata',
                col_names = FALSE)


de_Zwart_decadal_wages =
  read_excel_dt('deZwartLucassen/6.real_wage_calculations.xlsx',
                'unskilled_stata',
                col_names = make_clean_names(paste(de_Zwart_decadal_wages_noheader[1],
                                                   de_Zwart_decadal_wages_noheader[2],
                                                   sep = '_')),
                skip = 2
               ) %>%
  .[, .SD, .SDcols = c(1, grep('_dwage', names(.)))] %>%
  setnames(., c('year', gsub('_dwage', '', names(.)[2:length(names(.))])))

locations_to_regions =
  data.table(location = c('bengal', 'bihar', 'agra',  'allahabad', 'delhi', 'gujarat'),
             region   = c('east',   'east',  'north', 'north',     'north', 'west'))

de_Zwart_decade_averages =
  melt(de_Zwart_decadal_wages, id.vars = 'year', variable.name = 'location', value.name = 'nominal_wage_Rs'
     )[location != 'n_india'
     ][locations_to_regions, on = .(location)
     ][, regional_average_nominal_wage_Rs := exp(NaNtoNA(mean(log(nominal_wage_Rs), na.rm = TRUE))), .(region, year)
     ][de_Zwart_prices_decadal, on = .(year = decade, region)
     ][, real_wage_360 := nominal_wage_Rs * 360 / (prices * hh_size)
     ][, regional_average_real_wage_360 := regional_average_nominal_wage_Rs * 360 / (prices * hh_size)][]

all_together_decade_averages =
  allen[, decade := year - year %% 10
      ][, .(realWage = exp(NaNtoNA(mean(log(realWage), na.rm = TRUE))),
            prices = exp(NaNtoNA(mean(log(prices), na.rm = TRUE))),
            nominalWages = exp(NaNtoNA(mean(log(nominalWages), na.rm = TRUE))),
            realWageDeZwartPrices = exp(NaNtoNA(mean(log(realWageDeZwartPrices), na.rm = TRUE))),
            realWageDeZwartPricesWithWestImputed = exp(NaNtoNA(mean(log(realWageDeZwartPricesWithWestImputed), na.rm = TRUE)))),
        .(decade, region)]

all_together_decade_averages[unique(de_Zwart_decade_averages[, .(decade = year,
                                                                 region,
                                                                 deZwartRealWage = regional_average_real_wage_360,
                                                                 deZwartNominalWage = regional_average_nominal_wage_Rs)]),
                               on = .(decade, region),
                               `:=`(deZwartRealWage = i.deZwartRealWage, deZwartNominalWage = i.deZwartNominalWage)]

all_together_decade_averages[, deZwartWageAllenPrices := deZwartNominalWage * grams_of_silver_in_a_rupee * 360 / (prices * hh_size)]

all_together_decade_averages[, best := fifelse(region %in% c('north', 'east'), deZwartRealWage, realWageDeZwartPricesWithWestImputed)]

ggplot(data = all_together_decade_averages, aes(x = decade)) +
  geom_point(aes(y = realWage, color = region)) +
  geom_smooth(aes(y = realWage, color = region), se = FALSE)

ggplot(data = all_together_decade_averages, aes(x = decade)) +
  geom_point(aes(y = realWageDeZwartPrices, color = region)) +
  geom_smooth(aes(y = realWageDeZwartPrices, color = region), se = FALSE)

ggplot(data = all_together_decade_averages, aes(x = decade)) +
  geom_point(aes(y = realWageDeZwartPricesWithWestImputed, color = region)) +
  geom_smooth(aes(y = realWageDeZwartPricesWithWestImputed, color = region), se = FALSE)

ggplot(data = all_together_decade_averages, aes(x = decade)) +
  geom_point(aes(y = deZwartWageAllenPrices, color = region)) +
  geom_smooth(aes(y = deZwartWageAllenPrices, color = region), se = FALSE)

ggplot(data = all_together_decade_averages, aes(x = decade)) +
  geom_point(aes(y = deZwartRealWage, color = region)) +
  geom_smooth(aes(y = deZwartRealWage, color = region), se = FALSE)

ggplot(data = all_together_decade_averages, aes(x = decade)) +
  geom_point(aes(y = best, color = region)) +
  geom_smooth(aes(y = best, color = region), se = FALSE)

