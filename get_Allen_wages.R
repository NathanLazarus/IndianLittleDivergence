read_Allen = function(region, hh_size) {
  read_excel_dt('AllenStuderIMessedWith.xlsx', region, skip = 13) %>%
    setnames(., make_clean_names(names(.)))
}

get_Allen_wages = function(hh_size) {

  alleneast = read_Allen('east', hh_size)
  allenwest = read_Allen('west', hh_size)
  allensouth = read_Allen('south', hh_size)
  allennorth = read_Allen('north', hh_size)

  all_years = data.table(year = sort(unique(c(alleneast$year, allenwest$year, allensouth$year, allennorth$year))))

  allen = rbind(
    alleneast[!is.na(year), .(year, region = 'east', pricesAllenBasket = cpi_ind,
                              pricesDeZwartBasket = cpi_de_zwart, nominalWages = unskilledwage)
            ][all_years, on = .(year)],
    allenwest[!is.na(year), .(year, region = 'west', pricesAllenBasket = cpi_ind,
                              pricesDeZwartBasket = cpi_de_zwart, nominalWages = unskilledwage)
            ][all_years, on = .(year)],
    allensouth[!is.na(year), .(year, region = 'south', pricesAllenBasket = cpi_ind,
                               pricesDeZwartBasket = cpi_de_zwart, nominalWages = unskilledwage)
             ][all_years, on = .(year)],
    allennorth[!is.na(year), .(year, region = 'north', pricesAllenBasket = cpi_ind,
                               pricesDeZwartBasket = cpi_de_zwart, nominalWages = unskilledwage)
             ][all_years, on = .(year)]
    )

  return(allen)
}