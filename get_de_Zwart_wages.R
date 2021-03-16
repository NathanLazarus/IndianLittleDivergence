# get_de_Zwart_wages.R

get_de_Zwart_wages = function() {
  de_Zwart_wages_raw = data.table(read_dta('deZwartLucassen/1north_india_wages.dta'))

  locations_to_regions_de_Zwart =
    data.table(location = c('Assam', 'Bengal', 'Bihar', 'Bootan', 'Meckley', 'Orissa',
                            'Ajmer', 'Agra', 'Allahabad', 'Awadh', 'Delhi', 'Lahore', 'Malwa',
                            'Gujarat', 'Berar',
                            'Golconda',
                            'Pegu'),
               region = c(rep('east', times = 6),
                          rep('north', times = 7),
                          rep('west', times = 2),
                          rep('south', times = 1),
                          rep(NA_character_, times = 1))
    )


  # Removing duplicates with Allen, a couple observations from Pegu in Myanmar,
  # and keeping only unskilled and low-skilled wages,
  # dropping those paid to "medium" and "high" skilled workers in de Zwart and Lucassen's classification
  # (which splits hisclass 6 deliberately).
  # Also dropping wages paid to Europeans by the East India companies and those paid to women
  de_Zwart_wages = de_Zwart_wages_raw[state != 'Golconda' & state != 'Pegu' &
                                        !(allenstuder == 1 & state == 'Gujarat' & year < 1800)
                                    ][skill %in% c(1, 2) & gender_age == 0 & european == 0] #unskilled and low-skilled

  de_Zwart_wages[locations_to_regions_de_Zwart, on = .(state = location), region := i.region]

  de_Zwart_wages[, data_creator := fifelse(allenstuder == 1, 'allenstuder',
                                           fifelse(broadberrygupta == 1, 'broadberrygupta',
                                                   fifelse(haider == 1, 'haider',
                                                           fifelse(nadri == 1, 'nadri',
                                                                   fifelse(voc == 1, 'voc',
                                                                           fifelse(mbp == 1, 'mbp',
                                                                                   fifelse(boo == 1, 'boo',
                                                                                           fifelse(hunter == 1, 'hunter',
                                                                                                   NA_character_))))))))]

  return(de_Zwart_wages)
}
