library("mxmaps")
View(df_mxmunicipio_2020)
df_mxstate_2020$value <- df_mxstate_2020$pop
mxstate_choropleth(df_mxstate_2020,
                   title = "Total population, by state") 

df_mxmunicipio_2020$value <-  df_mxmunicipio_2020$indigenous_language / 
  df_mxmunicipio_2020$pop * 100
mxmunicipio_choropleth(df_mxmunicipio_2020, num_colors = 1,
                       zoom = subset(df_mxmunicipio_2020, state_name %in% 
                                       c("Baja California",
                                         "Baja California Sur"))$region,
                       title = "Percentage of the population that speaks\nan indigenous language",
                       legend = "%") 
