library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)
library(reshape2)
library(dplyr)
library(mxmaps)
library(leaflet) # for colorNumeric
library(scales) # needed for comma
library(readxl)

# Setup -------------------------------------------------------------------

#! 01 - Estimación de la Demanda Real del Sistema - Por Balance
df <- read.csv("input/DemandaRealBalance.csv", skip = 8)
drbSistema <- df[which.max(df[,8]),1]
drbArea <- df[which.max(df[,8]),2]
drbHora <- df[which.max(df[,8]),3]
drbNum <- df[which.max(df[,8]),8]

#! 02 - Precios Marginales por Hora
df <- read.csv("input/PMLHorarioMDA.csv", skip = 4)
pml.max.value <- max(df[,c(2,3,4)])
index <- arrayInd(which(df == pml.max.value), dim(df))
pmlArea <- colnames(df)[index[1,2]]
pmlHora <- df$HORA[index[1,1]]

#! 03 - (Plot) Energía Generada por Tipo de Tecnología
df <- read.csv("input/GeneracionLiquidada_SENenero2025.csv", skip = 7)
dat <- df[,2:14]
dat$Fecha <- paste0(dat$Dia," ",dat$Hora)
dat$Fecha <- as.POSIXct(dat$Fecha, format = "%d/%m/%Y %H")
dat$Dia <- NULL
dat$Hora <- NULL
dat_melted <- melt(dat, id.vars = "Fecha")
colnames(dat_melted)[2] <- "Tecnologia"
colnames(dat_melted)[3] <- "Generacion"

plotly_genTecno <- plot_ly(
    data = dat_melted, 
    x = ~Fecha, 
    y = ~Generacion, 
    color = ~Tecnologia,
    type = 'scatter', 
    mode = 'lines'
    # line = list(color = 'blue')
  ) %>%
    layout(
      title = "Energía Generada por Tipo de Tecnología",
      xaxis = list(title = "Tiempo"),
      yaxis = list(title = "MWh"),
      hovermode = "x unified"
    )

#! 04 - (Plot) PML por dia del SIN (MWh)
df1 <- read.csv("input/PreciosMargLocales-SIN-MDA-MesEnero-01.csv", skip = 7)
df2 <- read.csv("input/PreciosMargLocales-SIN-MDA-MesEnero-02.csv", skip = 7)
df.PML.mes <- rbind(df1,df2)
colnames(df.PML.mes)[1] <- "Dia"
colnames(df.PML.mes)[4] <- "precioMarginalLocal"
colnames(df.PML.mes)[5] <- "componenteDeEnergia"
colnames(df.PML.mes)[6] <- "componenteDePerdidas"
colnames(df.PML.mes)[7] <- "componenteDeCongestion"
df.PML.mes$Fecha <- paste0(df.PML.mes$Dia," ",df.PML.mes$Hora)
df.PML.mes$Fecha <- as.POSIXct(df.PML.mes$Fecha, format = "%Y-%m-%d %H")
df.PML.mes$Dia <- NULL
df.PML.mes$Hora <- NULL
pml_grouped <- aggregate(precioMarginalLocal ~ Fecha, data = df.PML.mes, FUN = mean)

plotly_PML.SIN <- plot_ly(
  data = pml_grouped, 
  x = ~Fecha, 
  y = ~precioMarginalLocal,
  type = 'scatter', 
  mode = 'lines'
  # line = list(color = 'blue')
) %>%
  layout(
    title = "PML por dia del SIN (MWh)",
    xaxis = list(title = "Tiempo",
                 rangeselector = list(
                   buttons = list(
                     list(
                       count = 2,
                       label = "2 Days",
                       step = "day",
                       stepmode = "backward"
                     ),
                     list(
                       count = 7,
                       label = "1 Week",
                       step = "day",
                       stepmode = "backward"
                     ),
                     list(
                       step = "all",  # Show full range
                       label = "All"
                     )
                   )
                 ),
                 rangeslider = list(visible = TRUE)  # Add a range slider
              ),
    yaxis = list(title = "MWh"),
    hovermode = "x unified"
  )

#! 05 - (Plot) Demanda vs PML
my_list <- seq(as.Date("2025/01/01"), as.Date("2025/01/31"), by = "day")
df.DRB <- lapply(my_list, function(x) {
  folder_path <- "input/demandaRealBalanceEnero"
  re.fn <- paste0("^Demanda Real Balance_0_.*",x)
  files <- list.files(path = folder_path, pattern = re.fn, full.names = TRUE)
  stopifnot(length(files)==1)
  df.temp <- read.csv(files[1], skip = 8)
  df.temp$dia <- x
  return(df.temp)
}) %>%
  bind_rows()
df.DRB$X.Generacion..MWh. <- NULL
df.DRB$X.Importacion.Total..MWh. <- NULL
df.DRB$X.Exportacion.Total..MWh. <- NULL
df.DRB$X.Intercambio.neto.entre.Gerencias..MWh. <- NULL
colnames(df.DRB)[2] <- "Area"
colnames(df.DRB)[3] <- "Hora"
colnames(df.DRB)[4] <- "Estimacion.de.Demanda.por.Balance"
df.DRB$Fecha <- paste0(df.DRB$dia," ",df.DRB$Hora)
df.DRB$Fecha <- as.POSIXct(df.DRB$Fecha, format = "%Y-%m-%d %H")
df.DRB$dia <- NULL
df.DRB$Hora <- NULL
drb_grouped <- aggregate(Estimacion.de.Demanda.por.Balance ~ Fecha, 
                         data = df.DRB, FUN = mean)
merged_df <- merge(drb_grouped, pml_grouped, by = "Fecha")

plotly_drb.pml <- plot_ly(
  data = merged_df, 
  x = ~Estimacion.de.Demanda.por.Balance, 
  y = ~precioMarginalLocal, 
  type = 'scatter', 
  mode = 'markers'
  # line = list(color = 'blue')
) %>%
  layout(
    title = "PML vs Demanda",
    xaxis = list(title = "Estimacion.de.Demanda.por.Balance (MWh)"),
    yaxis = list(title = "PML (MWh)"),
    hovermode = "x unified"
  )


#! 06 - (Plot) PML Máximo Enero 2025
df1 <- read.csv("input/PreciosMargLocales-BCA-ENERO-1.csv", skip = 7)
df2 <- read.csv("input/PreciosMargLocales-BCA-ENERO-2.csv", skip = 7)
df.bca.PML.mes <- rbind(df1,df2)
colnames(df.bca.PML.mes)[1] <- "Dia"
colnames(df.bca.PML.mes)[4] <- "precioMarginalLocal"
colnames(df.bca.PML.mes)[5] <- "componenteDeEnergia"
colnames(df.bca.PML.mes)[6] <- "componenteDePerdidas"
colnames(df.bca.PML.mes)[7] <- "componenteDeCongestion"
df.bca.PML.mes$Fecha <- paste0(df.bca.PML.mes$Dia," ",df.bca.PML.mes$Hora)
df.bca.PML.mes$Fecha <- as.POSIXct(df.bca.PML.mes$Fecha, format = "%Y-%m-%d %H")
df.bca.PML.mes$Dia <- NULL
df.bca.PML.mes$Hora <- NULL

df1 <- read.csv("input/PreciosMargLocales-BCS-ENERO-1.csv", skip = 7)
df2 <- read.csv("input/PreciosMargLocales-BCS-ENERO-2.csv", skip = 7)
df.bcs.PML.mes <- rbind(df1,df2)
colnames(df.bcs.PML.mes)[1] <- "Dia"
colnames(df.bcs.PML.mes)[4] <- "precioMarginalLocal"
colnames(df.bcs.PML.mes)[5] <- "componenteDeEnergia"
colnames(df.bcs.PML.mes)[6] <- "componenteDePerdidas"
colnames(df.bcs.PML.mes)[7] <- "componenteDeCongestion"
df.bcs.PML.mes$Fecha <- paste0(df.bcs.PML.mes$Dia," ",df.bcs.PML.mes$Hora)
df.bcs.PML.mes$Fecha <- as.POSIXct(df.bcs.PML.mes$Fecha, format = "%Y-%m-%d %H")
df.bcs.PML.mes$Dia <- NULL
df.bcs.PML.mes$Hora <- NULL

df.bc.PML.mes <- rbind(df.bca.PML.mes, df.bcs.PML.mes)

df_agg <- df.bc.PML.mes %>%
  group_by(Clave.del.nodo) %>%
  slice_max(precioMarginalLocal, n = 1) %>%
  ungroup()
stopifnot(duplicated(df_agg$Clave.del.nodo)==0)
df <- read_excel("input/catalogoNodosP-BCAyBCS.xlsx", skip = 1, col_names = TRUE)
stopifnot(duplicated(df$CLAVE)==0)
merged_df <- merge(df_agg, df, by.x = "Clave.del.nodo", by.y = "CLAVE")
dim(merged_df)
stopifnot(duplicated(merged_df$Clave.del.nodo)==0)
colnames(merged_df)[colnames(merged_df) == "ZONA DE CARGA"] <- "zonaDeCarga"
df_agg <- merged_df %>%
  group_by(zonaDeCarga) %>%
  slice_max(precioMarginalLocal, n = 1) %>%
  ungroup()

df_mxstate_2020$value <- df_mxstate_2020$afromexican / df_mxstate_2020$pop
pal <- colorNumeric("Blues", domain = df_mxstate_2020$value)

df_mxmunicipio$value <- df_mxmunicipio$indigenous /df_mxmunicipio$pop
magma <- c("#000004FF", "#1D1146FF", "#50127CFF", "#822681FF",
           "#B63779FF", "#E65163FF", "#FB8761FF", "#FEC387FF", "#FCFDBFFF")
pal <- colorNumeric(magma, domain = df_mxmunicipio$value)
mxleaflet <- mxmunicipio_leaflet(df_mxmunicipio,
                    pal,
                    ~ pal(value),
                    ~ sprintf("State: %s<br/>Municipio : %s<br/>Value: %s%%",
                              state_name, municipio_name, round(value * 100, 1))) %>%
  addLegend(position = "bottomright", pal = pal,
            values = df_mxmunicipio$value) %>%
  addProviderTiles("CartoDB.Positron")



# UI ----------------------------------------------------------------------

ui <- page_fluid(
  title = "Evaluación técnica",
  theme = bs_theme(bootswatch = "flatly"),  # Use a Bootswatch theme
  titlePanel("Subsecretaría de electricidad"),
  
  # Layout with two full-width cards
  layout_column_wrap(
    width = 1/2,  # Arrange two cards per row
    value_box(
      title = "Máximo de la Estimación de la demanda Real del Sistema",
      value = paste0(formatC(drbNum, digits = 2, format = "f"),"  ","$/MWh"),
      showcase = bs_icon("bar-chart"),
      theme = "purple",
      p(paste0("Sistema:  ",drbSistema)),
      p(paste0("Área:  ",drbArea)),
      p(paste0("Hora:  ",drbHora))
    ),
    value_box(
      title = "Máximo de los Precios Marginales por Hora",
      value = paste0(formatC(pml.max.value, digits = 2, format = "f"),"  ","MWh"),
      showcase = bs_icon("bar-chart"),
      theme = "purple",
      p(paste0("Área:  ",pmlArea)),
      p(paste0("Hora:  ",pmlHora))
    ),
  ),
  # Layout with two full-width cards
  layout_column_wrap(
    card(
      height = 400,
      full_screen = TRUE,
      # card_header("A filling plot"),
      card_body(plotly_genTecno)
    )
  ),
  layout_column_wrap(
    card(
      height = 400,
      full_screen = TRUE,
      card_header("PML por dia del SIN (MWh)"),
      card_body(plotly_PML.SIN)
    )
  ),
  layout_column_wrap(
    card(
      height = 500,
      full_screen = TRUE,
      card_body(plotly_drb.pml)
    )
  ),
  layout_column_wrap(
    card(
      height = 500,
      full_screen = TRUE,
      card_body(mxleaflet)
    )
  ),
)

# Server

server <- function(input, output) {
  
}


# Shiny App ---------------------------------------------------------------

# shinyApp(ui, function(input, output) {})
shinyApp(ui, server)

