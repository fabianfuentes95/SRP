rm(list=ls())
unlink("cache", recursive = TRUE)

# se cargan paquetes necesarios
if (!require(pacman)) install.packages('pacman') 
library(pacman) ; p_load('tidyverse','data.table', 'ggplot2','WDI', 'httr',
                         'jsonlite', 'corrplot', 'pheatmap')

# se establece el directorio de trabajo
setwd('/Users/fabianfuentes/Library/CloudStorage/OneDrive-InstitutoTecnologicoydeEstudiosSuperioresdeMonterrey/2024/SRP')

# para poder descargar la información del World Bank, podemos utilizar el paquete
# 'WDI' (https://cran.r-project.org/web/packages/WDI/WDI.pdf), que permite buscar 
# los datos que almacena en la API del banco

# buscamos los indicadores que nos interesan y los podemos guardar para su consulta 
co2_vars <- WDIsearch('CO2 emissions')
co2_vars<- as.data.frame(co2_vars)
fwrite(co2_vars, 'data/co2_vars.csv')

head(co2_vars)

gdp_vars <- WDIsearch('GDP')
gdp_vars<- as.data.frame(gdp_vars)
fwrite(gdp_vars, 'data/gdpvars.csv')

head(gdp_vars)

################################################################################
# Consulta de los indicadores a través de la API
################################################################################

# información obtenida de https://microdata.worldbank.org/api-documentation/catalog/index.html

# definimos los parámetros para la consulta
country  <- 'MEX'                                # país elegido
year_str <- 1990                                 # inicio de la serie
year_end <- 2020                                 # final de la serie
pages <- (year_end - year_str)                   # número de periodos esperados

# definimos el resto de parámetros 
params <- list(format = 'json', date = paste0(year_str,':', year_end), per_page = pages)

# se realiza la consulta para 'CO2 emissions'
indicator <- 'EN.ATM.CO2E.KT'                      

# se construye la url para la consulta con los parámetros antes definidos (país e indicador)
url <- paste0('https://api.worldbank.org/v2/country/', country,'/indicator/', indicator)

# solicitud GET a la API del worldbank
response <- GET(url, query = params, timeout(60))

# se desanida la consulta y de convierte en una base de datos
data <- fromJSON(content(response, 'text', encoding = 'UTF-8'))[[2]]

# el resultado es la consulta que esperamos sobre el indicador para México
head(data)

# después de analizar los datos disponibles se eligen los que se descargarán

indicators <- c(
  # emissions
  'EN.ATM.CO2E.KT',          # CO2 emissions (kt)
  'CC.CO2.EMSE.BF',	         # CO2 emissions by sector (Mt CO2 eq) - Bunker Fuels
  'CC.CO2.EMSE.BL',	         # CO2 emissions by sector (Mt CO2 eq) - Building
  'CC.CO2.EMSE.EL',		       # CO2 emissions by sector (Mt CO2 eq) - Total excluding LUCF
  'CC.CO2.EMSE.EN',		       # CO2 emissions by sector (Mt CO2 eq) - Energy
  'CC.CO2.EMSE.FE',		       # CO2 emissions by sector (Mt CO2 eq) - Fugitive Emissions
  'CC.CO2.EMSE.IL',		       # CO2 emissions by sector (Mt CO2 eq) - Total including LUCF
  'CC.CO2.EMSE.IP',	         # CO2 emissions by sector (Mt CO2 eq) - Industrial Processes
  'CC.CO2.EMSE.LU',		       # CO2 emissions by sector (Mt CO2 eq) - Land-Use Change and Forestry
  'CC.CO2.EMSE.MC',	         # CO2 emissions by sector (Mt CO2 eq) - Manufacturing/Construction
  'CC.CO2.EMSE.TR',		       # CO2 emissions by sector (Mt CO2 eq) - Transportation
  'EN.ATM.CO2E.PC',          # CO2 emissions (metric tons per capita)
  'EN.ATM.CO2E.PP.GD',       # CO2 emissions (kg per PPP $ of GDP)
  
  # GDP
  'NY.GDP.MKTP.KD',          # GDP (constant 2015 US$)
  'NY.GDP.MKTP.KD.ZG',       # GDP growth (annual %)
  'NY.GDP.PCAP.KD',          # GDP per capita (constant 2015 US$)
  'NY.GDP.PCAP.KD.ZG',       # GDP per capita growth (annual %)
  
  # Population
  'SP.POP.TOTL',             # Total population
  'SP.POP.GROW',             # Population growth (annual %)
  'SP.URB.TOTL.IN.ZS',       # Urban population (% of total population)
  'SP.URB.GROW',             # Urban population growth (annual %)
  'SP.RUR.TOTL.ZG',          # Rural population growth (annual %)
  
  # Energy consumption
  'EG.USE.PCAP.KG.OE',       # Energy use (kg of oil equivalent per capita)
  'EG.USE.COMM.GD.PP.KD',    # Energy use (kg of oil equivalent) per $1,000 GDP (constant 2017 PPP)
  'EG.USE.ELEC.KH.PC',       # Electric power consumption (kWh per capita)
  'EG.ELC.ACCS.ZS',          # Access to electricity (% of population)
  'EG.ELC.RNWX.ZS',          # Renewable energy consumption (% of total final energy consumption)
  'EG.ELC.RNEW.ZS',          # Renewable electricity output (% of total electricity output)
  'EG.ELC.NUCL.ZS',          # Electricity production from nuclear sources (% of total)
  'EG.ELC.FOSL.ZS',          # Electricity production from oil, gas and coal sources (% of total)
  'TX.VAL.FUEL.ZS.UN',       # Fuel exports (% of merchandise exports)
  
  # Urbanization rate
  'EN.URB.MCTY.TL.ZS',       # Population in the largest city (% of urban population)
  'EN.URB.LCTY.UR.ZS',       # Population in urban agglomerations of more than 1 million (% of total population)
  
  # Education level
  'SE.PRM.ENRL.TC.ZS',       # School enrollment, primary (% gross)
  'SE.SEC.ENRL.TC.ZS',       # School enrollment, secondary (% gross)
  'SE.TER.ENRL.TC.ZS',       # School enrollment, tertiary (% gross)
  'SE.PRM.CMPT.ZS',          # Primary completion rate, total (% of relevant age group)
  'SE.SEC.CMPT.LO.ZS',       # Lower secondary completion rate, total (% of relevant age group)
  
  # Relevant socio-economic
  'SI.POV.DDAY',             # Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)
  'SI.POV.LMIC',             # Poverty headcount ratio at $3.20 a day (2011 PPP) (% of population)
  'SI.POV.UMIC',             # Poverty headcount ratio at $5.50 a day (2011 PPP) (% of population)
  'SI.POV.GINI',             # GINI index (World Bank estimate)
  'SL.TLF.CACT.ZS',          # Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)
  'SL.EMP.TOTL.SP.ZS',       # Employment to population ratio, 15+, total (%) (modeled ILO estimate)
  'SP.DYN.LE00.IN',          # Life expectancy at birth, total (years)
  'SH.DYN.MORT',             # Mortality rate, under-5 (per 1,000 live births)
  'SH.IMM.MEAS',             # Immunization, measles (% of children ages 12-23 months)
  'NE.GDI.TOTL.ZS',          # Gross fixed capital formation (% of GDP)
  'SH.STA.SMSS.ZS',          # People using safely managed sanitation services (% of population)
  'SH.H2O.BASW.ZS',          # People using at least basic drinking water services (% of population)
  'NY.GNP.PCAP.CD'           # GNI per capita, Atlas method (current US$)
)

# se definen los países a consultar 
countries <- c('MEX', 'USA', 'CHN', 'IND', 'RUS','FRA','DEU','GBR', 'FIN','WLD') 

year_str <- 1980                                 # inicio de la serie
year_end <- 2020                                 # final de la serie
pages <- (year_end - year_str)                   # número de periodos esperados

# definimos el resto de parámetros 
params <- list(format = 'json', date = paste0(year_str,':', year_end), per_page = pages)

# Función para descargar los datos de manera masiva
data <- lapply(countries, function(country) {
  lapply(indicators, function(indicator) {
    
    
    # Construir la URL para la consulta con los parámetros definidos (país e indicador)
    url <- paste0('https://api.worldbank.org/v2/country/', country, '/indicator/', indicator)
    
    # Realizar la solicitud GET a la API del World Bank
    response <- GET(url, query = params, timeout(90))
    
    # Verificar el código de estado de la respuesta
    if (status_code(response) == 200) {
      # Desanidar la consulta y convertirla en un data.table
      data <- fromJSON(content(response, 'text', encoding = 'UTF-8'))[[2]]
      data <- as.data.table(data)
      data[, country.id:=country]
    } else {
      warning(paste0("Error en la consulta para el indicador: ", indicator, " en el país: ", country))
      data <- NULL
    }
    
    # Mostrar un mensaje de finalización para cada indicador
    print(paste0('Termina el indicador: ', indicator, ' para el país: ', country))
    
    return(data)
    
  }) %>% rbindlist(fill = TRUE) 
}) %>% rbindlist(fill = TRUE) 


head(data)

# ya que se tienen las series de datos seleccionados, se procede preprocesar la 
# información y determinar outliers y missings 

summary(data)
table(data$indicator.id, exclude = NULL)
nrow(data)

data <- data[!is.na(indicator.id)]

table(data$country.value)
table(data$country.id)
table(data$date)

# se cambia la estructura de la base de datos con un reshape
df_wide <- dcast(dplyr::select(data, date, country.value, country.id, indicator.id, value), 
                 date + country.value ~ indicator.id, value.var = 'value',
                 fun.aggregate = NULL) 
df_wide$date <- as.numeric(df_wide$date) 

# ahora se tiene una colunma para cada variable y en cada renglon un año del país
head(df_wide)

# para comenzar se analizan las principales variables 

# promedio GDP (constant 2015 million US$)
df_wide[, mean(NY.GDP.MKTP.KD, na.rm=T)/1000000, by=list(country.value)]

# promedio GDP per capita (constant 2015  US$)
df_wide[, mean(NY.GDP.PCAP.KD, na.rm=T), by=list(country.value)]

# promedio de emisiones de CO2 (KT)
df_wide[, mean(EN.ATM.CO2E.KT, na.rm=T), by=list(country.value )]

# ahora analizaremos la relación entre GDP y emisiones de CO2

# estandarizamos las varaibles para un manejo sin unidades distintas 
vars <- c('NY.GDP.MKTP.KD', 'NY.GDP.PCAP.KD', 
          'EN.ATM.CO2E.KT', 'EN.ATM.CO2E.PC')

# Loop para estandarizar las variables 
for (var in vars) {
  df_wide[, paste0(var, '_std') := scale(as.matrix(get(var)))]
}

head(df_wide[date >= 1990 & date <= 2020, 
             c('NY.GDP.MKTP.KD_std', 'NY.GDP.PCAP.KD_std', 
               'EN.ATM.CO2E.KT_std', 'EN.ATM.CO2E.PC_std')])

fwrite(df_wide, 'data/wbccdb.csv')