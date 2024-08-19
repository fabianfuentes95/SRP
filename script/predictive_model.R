rm(list=ls())
unlink("cache", recursive = TRUE)

# se cargan paquetes necesarios
if (!require(pacman)) install.packages('pacman') 
library(pacman) ; p_load('tidyverse','data.table', 'ggplot2','WDI', 'httr', 'olsrr',
                         'jsonlite', 'corrplot', 'pheatmap', 'caret', 'stargazer', 'cv','randomForest')

# se establece el directorio de trabajo
setwd('/Users/fabianfuentes/Library/CloudStorage/OneDrive-InstitutoTecnologicoydeEstudiosSuperioresdeMonterrey/2024/SRP')

# cargar y preparar los datos
df <- read.csv('data/wbccdb.csv') %>% 
  filter(country.value!='World') %>%  # cargamos los datos de todos los países
  as.data.table()


# partimos con un análisis de correlaciones entre las variables seleccionadas

controls <- c(
          'EN.ATM.CO2E.KT','EN.ATM.CO2E.PC','EN.ATM.CO2E.PP.GD','NY.GDP.MKTP.KD',
          'NY.GDP.MKTP.KD.ZG','NY.GDP.PCAP.KD','NY.GDP.PCAP.KD.ZG',
          'SP.POP.TOTL','SP.POP.GROW','SP.URB.TOTL.IN.ZS','SP.URB.GROW','SP.RUR.TOTL.ZG','EG.USE.COMM.GD.PP.KD',
          'EG.ELC.RNWX.ZS', 'EG.ELC.RNEW.ZS','EG.ELC.NUCL.ZS','EG.ELC.FOSL.ZS','TX.VAL.FUEL.ZS.UN', 'EN.URB.MCTY.TL.ZS',
          'EN.URB.LCTY.UR.ZS','SE.PRM.ENRL.TC.ZS','SE.SEC.ENRL.TC.ZS', 'EG.USE.PCAP.KG.OE', 'EG.USE.ELEC.KH.PC',
          'SE.PRM.CMPT.ZS','SE.SEC.CMPT.LO.ZS','SI.POV.DDAY','SI.POV.LMIC','SI.POV.UMIC',
          'SL.TLF.CACT.ZS','SL.EMP.TOTL.SP.ZS','SP.DYN.LE00.IN','SH.DYN.MORT',
          'SH.IMM.MEAS','NE.GDI.TOTL.ZS','SH.STA.SMSS.ZS','SH.H2O.BASW.ZS','NY.GNP.PCAP.CD' 
          )

# realizamos las correlaciones para las variables que seleccionamos  
df_scaled <- df[, ..controls]
df_scaled <- as.data.frame(scale(df_scaled))

cor_matrix <- cor(df_scaled, use = "complete.obs")
fwrite(cor_matrix, 'data/corr_matrix.csv')

# heatmap de la matriz de correlaciones
pheatmap(cor_matrix, 
         cluster_rows = FALSE,  # Desactivar clustering en filas
         cluster_cols = FALSE,  # Desactivar clustering en columnas
         color = colorRampPalette(c('#f03b20', 'white', '#feb24c'))(100),
         display_numbers = TRUE,  # Mostrar valores de correlación en cada celda
         main = "Heatmap de la Matriz de Correlación")

# heatmap de la matriz de correlaciones incluyendo clusters de varaible
pheatmap(cor_matrix, 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "complete",  # Método de clustering
         color = colorRampPalette(c('#f03b20', 'white', '#feb24c'))(100),
         display_numbers = TRUE,  # Mostrar valores de correlación en cada celda
         main = "Correlograma con Clustering")

# se crea una base de datos con los valores de las varaibles estandarizados
data <- df[date>=1990 & date<=2020, c('date', 'country.value', ..controls), with = FALSE]

# Loop para estandarizar las variables 
for (var in controls) {
 data[, paste0(var) := scale(as.matrix(get(var)))]
}

data_std <- data

controls2 <- c(
  # GDP
  'NY.GDP.MKTP.KD',          # GDP (constant 2015 US$)
  'NY.GDP.PCAP.KD',          # GDP per capita (constant 2015 US$)
  
  # Population
  'SP.POP.TOTL',             # Total population
  'SP.POP.GROW',             # Population growth (annual %)
  'SP.URB.TOTL.IN.ZS',       # Urban population (% of total population)
  'SP.URB.GROW',             # Urban population growth (annual %)
  'SP.RUR.TOTL.ZG',           # Rural population growth (annual %)
  
  # Relevant socio-economic
  'SI.POV.DDAY',             # Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)
  'SI.POV.LMIC',             # Poverty headcount ratio at $3.20 a day (2011 PPP) (% of population)
  'SI.POV.UMIC'              # Poverty headcount ratio at $5.50 a day (2011 PPP) (% of population)
  )

# verificamos algunas métricas con "Stepwise Selection"
fm1 <-as.formula(paste("EN.ATM.CO2E.KT ~", paste(controls2, collapse = " + ")))
model <- lm(fm1, data = data_std, na.action = na.omit)
ols_step_forward_adj_r2(model, progress = TRUE)

# primer modelo con variables seleccionadas
fm1 <-as.formula(paste("EN.ATM.CO2E.KT ~", paste(controls2, collapse = " + ")))
m1 <- lm(fm1, data = data_std, na.action = na.omit)

# resultados de la regresión
stargazer(m1,  type = "text", title = "OLS model")

controls3 <- c('NY.GDP.MKTP.KD','SP.POP.TOTL','SI.POV.LMIC','SI.POV.DDAY', 'SP.URB.GROW', 'SI.POV.UMIC', 'SP.RUR.TOTL.ZG','SP.URB.TOTL.IN.ZS')
fm2 <-as.formula(paste("EN.ATM.CO2E.KT ~", paste(controls3, collapse = " + ")))
m2 <- lm(fm2, data = data_std, na.action = na.omit)

# resultados de la regresión
stargazer(m1, m2,  type = "text", title = "OLS model")

# se crean las bases de entrenamiento y prueba
data <- df[date>=1990 & date<=2020, c('date', 'country.value', 'EN.ATM.CO2E.KT',..controls2), with = FALSE]

set.seed(1995)
train_indices <- createDataPartition(data$EN.ATM.CO2E.KT, p = 0.8, list = FALSE)

train_data <- data[train_indices, ]
test_data  <- data[-train_indices, ]


# se entrena el modelo de regresión lineal
fm1 <-as.formula(paste("EN.ATM.CO2E.KT ~", paste(controls3, collapse = " + ")))

model <- lm(fm1, data = train_data, na.action = na.omit)
summary(model)

# evaluar el rendimiento del modelo
predictions <- predict(model, newdata = test_data)
rmse <- sqrt(mean((test_data$EN.ATM.CO2E.KT - predictions )^2, ,na.rm=T))
r2 <- summary(model)$r.squared

cat("RMSE: ", rmse, "\n")
cat("R²: ", r2, "\n")

# se simula un escenario con un incremento del 10% en el GDP
test_data_increased_gdp <- test_data
test_data_increased_gdp$NY.GDP.MKTP.KD <- test_data$NY.GDP.MKTP.KD * 1.10
new_predictions <- predict(model, newdata = test_data_increased_gdp)

# cambio porcentual en las emisiones
percentage_change <- ((new_predictions - predictions) / predictions) * 100
summary(percentage_change)

# resultados
results <- data.frame(Country = test_data$country, 
                      Year = test_data$date,
                      Original_Emissions = predictions, 
                      New_Emissions = new_predictions, 
                      Percentage_Change = percentage_change)
tail(results)

################################################################################
# Función para probar modelos no paramétricos
################################################################################
p_load('rpart','gbm')

# se filtra la base de datos para tener casas completos
data <- drop_na(data)

# se crea una función para iterar entre los modelos 
evaluate_models <- function(data, target_var, predictors) {
  
  # train y test
  set.seed(123)
  train_indices <- createDataPartition(data[[target_var]], p = 0.8, list = FALSE)
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # lista para almacenar los resultados
  results <- list()
  
  # lista de modelos a probar
  models <- list(
    "RandomForest" = randomForest,
    "DecisionTree" = rpart,
    "GBM" = gbm
  )
  
  # entrenar y evaluar cada modelo
  for (model_name in names(models)) {
    cat("Training model:", model_name, "\n")
    
    # definir el modelo
    if (model_name == "RandomForest") {
      model <- models[[model_name]](as.formula(paste(target_var, "~", paste(predictors, collapse = "+"))), 
                                    data = train_data, ntree = 500)
    } else if (model_name == "DecisionTree") {
      model <- models[[model_name]](as.formula(paste(target_var, "~", paste(predictors, collapse = "+"))), 
                                    data = train_data, method = "anova")
    } else if (model_name == "GBM") {
      model <- models[[model_name]](as.formula(paste(target_var, "~", paste(predictors, collapse = "+"))), 
                                    data = train_data, distribution = "gaussian", n.trees = 100, interaction.depth = 3)
    }
    
    # dealizar predicciones en el conjunto de prueba
    if (model_name == "GBM") {
      predictions <- predict(model, newdata = test_data, n.trees = 100)
    } else {
      predictions <- predict(model, newdata = test_data)
    }
    
    # dalcular las métricas de evaluación
    rmse <- sqrt(mean((test_data[[target_var]] - predictions)^2))
    r2 <- cor(test_data[[target_var]], predictions)^2
    
    # dimular un escenario con un incremento del 10% en el GDP
    test_data_increased_gdp <- test_data
    test_data_increased_gdp$NY.GDP.MKTP.KD <- test_data$NY.GDP.MKTP.KD * 1.10
    
    if (model_name == "GBM") {
      new_predictions <- predict(model, newdata = test_data_increased_gdp, n.trees = 100)
    } else {
      new_predictions <- predict(model, newdata = test_data_increased_gdp)
    }
    
    # calcular el cambio porcentual en las emisiones
    percentage_change <- ((new_predictions - predictions) / predictions) * 100
    
    # guardar los resultados
    results[[model_name]] <- list(
      Model = model_name,
      RMSE = rmse,
      R2 = r2,
      Predictions = predictions,
      New_Predictions = new_predictions,
      Percentage_Change = percentage_change
    )
  }
  
  # convertir los resultados en un dataframe para la comparación
  summary_results <- do.call(rbind, lapply(results, function(res) {
    data.frame(Model = res$Model, RMSE = res$RMSE, R2 = res$R2)
  }))
  
  # resultados
  list(
    Summary = summary_results,
    Detailed = results
  )
}

# la función está diseñada para poder modificar las variables, en este caso usaremos los mismos
# cotroles que usamos para el modelo lineal

predictors <- controls2
target_var <- "EN.ATM.CO2E.KT"

# Evaluar los modelos
model_results <- evaluate_models(data, target_var, predictors)
print(model_results)

save.image('data/preditvive_model.RData')