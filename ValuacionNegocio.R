# Analisis descriptivo
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)

# Analisis diagnostico de series de tiempo
library(tidyquant)
library(tsibble)
library(forecast)
library(prophet)
library(tseries)
library(fable)
library(purrr)
library(feasts)

# Prescripcion
library(scales)

# Recomendacion sobre portfolio de productos
library(GA)
library(plotly)

sales <- read.csv("sales.csv") |>
  mutate(
    Time = lubridate::hm(Time), # La hora no tiene segundos, solo hora y minutos
    Hour = hour(Time), # Columna auxiliar para calcular el periodo
    Date = lubridate::mdy(Date), # La fecha esta en formato mm-dd-yyy
    Period = case_when(
      Hour >= 3  & Hour < 6  ~ "Madrugada",
      Hour >= 6  & Hour < 10 ~ "Mañana",
      Hour >= 10 & Hour < 12 ~ "Media mañana",
      Hour >= 12 & Hour < 14 ~ "Media tarde",
      Hour >= 14 & Hour < 18 ~ "Tarde",
      Hour >= 18 & Hour < 22 ~ "Noche",
      Hour >= 22 | Hour < 3  ~ "Trasnoche", 
      TRUE ~ NA_character_
    )
  )

makeDescription <- function(sales){
  sales_by_hour <- sales |>
    count(Hour, sort = TRUE)
  
  sales_products <- sales |>
    group_by(Product.line) |>
    summarise(Ventas = sum(Quantity))
  
  sales_payment <- sales |>
    count(Payment, sort = TRUE)
  
  sales_period <- sales |>
    count(Period, sort = TRUE)
  
  sales_cities <- sales |>
    group_by(City) |>
    summarise(Facturacion = sum(Total)) |>
    arrange(desc(Facturacion))
  
  avg_fact_by_product <- sales |>
    group_by(Product.line) |>
    summarise(Average = mean(Total)) |>
    arrange(desc(Average))
  
  list(
    "PeriodoMayorVentas" = sales_period |> slice(1),
    "PeriodoMenorVentas" = sales_period |> slice(n()),
    "HoraMasVentas" = sales_by_hour |> slice(1),
    "HoraMenosVentas" = sales_by_hour |> slice(n()),
    "ProductoMayorVentas" = sales_products |> arrange(desc(Ventas)) |> slice(1),
    "ProductoMenorVentas" = sales_products |> arrange(Ventas) |> slice(1),
    "MetodoPagoMasUsado" = sales_payment |> slice(1),
    "MetodoPagoMenosUsado" = sales_payment |> slice(n()),
    "FacturacionPromedio" = mean(sales$Total),
    "RangoFacturacion" = range(sales$Total),
    "CiudadMayorFacturacion" = sales_cities |> slice(1),
    "CiudadMenorFacturacion" = sales_cities |> slice(n()),
    "CalificacionPromedio" = mean(sales$Rating),
    "FacturacionPromedioPorLineaProducto" = avg_fact_by_product
  )
}

buildForecastMetrics <- function(fc, ts_test) {
  ts_test |>
    inner_join(fc, by = join_by(ds)) |>
    rename(test = y.x, forecast = y.y)
}

buildForecastPlot <- function(mtrcs) {
  mtrcs |>
    ggplot(aes(x = ds, y = test)) +
    geom_line() +
    geom_line(aes(y = forecast, color = "red")) +
    labs(title = "Test vs Forecast")
}

buildForecastError <- function(mtrcs, ts_h) {
  err <- mtrcs |>
    mutate(rmsd =  sqrt((test - forecast)^2 / ts_h))
  
  err$rmsd |> mean() |> round(2)
}

makeTimeSeries <- function(sales) {
  ts_diag <- sales |>
    dplyr::select(Total, Date) |>
    group_by(Date) |>
    summarise(Total = sum(Total)) |>
    arrange(Date) |>
    as_tsibble(index = Date) |>
    rename(ds = Date, y = Total)
}

makeForecast <- function(ts_diag) {
  test_size <- 20
  fecha_corte <- max(ts_diag$ds) - test_size
  
  ts_train <- ts_diag |> filter(ds <= fecha_corte)
  ts_test  <- ts_diag |> filter(ds > fecha_corte)
  ts_h <- length(ts_test)
  
  decomp <- ts_diag |>
    model(STL(y ~ trend(window = 90))) |>
    components()
  
  rem <- decomp |> as_tibble() |> select(ds, remainder) |> rename(y = remainder) |> as_tsibble(index = ds)
  
  # Forecast de la tendencia con GridETS
  
  fc_trend <- (function(train){
    
    model_grid <- expand.grid(
      Tendencia = c("A", "N"),
      Estacionalidad = c("A", "M", "N")
    )
    
    ets_results <- purrr::pmap_dfr(model_grid, function(Tendencia, Estacionalidad) {
      
      fc <- train |> 
        model(ETS = ETS(y ~ trend(Tendencia |> as.character()) + season(Estacionalidad |> as.character()))) |> 
        forecast(h = 90) |>
        as_tibble() |>
        mutate(ds = ymd(ds)) |>
        dplyr::select(ds, .mean) |>
        rename(y = .mean)
      
      rmse_val <- buildForecastMetrics(fc, ts_test) |> buildForecastError(ts_h)
      
      tibble(Modelo = paste(Tendencia, Estacionalidad, sep = "_"),
             RMSE = rmse_val)
    })
    
    coef_results <- ets_results |> arrange(RMSE) |> head(1)
    coef_results <- (function(){
      m <- coef_results$Modelo |> str_split("_")
      p <- m[[1]]
      
      list(
        "trend" = p[1],
        "season" = p[2]
      )
    })()
    
    train |> 
      model(ETS = ETS(y ~ trend(coef_results$trend) + season(coef_results$season))) |> 
      forecast(h = 90) |>
      as_tibble() |>
      mutate(ds = ymd(ds)) |>
      dplyr::select(ds, .mean) |>
      rename(y = .mean)
  })(decomp |> as_tibble() |> select(ds, trend) |> rename(y = trend) |> as_tsibble())
  
  print(fc_trend)
  
  # Forecast de la estacionalidad siguiendo el patron
  
  fc_season <- (function(d){
    
    seasonal_data <- d |>
      dplyr::select(ds, season_week) |>
      rename(seasonality = season_week) %>%
      filter(!is.na(seasonality))
    
    future_dates <- seq.Date(from = max(ts_diag$ds), by = "day", length.out = 90) |>
      as_tibble() |>
      rename(ds = value)
    
    pattern <- tail(seasonal_data$seasonality, n = nrow(future_dates))
    
    if (length(pattern) < nrow(future_dates)) {
      pattern <- rep(pattern, length.out = nrow(future_dates))
    }
    
    fc_season <- future_dates %>%
      mutate(seasonality = pattern)
    
    fc_season
    
  })(decomp)
  
  print(fc_season)
  
  # Forecast de los residuos con Prophet
  
  fc_remainder <- (function(t){
    mdl <- prophet(t)
    ft <- make_future_dataframe(mdl, periods = 90)
    fc <- predict(mdl, ft)
    
    fc |>
      mutate(ds = lubridate::ymd(ds)) |>
      filter(ds > fecha_corte) |>
      dplyr::select(ds, yhat) |>
      rename(y = yhat)
  })(rem)
  
  print(fc_remainder)
  
  fc_trend |>
    inner_join(fc_season, by = join_by(ds)) |>
    mutate(rem = mean(fc_remainder$y)) |>
    rename(trend = y) |>
    mutate(y = trend + seasonality + rem) |>
    select(ds, y)
  
}

makeValuation.fcf <- function(fc, ratios) {
  if (!"y" %in% names(fc)) stop("La columna 'y' no está presente en el forecast.")
  if (nrow(fc) < 2) stop("No hay suficientes datos para calcular FCF.")
  
  fc <- fc |>
    mutate(
      ebit = y * ratios$MargenOperativo,
      depr = y * ratios$DepreciacionPorVenta,
      capex = y * ratios$CapexPorVenta,
      wc = y * ratios$CapTrabajoPorVenta
    ) |>
    mutate(
      delta_wc = difference(wc)
    ) |>
    na.omit() |>
    select(ds, ebit, depr, capex, delta_wc) |>
    mutate(fcf = ebit * (1 - ratios$TasaImpositiva) + depr - capex - delta_wc)
  
  if (nrow(fc) == 0) stop("El resultado de FCF está vacío después de aplicar difference() y na.omit().")
  
  return(fc)
}


makeValuation.npv <- function(fcf, ratios) {
  sum(fcf$fcf / (1 + ratios$TasaDescuento)^(1:length(fcf$fcf)))
}

makeValuation.terminal_value <- function(fcf, ratios) {
  terminal_value <- (tail(fcf, n = 1)$fcf * (1 + ratios$TasaCrecimiento)) / (ratios$TasaDescuento - ratios$TasaCrecimiento)
  terminal_value <- terminal_value / (1 + ratios$TasaDescuento)^length(fcf$fcf)
}

makeValuation <- function(fc, ratios) {
  fcf <- fc |>
    mutate(
      ebit = y * ratios$MargenOperativo,
      depr = y * ratios$DepreciacionPorVenta,
      capex = y * ratios$CapexPorVenta,
      wc = y * ratios$CapTrabajoPorVenta
    ) |>
    mutate(
      delta_wc = difference(wc)
    ) |>
    na.omit() |>
    select(-wc) |>
    mutate(fcf = ebit * (1 - ratios$TasaImpositiva) + depr - capex - delta_wc)
  
  npv <- sum(fcf$fcf / (1 + ratios$TasaDescuento)^(1:length(fcf$fcf)))
  
  terminal_value <- (tail(fcf, n = 1)$fcf * (1 + ratios$TasaCrecimiento)) / (ratios$TasaDescuento - ratios$TasaCrecimiento)
  terminal_value <- terminal_value / (1 + ratios$TasaDescuento)^length(fcf$fcf)
  
  valor_empresa <- npv + terminal_value
  
  valor_empresa
}

makeProductPortfolio <- function(sales, type = "tangent") {
  tks <- c("ElectronicAccessories", "FashionAccessories", "FoodAndBeverages", "HealthAndBeauty", "HomeAndLifestyle", "SportsAndTravel")
  
  trainGa <- function(x) {
    ga_results <- ga(
      type = "real-valued",
      fitness = x,
      lower = rep(0, n),
      upper = rep(1, n),
      popSize = 100,
      maxiter = 500,
      run = 50,
      seed = 123
    )
    
    plot(ga_results)
    
    ga_weights <- ga_results@solution
    ga_weights <- ga_weights / sum(ga_weights)
    
    y <- as.data.frame(ga_weights)
    colnames(y) <- tks
    
    ga_weights <- ga_weights |> as.numeric()
    
    y$Return <- c(sum(ga_weights * expected_returns))
    y$Risk <- c(sqrt(t(ga_weights) %*% cov_matrix %*% ga_weights))
    
    y
  }
  
  prices <- (function(){
    rellenar_ceros <- function(mat, factor = 0.1) {
      mat_filled <- mat
      for (j in seq_len(ncol(mat_filled))) {
        col <- mat_filled[, j]
        min_positive <- min(col[col > 0], na.rm = TRUE)
        if (!is.finite(min_positive)) next
        col[col == 0] <- min_positive * factor
        mat_filled[, j] <- col
      }
      return(mat_filled)
    }
    
    sales |>
      group_by(Date, Product.line) |>
      summarise(Total = sum(Total), .groups = "drop") |>
      pivot_wider(
        names_from = Product.line,
        values_from = Total,
        values_fill = list(Total = 0)
      ) |>
      rename (
        ElectronicAccessories = `Electronic accessories`,
        FashionAccessories = `Fashion accessories`,
        FoodAndBeverages = `Food and beverages`,
        HealthAndBeauty = `Health and beauty`,
        HomeAndLifestyle = `Home and lifestyle`,
        SportsAndTravel = `Sports and travel`
      ) |>
      select(-Date) |> 
      as.matrix() |>
      rellenar_ceros()
  })()
  
  retts <- prices |> log() |> diff() |> na.omit()
  
  cov_matrix <- cov(retts)
  
  expected_returns <- colMeans(retts)
  n <- length(expected_returns)
  
  fitness_sharpe <- function(wg) {
    wg <- wg / sum(wg)
    p_ret <- sum(wg * expected_returns)
    p_sd <- sqrt(t(wg) %*% cov_matrix %*% wg)
    -(p_ret / p_sd)
  }
  
  fitness_return <- function(wg) {
    wg <- wg / sum(wg)
    -(sum(wg * expected_returns))
  }
  
  fitness_risk <- function(wg) {
    wg <- wg / sum(wg)
    sqrt(t(wg) %*% cov_matrix %*% wg)
  }
  
  pf <- list()
  if (type == "min-risk") {
    pf <- trainGa(fitness_risk)
  } else if (type == "max-return") {
    pf <- trainGa(fitness_return)
  } else {
    pf <- trainGa(fitness_sharpe)
  }
  
  pf
}

plotProductPortfolio <- function(x){
  x <- x |> select(-Return, -Risk)
  
  df_long <- pivot_longer(x, cols = everything(), names_to = "Activo", values_to = "Peso")
  df_long <- df_long %>%
    mutate(
      porcentaje = round(Peso * 100, 1),
      etiqueta = paste0(Activo, "\n", porcentaje, "%")
    )
  
  p <- plot_ly(df_long, labels = ~Activo, values = ~Peso, type = 'pie',
               textinfo = 'label+percent',
               insidetextorientation = 'radial') %>%
    layout(title = 'Distribución de Pesos del Portafolio')
  
  p
}