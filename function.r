setwd("C:/Users/dtisl/OneDrive - studmail.w-hs.de/Studium/3. Semester/MLDS 2/Präsentation")

create_arima_models <- function(dfv, max_order = 3) {
  for (p in 0:max_order) {
    for (d in 0:max_order) {
      for (q in 0:max_order) {
        order_tuple <- c(p, d, q)
        cat("Fitting ARIMA(", paste(order_tuple, collapse = ","), ") model...\n")
        
        # Erstellen Sie hier Ihr ARIMA-Modell mit den aktuellen p, d, q-Werten
        arima_model <- arima(dfv, order = order_tuple)
        
        cat("Model ARIMA(", paste(order_tuple, collapse = ","), ") fit complete.\n\n")
      }
    }
  }
}

sig_test <- function(modell) {
  arima100 <- arima(dfv, order=c(1, 0, 0))
  arima100$se = sqrt(arima100$var.coef)
  arima100_z = arima100$coef/arima100$se  
  arima100_z[1]
}

auto_arima_models <- function(data, max_order = 10) {
  results <- list()
  fixed_list <- list(NA ,NA)
  
  for (p in 1:max_order) {
    
    order <- c(p, 0, 0)
    
    arima_model <- arima(data, order = c(p, 0, 0), fixed = fixed_list)
    arima_model$se = sqrt(diag(arima_model$var.coef))
    arima_model$z = arima_model$coef[p] / arima_model$se
    if (abs(arima_model$z[p]) > 1.645) {
      results[[paste("ARIMA", paste(order, collapse = ","))]] <- arima_model
      fixed_list <- c(fixed_list[1:(length(fixed_list) - 1)], NA, fixed_list[length(fixed_list)])
    } else {
      fixed_list <- c(fixed_list[1:(length(fixed_list) - 1)], 0, fixed_list[length(fixed_list)])
    }
  }
  print(p)
  return(results)

}



df.modell <- function(dfv,x,y,z) {
  df.arima = arima(dfv, order=c(x, y, z))
  return(df.arima)
}

aic_calc <- function(modell) {
  npar <- length(modell$coef) + 1
  
  aic <- -2 * modell$loglik + 2 * npar
  return(aic)
}

bic_calc <- function(modell) {
  npar <- length(modell$coef) + 1
  nobs <- length(modell$residuals)
  
  bic <- -2 * modell$loglik + npar * log(nobs)
  return(bic)
}

hq_calc <- function(modell) {
  npar <- length(modell$coef)
  nobs <- length(modell$residuals)
  
  hq <- -2 * modell$loglik + 2 * npar * (log(nobs) / log(2))
  
  return(hq)
}


info_krit <- function(modell) {
  summe = aic_calc(modell) + bic_calc(modell) 
  
  cat("aic: ",round(aic_calc(modell), 2), "\n") 
  cat("bic: ",round(bic_calc(modell), 2), "\n") 
  cat("hq: ",round(hq_calc(modell), 2), "\n") 
  cat("sum_aic+bic: ",round(summe, 2),"\n")
  
}

mae <- function(observed, predicted){
  mae_dfv <- mean(abs(observed - predicted))
  return (mae_dfv)
}

rmse <- function(observed,predicted){
  rmse_dfv <- sqrt(mean((observed - predicted)^2))
  return (rmse_dfv)
}

mape <- function(observed,predicted){
  mape_dfv <- mean(abs((observed - predicted) / observed) * 100 )
  return (mape_dfv)
} #Prozentuale Abweichung von dem vorliegenden Wert

prog_krit <- function(observed, predicted) {
  cat("mae: ", mae(observed, predicted), "\n")
  cat("rmse: ",rmse(observed, predicted), "\n")
  cat("mape: ", mape(observed, predicted), "\n")
}
