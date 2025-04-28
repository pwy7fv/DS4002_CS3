#### BSTS Evaluations

# Actual values from the test data
actual_values <- test_data$ride_count[1:horizon]

# Ensure that bsts_pred is numeric before evaluating
if (!is.numeric(forecast_values)) {
  stop("bsts_pred is not numeric.")
}

# Evaluate RMSE, MAE, MAPE
rmse_bsts <- sqrt(mean((actual_values - forecast_values)^2))
mae_bsts <- mean(abs(actual_values - forecast_values))
mape_bsts <- mean(abs((actual_values - forecast_values) / actual_values)) * 100



# Print metrics
print(rmse_bsts)
print(mae_bsts)
print(mape_bsts)

> # Print metrics
  > print(rmse_bsts)
[1] 25.76266
> print(mae_bsts)
[1] 18.78601
> print(mape_bsts)
[1] 11.84987
>
  