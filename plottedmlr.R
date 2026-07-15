# =========================================================================
# CORRECTED DIAGNOSTIC VISUALIZATION GENERATOR 
# =========================================================================
library(tidyverse)
library(patchwork)

# 1. EXTRACT FROM ITERATION 1 (Raw Scale Training Data)
df_it1 <- tibble(
  fitted   = model_it1$fitted.values,
  residual = model_it1$residuals
)

# 2. EXTRACT FROM ITERATION 2 (Log Scale Training Data)
df_it2 <- tibble(
  fitted   = model_it2$fitted.values,
  residual = model_it2$residuals
)

# 3. EXTRACT FROM ITERATION 3 (Log Scale Capped Training Data)
df_it3 <- tibble(
  fitted   = model_it3$fitted.values,
  residual = model_it3$residuals
)

# --- PLOT 1: ITERATION 1 (RAW SCALE HETEROSCEDASTICITY) ---
p1 <- ggplot(df_it1, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.25, color = "#d95f02") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  geom_smooth(method = "loess", color = "black", se = FALSE, size = 1) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Iteration 1: Raw Baseline Model",
    subtitle = "Severe non-linear fanning profile and scale heteroscedasticity",
    x = "Predicted Price (Raw Dollars)",
    y = "Residuals ($)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# --- PLOT 2: ITERATION 2 (LOG SCALE TRANSLATION & ELBOW FLARE) ---
# Note: xintercept uses the log scale equivalent of $50,000 (log(50000) ≈ 10.82)
p2 <- ggplot(df_it2, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.25, color = "#7570b3") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  geom_smooth(method = "loess", color = "black", se = FALSE, size = 1) +
  geom_vline(xintercept = 10.82, linetype = "dotted", color = "red", size = 1.2) +
  annotate("text", x = 10.95, y = 1.5, label = "Luxury Elbow\n(log($50k) ≈ 10.8)", 
           color = "red", hjust = 0, fontface = "bold", size = 3.5) +
  labs(
    title = "Iteration 2: Engineered & Interaction Model",
    subtitle = "Variance destabilizes past the luxury breakpoint",
    x = "Predicted Price (Log Scale)",
    y = "Residuals (Log Scale)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# --- PLOT 3: ITERATION 3 (FINAL CONSUMER HOMOSCEDASTICITY) ---
p3 <- ggplot(df_it3, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.25, color = "#1b9e77") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  geom_smooth(method = "loess", color = "black", se = FALSE, size = 1) +
  labs(
    title = "Iteration 3: Final Capped Model",
    subtitle = "Homoscedastic, stable residual distribution optimized for the consumer tier",
    x = "Predicted Price (Log Scale)",
    y = "Residuals (Log Scale)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# --- COMPOSITE PLOT---
composite_diagnostic <- (p1 / p2 / p3) + 
  plot_annotation(
    title = "Residual Diagnostic Evolution Across Model Iterations",
    subtitle = "Visualizing the progression from raw data violations to stable OLS homoscedasticity",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Display the final plot
print(composite_diagnostic)