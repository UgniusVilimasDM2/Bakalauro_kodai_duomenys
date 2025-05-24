# Įkeliame bibliotekas
library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)
library(stringi)

# Ištraukiame populiacijos kintamuosius (nepriklausomus)
pop_variables_data <- duomenys %>% 
  filter(Priklausomas == 0)

pop_vars <- unique(pop_variables_data$Kategorija)
countries <- unique(pop_variables_data$Šalis)

# Funkcija ARIMA modeliui ir prognozei sukurti
create_arima_forecast <- function(data, var_name, country_name) {
  # Filtruojame ir tvarkome duomenis
  var_data <- data %>%
    filter(Kategorija == var_name, Šalis == country_name) %>%
    arrange(Metai) %>%
    mutate(
      Metai = as.numeric(Metai),
      Reikšmė = as.numeric(Reikšmė)
    )
  
  # Tikriname ar pakanka duomenų
  if(nrow(var_data) < 8) return(NULL)
  
  # Kuriame laiko eilutę
  ts_data <- ts(var_data$Reikšmė, start = min(var_data$Metai), frequency = 1)
  
  # Pritaikome ARIMA modelį ir prognozuojame
  tryCatch({
    model <- auto.arima(ts_data, seasonal = FALSE, stepwise = TRUE, approximation = FALSE)
    forecast_values <- forecast(model, h = 5)
    
    # Formuojame prognozių duomenis (2025-2029)
    forecast_df <- data.frame(
      Šalis = country_name,
      Kategorija = var_name,
      Metai = 2025:2029,
      Priklausomas = 0,
      Reikšmė = as.numeric(forecast_values$mean),
      Lower95 = as.numeric(forecast_values$lower[, 2]),
      Upper95 = as.numeric(forecast_values$upper[, 2]),
      Tipas = "Prognozė"
    )
    
    # Sujungiame su istoriniais duomenimis
    historical_df <- var_data %>%
      mutate(Lower95 = NA, Upper95 = NA, Tipas = "Istoriniai")
    
    return(bind_rows(historical_df, forecast_df))
    
  }, error = function(e) {
    cat("Klaida apdorojant", var_name, "kintamąjį", country_name, "šalyje:", e$message, "\n")
    return(NULL)
  })
}

# Sukuriame katalogą rezultatams
dir.create("populiacijos_prognozes", showWarnings = FALSE)

# Saugojimo sąrašas visoms prognozėms
all_forecasts <- list()

# Apdorojame kiekvieną kintamąjį
for(var in pop_vars) {
  cat("Apdorojamas kintamasis:", var, "\n")
  
  # Kataloge kintamajui
  var_dir <- file.path("populiacijos_prognozes", gsub("[^a-zA-Z0-9]", "_", var))
  dir.create(var_dir, showWarnings = FALSE)
  
  # Prognozės kiekvienai šaliai
  for(country in countries) {
    forecast_result <- create_arima_forecast(pop_variables_data, var, country)
    
    if(!is.null(forecast_result)) {
      all_forecasts[[length(all_forecasts) + 1]] <- forecast_result
      
      # Kuriame individualų grafiką
      p <- ggplot(forecast_result, aes(x = Metai, y = Reikšmė, color = Tipas)) +
        geom_line() +
        geom_point() +
        geom_ribbon(aes(ymin = Lower95, ymax = Upper95), 
                    data = filter(forecast_result, Tipas == "Prognozė"),
                    fill = "blue", alpha = 0.2, color = NA) +
        labs(
          title = paste(var, "-", country),
          subtitle = "ARIMA prognozės (2025-2029)",
          x = "Metai",
          y = "Reikšmė"
        ) +
        scale_color_manual(
          values = c("Istoriniai" = "black", "Prognozė" = "blue"),
          name = "Duomenų tipas"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        ) +
        scale_x_continuous(breaks = seq(2010, 2029, by = 2))
      
      # Išsaugome grafiką
      plot_filename <- file.path(var_dir, paste0(gsub("[^a-zA-Z0-9]", "_", country), ".png"))
      ggsave(plot_filename, p, width = 10, height = 6, bg = "white")
    }
  }
  
  # Bendras grafikas visoms šalims
  var_forecasts <- Filter(function(x) !is.null(x) && x$Kategorija[1] == var, all_forecasts)
  
  if(length(var_forecasts) > 0) {
    combined_data <- bind_rows(var_forecasts)
    
    p_combined <- ggplot(combined_data, aes(x = Metai, y = Reikšmė, color = Šalis)) +
      geom_line() +
      facet_wrap(~Tipas, scales = "free_x", ncol = 2) +
      labs(
        title = paste("Visos šalys -", var),
        x = "Metai",
        y = "Reikšmė"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        strip.background = element_rect(fill = "lightgray", color = "gray"),
        strip.text = element_text(face = "bold")
      ) +
      scale_x_continuous(breaks = seq(2010, 2029, by = 5))
    
    # Išsaugome bendrą grafiką
    combined_filename <- file.path("populiacijos_prognozes", 
                                   paste0("Visos_salys_", gsub("[^a-zA-Z0-9]", "_", var), ".png"))
    ggsave(combined_filename, p_combined, width = 12, height = 8, bg = "white")
  }
}

# Sujungiame visas prognozes
all_forecasts_df <- bind_rows(all_forecasts)

# Išsaugome duomenis
write.csv(all_forecasts_df, "populiacijos_prognozes/visos_populiacijos_prognozes.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

# Prognozių santrauka
forecasts_summary <- all_forecasts_df %>%
  filter(Tipas == "Prognozė") %>%
  group_by(Kategorija, Metai) %>%
  summarize(
    Vidurkis = mean(Reikšmė, na.rm = TRUE),
    Mediana = median(Reikšmė, na.rm = TRUE),
    Minimumas = min(Reikšmė, na.rm = TRUE),
    Maksimumas = max(Reikšmė, na.rm = TRUE),
    Šalių_skaičius = n(),
    .groups = 'drop'
  )

# Išsaugome santrauką
write.csv(forecasts_summary, "populiacijos_prognozes/prognoziu_santrauka.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

# Pranešimai apie užbaigtą darbą
cat("\n========================================\n")
cat("ARIMA prognozavimas užbaigtas!\n")
cat("========================================\n")
cat("✓ Grafikai išsaugoti 'populiacijos_prognozes' kataloge\n")
cat("✓ Kiekvienas kintamasis turi atskirą katalogą\n")
cat("✓ Duomenys išsaugoti CSV formatu\n")
cat("✓ Sukurta prognozių santrauka\n")
cat("========================================\n")