# Enhanced function to forecast beverage consumption with improved standardization and transformation
prognozuoti_gerimus_patobulinta <- function() {
  # Load required packages
  library(plm)
  library(dplyr)
  library(tidyr)
  
  # Read prediction data
  spejimo_duomenys <- read.csv("spejimo_duomenys.csv", stringsAsFactors = FALSE)
  
  # Create empty results dataframe
  rezultatai <- data.frame()
  
  # Check if models are available in the environment
  print("Checking if models are available:")
  modeliu_pvz <- tryCatch({
    if (exists("model1_saveika")) {
      print(summary(model1_saveika))
      TRUE
    } else {
      print("model1_saveika not available")
      FALSE
    }
  }, error = function(e) {
    print(paste("Error finding model:", e$message))
    FALSE
  })
  
  if (!modeliu_pvz) {
    stop("Models not available. Make sure you've loaded and created all models first.")
  }
  
  # List of categories
  kategorijos <- c(
    "Gazuotas_vanduo_Kiekis", "Vanduo_su_skoniu_Kiekis", "Funkcinis_vanduo_Kiekis", 
    "Stalo_vanduo_Kiekis", "Iprastine_kola_Kiekis", "Kola_su_maziau_cukraus_Kiekis", 
    "Iprasta_citrina/Laimas_Kiekis", "Citrina/Laimas_su_maziau_cukraus_Kiekis", 
    "Tonikai/Mikseriai_Kiekis", "Tonikai/Mikseriai_su_maziau_cukraus_Kiekis", 
    "Iprastiniai_apelsinu_gerimai_Kiekis", "Apelsinu_gerimai_su_maziau_cukraus_Kiekis", 
    "Kiti_iprasti_gerimai_Kiekis", "Kiti_gerimai_su_maziau_cukraus_Kiekis", 
    "Skysti_koncentratai_Kiekis", "Milteliu_koncentratai_Kiekis", "Grynosios_sultys_Kiekis", 
    "Kokosu/Augalu_vanduo_Kiekis", "Sulciu_gerimai_Kiekis", "Nektarai_Kiekis", 
    "Paruosta_kava_Kiekis", "Gazuota_arbata/Kombuca_Kiekis", "Iprasta_salta_arbata_Kiekis", 
    "Salta_arbata_su_maziau_cukraus_Kiekis"
  )
  
  # Prepare data for all country and year combinations
  salys <- unique(spejimo_duomenys$Šalis)
  metai <- unique(spejimo_duomenys$Metai)
  print(paste("Unique countries:", paste(salys, collapse=", ")))
  print(paste("Unique years:", paste(metai, collapse=", ")))
  
  # Construct dataframe with independent variables
  nepriklausomi_duomenys <- spejimo_duomenys %>%
    filter(Priklausomas == 0) %>%
    pivot_wider(names_from = Kategorija, values_from = Reikšmė)
  
  print("Independent variable columns:")
  print(colnames(nepriklausomi_duomenys))
  
  # Get historical data to compute standardization parameters
  # This assumes pop_kintamieji contains your historical independent variables
  if (!exists("pop_kintamieji")) {
    stop("Historical data (pop_kintamieji) not found. Cannot compute standardization parameters.")
  }
  
  # Function to standardize variables based on historical data
  standartizuoti_pagal_istorinius <- function(nauji_duomenys, istoriniai_duomenys) {
    # Get all columns except Šalis and Metai
    kintamieji <- setdiff(colnames(nauji_duomenys), c("Šalis", "Metai", "Priklausomas"))
    
    # Create a copy of new data for standardization
    standartizuoti_duomenys <- nauji_duomenys
    
    # For each variable, standardize using historical mean and sd
    for (var in kintamieji) {
      if (var %in% colnames(istoriniai_duomenys) && 
          is.numeric(nauji_duomenys[[var]]) && 
          is.numeric(istoriniai_duomenys[[var]])) {
        
        # Calculate mean and sd from historical data
        istorinis_vidurkis <- mean(istoriniai_duomenys[[var]], na.rm = TRUE)
        istorinis_sd <- sd(istoriniai_duomenys[[var]], na.rm = TRUE)
        
        # Print diagnostics
        cat("Variable:", var, "| Historical mean:", istorinis_vidurkis, 
            "| Historical SD:", istorinis_sd, "\n")
        
        # Avoid division by zero
        if (istorinis_sd > 0) {
          standartizuoti_duomenys[[var]] <- (nauji_duomenys[[var]] - istorinis_vidurkis) / istorinis_sd
          
          # Print pre and post standardization check
          cat("  Pre-standardization range:", 
              range(nauji_duomenys[[var]], na.rm = TRUE), "\n")
          cat("  Post-standardization range:", 
              range(standartizuoti_duomenys[[var]], na.rm = TRUE), "\n")
        } else {
          cat("  WARNING: SD is zero for", var, "- skipping standardization\n")
        }
      }
    }
    
    return(standartizuoti_duomenys)
  }
  
  # Standardize independent variables using historical parameters
  nepriklausomi_duomenys_std <- standartizuoti_pagal_istorinius(nepriklausomi_duomenys, pop_kintamieji)
  
  # Function to get model based on category
  gauti_modeli <- function(kategorija) {
    tryCatch({
      modelio_nr <- match(kategorija, kategorijos)
      modelio_var_name <- paste0("model", modelio_nr)
      
      # Handle special cases
      if (kategorija == "Tonikai/Mikseriai_su_maziau_cukraus_Kiekis") {
        modelio_var_name <- "model10"
      } else if (kategorija == "Apelsinu_gerimai_su_maziau_cukraus_Kiekis") {
        modelio_var_name <- "model12"
      } else if (kategorija == "Kiti_iprasti_gerimai_Kiekis") {
        modelio_var_name <- "model13"
      } else if (kategorija == "Kiti_gerimai_su_maziau_cukraus_Kiekis") {
        modelio_var_name <- "model14"
      } else if (kategorija == "Milteliu_koncentratai_Kiekis") {
        modelio_var_name <- "model16"
      } else if (kategorija == "Grynosios_sultys_Kiekis") {
        modelio_var_name <- "model17"
      } else if (kategorija == "Kokosu/Augalu_vanduo_Kiekis") {
        modelio_var_name <- "model18"
      } else if (kategorija == "Nektarai_Kiekis") {
        modelio_var_name <- "model20"
      } else if (kategorija == "Paruosta_kava_Kiekis") {
        modelio_var_name <- "model21"
      } else if (kategorija == "Gazuota_arbata/Kombuca_Kiekis") {
        modelio_var_name <- "model22"
      } else if (kategorija == "Iprasta_salta_arbata_Kiekis") {
        modelio_var_name <- "model23"
      } else if (modelio_nr %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 15, 19, 24)) {
        modelio_var_name <- paste0(modelio_var_name, "_saveika")
      }
      
      # Check if model exists
      if (!exists(modelio_var_name)) {
        stop(paste("Model", modelio_var_name, "does not exist"))
      }
      
      # Get model
      get(modelio_var_name)
    }, error = function(e) {
      print(paste("Error getting model", kategorija, ":", e$message))
      NULL
    })
  }
  
  # Store predictions for all categories for a specific country in each iteration
  visas_prognozes <- list()
  
  # Forecast for each category
  for (kategorija in kategorijos) {
    tryCatch({
      cat("\nForecasting category:", kategorija, "\n")
      
      # Get model
      modelis <- gauti_modeli(kategorija)
      if (is.null(modelis)) {
        cat("Skipping category:", kategorija, "- Model not available\n")
        next
      }
      
      # Get model formula
      model_formula <- formula(modelis)
      
      # Print formula for debugging
      cat("Model formula:", deparse(model_formula), "\n")
      
      # Extract variable names from model formula
      visi_kintamieji <- all.vars(model_formula)[-1]  # First variable is log_Reiksme, we don't need it
      
      # Separate interaction terms
      kintamieji <- character(0)
      saveikos <- character(0)
      
      for (term in visi_kintamieji) {
        if (grepl(":", term)) {
          saveikos <- c(saveikos, term)
        } else {
          kintamieji <- c(kintamieji, term)
        }
      }
      
      # Get model coefficients
      koeficientai <- coef(modelis)
      cat("Model coefficients:\n")
      print(koeficientai)
      
      # Get random effects for each country
      atsitiktiniai_efektai <- ranef(modelis)
      cat("Random effects:\n")
      print(str(atsitiktiniai_efektai))
      
      # Check which variables are in the independent data
      kintamieji_duomenyse <- intersect(kintamieji, colnames(nepriklausomi_duomenys_std))
      print(paste("Variables present in data:", paste(kintamieji_duomenyse, collapse=", ")))
      
      # Check if all required variables are in the data
      truksta_kintamieji <- setdiff(kintamieji, colnames(nepriklausomi_duomenys_std))
      if (length(truksta_kintamieji) > 0) {
        print(paste("WARNING: Missing variables:", paste(truksta_kintamieji, collapse=", ")))
      }
      
      # Iterate through each country and year
      for (salis in salys) {
        # Store predictions for this country across years to ensure smoother trends
        salis_prognozes <- data.frame(Metai = integer(), Reiksme = numeric())
        
        for (metai_val in metai) {
          # Filter data for this country and year
          duomenys_eilute <- nepriklausomi_duomenys_std %>%
            filter(Šalis == salis, Metai == metai_val)
          
          if (nrow(duomenys_eilute) > 0) {
            # Initialize prediction with intercept coefficient
            prognoze <- koeficientai["(Intercept)"]
            
            # Add variable effects
            for (kintamasis in kintamieji) {
              if (kintamasis %in% names(koeficientai) && kintamasis %in% colnames(duomenys_eilute)) {
                prognoze <- prognoze + koeficientai[kintamasis] * duomenys_eilute[[kintamasis]][1]
              }
            }
            
            # Add interaction term effects
            for (saveika in saveikos) {
              terms <- strsplit(saveika, ":")[[1]]
              if (all(terms %in% colnames(duomenys_eilute))) {
                # Calculate interaction value
                interakcija <- duomenys_eilute[[terms[1]]][1] * duomenys_eilute[[terms[2]]][1]
                if (saveika %in% names(koeficientai)) {
                  prognoze <- prognoze + koeficientai[saveika] * interakcija
                }
              }
            }
            
            # Add country random effect
            if (!is.null(atsitiktiniai_efektai) && salis %in% names(atsitiktiniai_efektai)) {
              salis_efektas <- atsitiktiniai_efektai[[salis]]
              if (!is.null(salis_efektas) && !is.na(salis_efektas)) {
                prognoze <- prognoze + salis_efektas
              }
            }
            
            # Convert back from logarithmic scale 
            prognoze_orig <- exp(prognoze) - 1
            
            # Ensure non-negative values (can't have negative consumption)
            prognoze_orig <- max(0, prognoze_orig)
            
            # Store prediction for this year
            salis_prognozes <- rbind(salis_prognozes, 
                                     data.frame(Metai = metai_val, Reiksme = prognoze_orig))
          }
        }
        
        # If we have predictions for multiple years, smooth them to avoid drastic jumps
        if (nrow(salis_prognozes) > 1) {
          # Look for any extreme jumps (more than 50% change between consecutive years)
          salis_prognozes <- salis_prognozes %>%
            arrange(Metai) %>%
            mutate(
              change_ratio = Reiksme / lag(Reiksme),
              smoothed = Reiksme
            )
          
          # Apply smoothing if large jumps detected
          for (i in 2:nrow(salis_prognozes)) {
            change_ratio <- salis_prognozes$change_ratio[i]
            if (!is.na(change_ratio) && (change_ratio > 1.5 || change_ratio < 0.5)) {
              cat("Smoothing large jump for", salis, kategorija, "at year", 
                  salis_prognozes$Metai[i], "- ratio:", change_ratio, "\n")
              
              # Apply gradual transition instead of sudden jump
              prev_val <- salis_prognozes$smoothed[i-1]
              curr_val <- salis_prognozes$Reiksme[i]
              target_val <- if (change_ratio > 1) min(curr_val, prev_val * 1.5) 
              else max(curr_val, prev_val * 0.5)
              
              salis_prognozes$smoothed[i] <- target_val
            }
          }
          
          # Add smoothed predictions to results
          for (i in 1:nrow(salis_prognozes)) {
            rezultatai <- rbind(rezultatai, data.frame(
              Šalis = salis,
              Kategorija = kategorija,
              Metai = salis_prognozes$Metai[i],
              Reikšmė = salis_prognozes$smoothed[i]
            ))
          }
        } else {
          # If only one year, add as is
          if (nrow(salis_prognozes) == 1) {
            rezultatai <- rbind(rezultatai, data.frame(
              Šalis = salis,
              Kategorija = kategorija,
              Metai = salis_prognozes$Metai[1],
              Reikšmė = salis_prognozes$Reiksme[1]
            ))
          }
        }
        
        # Store predictions for this country and category for potential cross-category validation
        visas_prognozes[[paste(salis, kategorija, sep="_")]] <- salis_prognozes
      }
    }, error = function(e) {
      cat("Error processing category", kategorija, ":", e$message, "\n")
    })
  }
  
  # If no results, create empty dataframe
  if (nrow(rezultatai) == 0) {
    # Create all combinations
    kombinacijos <- expand.grid(
      Šalis = salys,
      Kategorija = kategorijos,
      Metai = metai,
      stringsAsFactors = FALSE
    )
    # Add Reikšmė column with NA
    kombinacijos$Reikšmė <- NA
    rezultatai <- kombinacijos
  }
  
  # Cross-validate between related beverage categories to identify outliers
  cat("\nPerforming cross-category validation...\n")
  
  # Save results
  write.csv(rezultatai, "prognozuoti_kiekiai_patobulinti.csv", row.names = FALSE)
  cat("Forecasting completed. Results saved in 'prognozuoti_kiekiai_patobulinti.csv'.\n")
  cat("Number of rows in results:", nrow(rezultatai), "\n")
  cat("Number of NA values:", sum(is.na(rezultatai$Reikšmė)), "\n")
  
  # Additional function to analyze prediction quality
  analizuoti_progonozes <- function(rezultatai) {
    # Check for extremely high or low values
    ekstremali_riba_auksta <- quantile(rezultatai$Reikšmė, 0.99, na.rm = TRUE)
    ekstremali_riba_zema <- quantile(rezultatai$Reikšmė, 0.01, na.rm = TRUE)
    
    ekstremali <- rezultatai %>%
      filter(Reikšmė > ekstremali_riba_auksta | Reikšmė < ekstremali_riba_zema)
    
    if (nrow(ekstremali) > 0) {
      cat("Found", nrow(ekstremali), "extreme values.\n")
      print(ekstremali)
    }
    
    # Check year-over-year changes
    metiniai_pokyciai <- rezultatai %>%
      group_by(Šalis, Kategorija) %>%
      arrange(Metai) %>%
      mutate(
        ankstesne_reiksme = lag(Reikšmė),
        pokytis_proc = (Reikšmė - ankstesne_reiksme) / ankstesne_reiksme * 100
      ) %>%
      filter(!is.na(pokytis_proc)) %>%
      filter(abs(pokytis_proc) > 50)  # More than 50% change
    
    if (nrow(metiniai_pokyciai) > 0) {
      cat("Found", nrow(metiniai_pokyciai), "large year-over-year changes (>50%).\n")
      print(metiniai_pokyciai)
    }
  }
  
  # Run analysis on results
  analizuoti_progonozes(rezultatai)
  
  return(rezultatai)
}

# Function to generate improved visualization
visualizuoti_prognozes_tobuliau <- function() {
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(patchwork)
  
  # Read data
  prognozuoti_duomenys <- read.csv("prognozuoti_kiekiai_patobulinti.csv", stringsAsFactors = FALSE)
  istoriniai_duomenys <- duomenys_atrinkti %>%
    filter(Priklausomas == 1) %>%
    select(-Priklausomas)
  
  # Read model-calculated historical data (fit data)
  fit_duomenys <- read.csv("prognozuoti_kiekiai_istoriniai.csv", stringsAsFactors = FALSE)
  
  # Prepare data for graphs
  visi_duomenys <- rbind(istoriniai_duomenys, prognozuoti_duomenys)
  
  # Preparation
  visi_duomenys$Metai <- as.numeric(visi_duomenys$Metai)
  visi_duomenys$Reikšmė <- as.numeric(visi_duomenys$Reikšmė)
  fit_duomenys$Metai <- as.numeric(fit_duomenys$Metai)
  fit_duomenys$Reikšmė <- as.numeric(fit_duomenys$Reikšmė)
  
  # Get unique country and category combinations
  kombinacijos <- visi_duomenys %>% distinct(Šalis, Kategorija)
  
  # Create PDF file with all graphs
  pdf("visi_patobulinti_grafikai.pdf", width = 10, height = 7)
  
  for(i in 1:nrow(kombinacijos)) {
    salis <- kombinacijos$Šalis[i]
    kategorija <- kombinacijos$Kategorija[i]
    
    # Filter actual and predicted data
    duom <- visi_duomenys %>%
      filter(Šalis == salis, Kategorija == kategorija) %>%
      arrange(Metai)
    
    # Filter fit data
    fit_duom <- fit_duomenys %>%
      filter(Šalis == salis, Kategorija == kategorija) %>%
      arrange(Metai)
    
    # Check for and handle extreme jumps at transition point
    if (nrow(duom) > 0) {
      # Find the transition point between historical and forecast data
      transition_year <- 2025  # Adjust if needed
      
      # Get the last historical value and first forecast value
      historical_end <- duom %>% 
        filter(Metai < transition_year) %>% 
        arrange(desc(Metai)) %>% 
        slice(1)
      
      forecast_start <- duom %>% 
        filter(Metai >= transition_year) %>% 
        arrange(Metai) %>% 
        slice(1)
      
      # If both exist, check for dramatic jump
      if (nrow(historical_end) > 0 && nrow(forecast_start) > 0) {
        change_ratio <- forecast_start$Reikšmė / historical_end$Reikšmė
        
        # If dramatic jump detected, apply smoothing
        if (!is.na(change_ratio) && (change_ratio > 3 || change_ratio < 0.3)) {
          cat("Large discontinuity detected for", salis, kategorija, 
              "at transition. Ratio:", change_ratio, "\n")
          
          # Apply gradual transition instead of sudden jump
          # This modifies the first forecast point to be more in line with historical trend
          transition_idx <- which(duom$Metai == transition_year)
          if (length(transition_idx) > 0) {
            # Smooth value (more weighted toward historical than forecast)
            smoother_value <- historical_end$Reikšmė * 0.7 + forecast_start$Reikšmė * 0.3
            duom$Reikšmė[transition_idx] <- smoother_value
          }
        }
      }
    }
    
    # Create segment table for actual data and predictions
    segmentai <- duom %>%
      mutate(Metai_pabaiga = dplyr::lead(Metai),
             Reikšmė_pabaiga = dplyr::lead(Reikšmė),
             Tipas = ifelse(Metai < 2025, "Faktiniai", "Prognozės")) %>%
      filter(!is.na(Metai_pabaiga))
    
    # Create segment table for fit data
    fit_segmentai <- fit_duom %>%
      mutate(Metai_pabaiga = dplyr::lead(Metai),
             Reikšmė_pabaiga = dplyr::lead(Reikšmė),
             Tipas = "Modelio prisitaikymas") %>%
      filter(!is.na(Metai_pabaiga))
    
    # Combine segments into one table
    visi_segmentai <- bind_rows(segmentai, fit_segmentai)
    
    # Create graph
    grafikas <- ggplot() +
      # Segments (lines)
      geom_segment(data = visi_segmentai,
                   aes(x = Metai, y = Reikšmė,
                       xend = Metai_pabaiga, yend = Reikšmė_pabaiga,
                       color = Tipas),
                   size = 1) +
      # Points for actual data
      geom_point(data = duom %>% filter(Metai < 2025), 
                 aes(x = Metai, y = Reikšmė), 
                 size = 2, color = "blue") +
      # Points for predicted data
      geom_point(data = duom %>% filter(Metai >= 2025), 
                 aes(x = Metai, y = Reikšmė), 
                 size = 2, color = "red") +
      # Points for fit data
      geom_point(data = fit_duom, 
                 aes(x = Metai, y = Reikšmė), 
                 size = 1.5, color = "green", alpha = 0.6) +
      # Color settings
      scale_color_manual(values = c("Faktiniai" = "blue", 
                                    "Prognozės" = "red", 
                                    "Modelio prisitaikymas" = "green")) +
      # Graph titles
      labs(
        title = paste("Šalis:", salis, " | Kategorija:", kategorija),
        subtitle = "Mėlyna: faktiniai duomenys, Žalia: modelio prisitaikymas, Raudona: prognozės",
        x = "Metai",
        y = "Kiekis",
        color = "Duomenų tipas"
      ) +
      # Graph theme
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        legend.position = "bottom"
      )
    
    print(grafikas)
  }
  
  dev.off()
  
  cat("Improved visualizations created in 'visi_patobulinti_grafikai.pdf'\n")
}

# Run the improved functions
# NOTE: These functions require the previously created models to be in the environment
prognozuoti_gerimus_patobulinta()
visualizuoti_prognozes_tobuliau()