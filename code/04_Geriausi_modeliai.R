# Įkeliame bibliotekas
library(dplyr)
library(plm)
library(car)

# ====================== ATSITIKTINIŲ EFEKTŲ MODELIAI SU P < 0.05 ======================
modeliai_re_05 <- list()

for (kat in kategorijos) {
  cat("\n=== Analizuojama kategorija:", kat, "===\n")
  
  # Paruošiame duomenis
  duom_kat <- gerimu_duomenys %>%
    filter(Kategorija == kat, !is.na(Reikšmė)) %>%
    mutate(log_Reiksme = log(Reikšmė + 1)) %>%
    dplyr::select(Šalis, Metai, log_Reiksme)
  
  modelio_duomenys <- left_join(duom_kat, pop_kintamieji, by = c("Šalis", "Metai"))
  
  if (anyNA(modelio_duomenys)) {
    cat("Praleidžiama kategorija dėl trūkstamų reikšmių:", kat, "\n")
    next
  }
  
  # Standartizuojame duomenis
  modelio_duomenys_df <- as.data.frame(modelio_duomenys)
  modelio_duomenys_std <- modelio_duomenys_df
  nepriklausomi <- setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))
  
  for (var in nepriklausomi) {
    if (is.numeric(modelio_duomenys_std[[var]])) {
      modelio_duomenys_std[[var]] <- scale(modelio_duomenys_std[[var]])
    }
  }
  
  panel_data_std <- pdata.frame(modelio_duomenys_std, index = c("Šalis", "Metai"))
  
  tryCatch({
    # Pilnas modelis
    formule_pilna <- as.formula(paste("log_Reiksme ~", paste(nepriklausomi, collapse = " + ")))
    plm_pilnas <- plm(formule_pilna, data = panel_data_std, model = "random", random.method = "amemiya")
    santrauka <- summary(plm_pilnas)
    
    koeficientai_df <- data.frame(
      p.value = santrauka$coefficients[,4],
      row.names = rownames(santrauka$coefficients)
    )
    
    # Atrenkame kintamuosius su p < 0.05
    reiksmingu_indeksai <- which(koeficientai_df$p.value < 0.05 & 
                                   rownames(koeficientai_df) != "(Intercept)")
    
    if (length(reiksmingu_indeksai) > 0) {
      reiksmingi <- rownames(koeficientai_df)[reiksmingu_indeksai]
      cat("Kintamieji su p < 0.05:", paste(reiksmingi, collapse=", "), "\n")
      
      # VIF tikrinimas
      if (length(reiksmingi) > 1) {
        formule_vif <- as.formula(paste("log_Reiksme ~", paste(reiksmingi, collapse = " + ")))
        lm_vif <- lm(formule_vif, data = modelio_duomenys_std)
        
        tryCatch({
          vif_reiksmes <- vif(lm_vif)
          auksto_vif <- names(vif_reiksmes[vif_reiksmes > 10])
          if (length(auksto_vif) > 0) {
            cat("Aukšto VIF kintamieji (>10):", paste(auksto_vif, collapse=", "), "\n")
            reiksmingi <- setdiff(reiksmingi, auksto_vif)
          }
        }, error = function(e) {
          cat("Klaida skaičiuojant VIF\n")
        })
      }
      
      if (length(reiksmingi) > 0) {
        # Galutinis modelis
        formule_final <- as.formula(paste("log_Reiksme ~", paste(reiksmingi, collapse = " + ")))
        modelis <- plm(formule_final, data = panel_data_std, model = "random", random.method = "amemiya")
        
        cat("Galutinis modelis su visais p < 0.05 kintamaisiais:", paste(reiksmingi, collapse=", "), "\n")
        print(summary(modelis))
        
        modeliai_re_05[[kat]] <- modelis
      } else {
        cat("Po VIF atrankos neliko reikšmingų kintamųjų\n")
      }
    } else {
      cat("Nėra kintamųjų su p < 0.05\n")
    }
  }, error = function(e) {
    cat("Klaida kuriant modelį:", conditionMessage(e), "\n")
  })
}

# Išsaugome modelius ir suvestinę
saveRDS(modeliai_re_05, "modeliai_re_05.rds")

model_summary <- data.frame(
  Kategorija = character(),
  R_squared = numeric(),
  Kintamuju_skaicius = numeric(),
  Kintamieji = character(),
  stringsAsFactors = FALSE
)

for (kat in names(modeliai_re_05)) {
  model <- modeliai_re_05[[kat]]
  if (!is.null(model)) {
    model_summary <- rbind(model_summary, data.frame(
      Kategorija = kat,
      R_squared = summary(model)$r.squared[1],
      Kintamuju_skaicius = length(coef(model)) - 1,
      Kintamieji = paste(names(coef(model))[-1], collapse = ", "),
      stringsAsFactors = FALSE
    ))
  }
}

if (nrow(model_summary) > 0) {
  cat("\n=== Modelių suvestinė ===\n")
  print(model_summary[order(-model_summary$R_squared), ])
  write.csv(model_summary, "modeliu_re_05_suvestine.csv", row.names = FALSE)
}

# ====================== MODELIAI SU TOP 3 KINTAMAISIAIS ======================
modeliai_re_top3 <- list()

for (kat in kategorijos) {
  cat("\n=== Analizuojama kategorija:", kat, "===\n")
  
  # Paruošiame duomenis (identiškai)
  duom_kat <- gerimu_duomenys %>%
    filter(Kategorija == kat, !is.na(Reikšmė)) %>%
    mutate(log_Reiksme = log(Reikšmė + 1)) %>%
    dplyr::select(Šalis, Metai, log_Reiksme)
  
  modelio_duomenys <- left_join(duom_kat, pop_kintamieji, by = c("Šalis", "Metai"))
  if (anyNA(modelio_duomenys)) {
    cat("Praleidžiama kategorija dėl trūkstamų reikšmių:", kat, "\n")
    next
  }
  
  # Standartizuojame
  modelio_duomenys_df <- as.data.frame(modelio_duomenys)
  modelio_duomenys_std <- modelio_duomenys_df
  nepriklausomi <- setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))
  
  for (var in nepriklausomi) {
    if (is.numeric(modelio_duomenys_std[[var]])) {
      modelio_duomenys_std[[var]] <- scale(modelio_duomenys_std[[var]])
    }
  }
  
  panel_data_std <- pdata.frame(modelio_duomenys_std, index = c("Šalis", "Metai"))
  
  tryCatch({
    # Pilnas modelis
    formule_pilna <- as.formula(paste("log_Reiksme ~", paste(nepriklausomi, collapse = " + ")))
    plm_pilnas <- plm(formule_pilna, data = panel_data_std, model = "random", random.method = "amemiya")
    santrauka <- summary(plm_pilnas)
    
    koeficientai_df <- data.frame(
      koeficientas = santrauka$coefficients[,1],
      st_klaida = santrauka$coefficients[,2],
      t_reiksme = santrauka$coefficients[,3],
      p_reiksme = santrauka$coefficients[,4],
      row.names = rownames(santrauka$coefficients)
    )
    
    # Rikiuojame pagal p-reikšmes ir imame TOP 3
    koeficientai_be_intercept <- koeficientai_df[rownames(koeficientai_df) != "(Intercept)", ]
    surikiuoti_koef <- koeficientai_be_intercept[order(koeficientai_be_intercept$p_reiksme), , drop = FALSE]
    top3_kintamieji <- rownames(surikiuoti_koef)[1:min(3, nrow(surikiuoti_koef))]
    
    cat("3 kintamieji su mažiausiomis p reikšmėmis:", paste(top3_kintamieji, collapse=", "), "\n")
    cat("Jų p reikšmės:", paste(round(surikiuoti_koef[top3_kintamieji, "p_reiksme"], 6), collapse=", "), "\n")
    
    # VIF tikrinimas ir koregavimas
    if (length(top3_kintamieji) > 1) {
      formule_vif <- as.formula(paste("log_Reiksme ~", paste(top3_kintamieji, collapse = " + ")))
      lm_vif <- lm(formule_vif, data = modelio_duomenys_std)
      
      tryCatch({
        vif_reiksmes <- vif(lm_vif)
        auksto_vif <- names(vif_reiksmes[vif_reiksmes > 10])
        
        if (length(auksto_vif) > 0) {
          cat("Aukšto VIF kintamieji (>10):", paste(auksto_vif, collapse=", "), "\n")
          
          # Koreguojame kintamųjų sąrašą
          top3_be_vif <- setdiff(top3_kintamieji, auksto_vif)
          
          if (length(top3_be_vif) < min(3, nrow(surikiuoti_koef))) {
            papildomi_kintamieji <- setdiff(rownames(surikiuoti_koef), c(top3_kintamieji, auksto_vif))
            if (length(papildomi_kintamieji) > 0) {
              papildomi_imti <- papildomi_kintamieji[1:min(3 - length(top3_be_vif), length(papildomi_kintamieji))]
              top3_kintamieji <- c(top3_be_vif, papildomi_imti)
              cat("Papildytas sąrašas po VIF:", paste(top3_kintamieji, collapse=", "), "\n")
            } else {
              top3_kintamieji <- top3_be_vif
            }
          } else {
            top3_kintamieji <- top3_be_vif
          }
        }
      }, error = function(e) {
        cat("Klaida skaičiuojant VIF\n")
      })
    }
    
    if (length(top3_kintamieji) > 0) {
      # Galutinis modelis
      formule_final <- as.formula(paste("log_Reiksme ~", paste(top3_kintamieji, collapse = " + ")))
      modelis <- plm(formule_final, data = panel_data_std, model = "random", random.method = "amemiya")
      
      cat("Galutinio modelio rezultatai su", length(top3_kintamieji), "kintamaisiais:\n")
      print(summary(modelis))
      
      modeliai_re_top3[[kat]] <- modelis
    }
  }, error = function(e) {
    cat("Klaida kuriant modelį:", conditionMessage(e), "\n")
  })
}

# Išsaugome modelius ir suvestinę
saveRDS(modeliai_re_top3, "modeliai_re_top3.rds")

model_summary_top3 <- data.frame(
  Kategorija = character(),
  R_squared = numeric(),
  Kintamuju_skaicius = numeric(),
  Kintamieji = character(),
  P_reiksmes = character(),
  stringsAsFactors = FALSE
)

for (kat in names(modeliai_re_top3)) {
  model <- modeliai_re_top3[[kat]]
  if (!is.null(model)) {
    modelio_santrauka <- summary(model)
    kintamieji <- names(coef(model))[-1]
    p_reiksmes <- modelio_santrauka$coefficients[kintamieji, 4]
    
    model_summary_top3 <- rbind(model_summary_top3, data.frame(
      Kategorija = kat,
      R_squared = modelio_santrauka$r.squared[1],
      Kintamuju_skaicius = length(kintamieji),
      Kintamieji = paste(kintamieji, collapse = ", "),
      P_reiksmes = paste(round(p_reiksmes, 6), collapse = ", "),
      stringsAsFactors = FALSE
    ))
  }
}

if (nrow(model_summary_top3) > 0) {
  cat("\n=== Modelių suvestinė ===\n")
  print(model_summary_top3[order(-model_summary_top3$R_squared), ])
  write.csv(model_summary_top3, "modeliu_re_top3_suvestine.csv", row.names = FALSE)
}

# ====================== AIC IR BIC SKAIČIAVIMAS ======================
# AIC ir BIC skaičiavimo funkcija
calculate_aic_bic <- function(model) {
  n <- length(model$residuals)
  k <- length(coef(model))
  rss <- sum(model$residuals^2)
  sigma2 <- rss / n
  
  # Logaritminė tikėtinumo funkcija
  loglik <- -n/2 * log(2 * pi) - n/2 * log(sigma2) - rss/(2 * sigma2)
  
  # AIC ir BIC
  aic_value <- -2 * loglik + 2 * k
  bic_value <- -2 * loglik + log(n) * k
  
  return(list(AIC = aic_value, BIC = bic_value))
}

# Skaičiuojame visų modelių AIC ir BIC
cat("\n=== MODELIŲ PALYGINIMAS (AIC/BIC) ===\n")

# P < 0.1 modeliai
cat("\n--- Modeliai su p < 0.1 taisykle ---\n")
rezultatai_re <- data.frame(
  Kategorija = character(),
  AIC = numeric(),
  BIC = numeric(),
  Kintamuju_skaicius = numeric(),
  stringsAsFactors = FALSE
)

for (kat in names(modeliai_re)) {
  model <- modeliai_re[[kat]]
  metrics <- calculate_aic_bic(model)
  rezultatai_re <- rbind(rezultatai_re, data.frame(
    Kategorija = kat,
    AIC = metrics$AIC,
    BIC = metrics$BIC,
    Kintamuju_skaicius = length(coef(model)) - 1
  ))
}
print(rezultatai_re)

# P < 0.05 modeliai
cat("\n--- Modeliai su p < 0.05 ---\n")
rezultatai_05 <- data.frame(
  Kategorija = character(),
  AIC = numeric(),
  BIC = numeric(),
  Kintamuju_skaicius = numeric(),
  stringsAsFactors = FALSE
)

for (kat in names(modeliai_re_05)) {
  model <- modeliai_re_05[[kat]]
  metrics <- calculate_aic_bic(model)
  rezultatai_05 <- rbind(rezultatai_05, data.frame(
    Kategorija = kat,
    AIC = metrics$AIC,
    BIC = metrics$BIC,
    Kintamuju_skaicius = length(coef(model)) - 1
  ))
}
print(rezultatai_05)

# TOP 3 modeliai
cat("\n--- Modeliai su TOP 3 kintamaisiais ---\n")
rezultatai_3 <- data.frame(
  Kategorija = character(),
  AIC = numeric(),
  BIC = numeric(),
  Kintamuju_skaicius = numeric(),
  stringsAsFactors = FALSE
)

for (kat in names(modeliai_re_top3)) {
  model <- modeliai_re_top3[[kat]]
  metrics <- calculate_aic_bic(model)
  rezultatai_3 <- rbind(rezultatai_3, data.frame(
    Kategorija = kat,
    AIC = metrics$AIC,
    BIC = metrics$BIC,
    Kintamuju_skaicius = length(coef(model)) - 1
  ))
}
print(rezultatai_3)

# ====================== SĄVEIKŲ ANALIZĖ ======================
library(utils)
library(combinat)

reiksmingos_saveikos <- list()
reiksmingos_saveikos_info <- list()

for (kat in kategorijos) {
  cat("\n=== Analizuojama kategorija:", kat, "===\n")
  
  # Paruošiame duomenis
  duom_kat <- gerimu_duomenys %>%
    filter(Kategorija == kat, !is.na(Reikšmė)) %>%
    mutate(log_Reiksme = log(Reikšmė + 1)) %>%
    dplyr::select(Šalis, Metai, log_Reiksme)
  
  modelio_duomenys <- left_join(duom_kat, pop_kintamieji, by = c("Šalis", "Metai"))
  if (anyNA(modelio_duomenys)) {
    cat("Praleidžiama kategorija dėl trūkstamų reikšmių:", kat, "\n")
    next
  }
  
  # Standartizuojame
  modelio_duomenys_df <- as.data.frame(modelio_duomenys)
  nepriklausomi <- setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))
  modelio_duomenys_std <- modelio_duomenys_df
  
  for (var in nepriklausomi) {
    if (is.numeric(modelio_duomenys_std[[var]])) {
      modelio_duomenys_std[[var]] <- scale(modelio_duomenys_std[[var]])
    }
  }
  
  panel_data_std <- pdata.frame(modelio_duomenys_std, index = c("Šalis", "Metai"))
  
  # Tikriname visas dviejų kintamųjų sąveikas
  kintamuju_kombinacijos <- combn(nepriklausomi, 2)
  reiksmingu_saveiku_sarasas <- c()
  reiksmingu_saveiku_info_list <- list()
  
  for (i in 1:ncol(kintamuju_kombinacijos)) {
    var1 <- kintamuju_kombinacijos[1, i]
    var2 <- kintamuju_kombinacijos[2, i]
    
    # Formulė su sąveika
    formule <- as.formula(paste("log_Reiksme ~", var1, "*", var2))
    
    tryCatch({
      modelis <- plm(formule, data = panel_data_std, model = "random", random.method = "amemiya")
      santrauka <- summary(modelis)
      koeficientai <- santrauka$coefficients
      
      # Ieškome sąveikos
      saveikos_pavadinimas <- paste(var1, ":", var2, sep = "")
      saveikos_eilute <- which(rownames(koeficientai) == saveikos_pavadinimas)
      
      # Jei sąveika reikšminga (p < 0.05)
      if (length(saveikos_eilute) > 0 && koeficientai[saveikos_eilute, 4] < 0.05) {
        p_reiksme <- koeficientai[saveikos_eilute, 4]
        koef_reiksme <- koeficientai[saveikos_eilute, 1]
        var1_p <- koeficientai[which(rownames(koeficientai) == var1), 4]
        var2_p <- koeficientai[which(rownames(koeficientai) == var2), 4]
        
        # Saugome informaciją
        saveikos_info <- list(
          var1 = var1,
          var2 = var2,
          koef = koef_reiksme,
          p_saveika = p_reiksme,
          p_var1 = var1_p,
          p_var2 = var2_p,
          r_squared = santrauka$r.squared[1]
        )
        
        reiksmingu_saveiku_info <- sprintf("%s * %s (koef = %.4f, p = %.6f) [%s p=%.4f, %s p=%.4f, R²=%.4f]", 
                                           var1, var2, koef_reiksme, p_reiksme, var1, var1_p, var2, var2_p, 
                                           santrauka$r.squared[1])
        reiksmingu_saveiku_sarasas <- c(reiksmingu_saveiku_sarasas, reiksmingu_saveiku_info)
        reiksmingu_saveiku_info_list[[length(reiksmingu_saveiku_info_list) + 1]] <- saveikos_info
        
        cat("Reikšminga sąveika:", reiksmingu_saveiku_info, "\n")
        print(santrauka)
        cat("\n-----------------------------\n")
      }
    }, error = function(e) {
      cat("Klaida tikrinant sąveiką", var1, "*", var2, ":", conditionMessage(e), "\n")
    })
  }
  
  if (length(reiksmingu_saveiku_sarasas) > 0) {
    reiksmingos_saveikos[[kat]] <- reiksmingu_saveiku_sarasas
    reiksmingos_saveikos_info[[kat]] <- reiksmingu_saveiku_info_list
  } else {
    cat("Nerasta reikšmingų sąveikų kategorijai:", kat, "\n")
  }
}

# Rezultatų suvestinė
cat("\n\n============== REIKŠMINGOS SĄVEIKOS (p < 0.05) ==============\n\n")

for (kat in names(reiksmingos_saveikos)) {
  cat("\n=== Kategorija:", kat, "===\n")
  for (sav in reiksmingos_saveikos[[kat]]) {
    cat("  ", sav, "\n")
  }
}

# Išsaugome rezultatus
rezultatu_df <- data.frame(
  Kategorija = character(),
  Saveika = character(),
  Koeficientas = numeric(),
  P_reiksme_saveika = numeric(),
  P_reiksme_var1 = numeric(),
  P_reiksme_var2 = numeric(),
  R_squared = numeric(),
  stringsAsFactors = FALSE
)

for (kat in names(reiksmingos_saveikos)) {
  saveiku_info_list <- reiksmingos_saveikos_info[[kat]]
  
  for (info in saveiku_info_list) {
    rezultatu_df <- rbind(rezultatu_df, data.frame(
      Kategorija = kat,
      Saveika = paste(info$var1, "*", info$var2),
      Koeficientas = info$koef,
      P_reiksme_saveika = info$p_saveika,
      P_reiksme_var1 = info$p_var1,
      P_reiksme_var2 = info$p_var2,
      R_squared = info$r_squared,
      stringsAsFactors = FALSE
    ))
  }
}

# Rikiuojame ir išsaugome
rezultatu_df <- rezultatu_df[order(rezultatu_df$Kategorija, rezultatu_df$P_reiksme_saveika), ]
write.csv(rezultatu_df, "reiksmingos_saveikos.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(rezultatu_df, "reiksmingos_saveikos_detali.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\n========================================\n")
cat("✓ Visi modeliai baigti\n")
cat("✓ Rezultatai išsaugoti CSV failuose\n")
cat("✓ Modeliai išsaugoti RDS failuose\n")
cat("========================================\n")