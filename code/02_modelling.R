# Įkeliame bibliotekas
library(dplyr)
library(tidyr)
library(plm)
library(janitor)
library(stringi)
library(car)

# Paruošiame duomenis analizei
combined_data_final <- combined_data_final %>%
  mutate(
    Priklausomas = ifelse(str_ends(Kategorija, "Kiekis"), 1, 0),
    Kintamasis = ifelse(str_ends(Kategorija, "Vertė") | str_ends(Kategorija, "Kiekis"), 0, 1)
  )

# Filtruojame duomenis - pašaliname vertės kategorijas ir nereikalingus kintamuosius
duomenys <- combined_data_final %>%
  filter(
    !grepl("Vertė$", Kategorija),
    !Kategorija %in% c("Namų ūkio dydis", "Gimstamumas")
  ) %>%
  select(-Kintamasis)

# Išsaugome duomenis
write.csv(duomenys, file = "duomenys_su_prognoze.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Sutvarkom kategorijų pavadinimus (pašaliname lietuviškus simbolius)
duomenys$Kategorija <- duomenys$Kategorija %>%
  stri_trans_general("Latin-ASCII") %>%
  gsub(" ", "_", .)

# Atskiriame gėrimų ir populiacijos duomenis
gerimu_duomenys <- duomenys %>% filter(Priklausomas == 1)
pop_kintamieji <- duomenys %>%
  filter(Priklausomas == 0) %>%
  pivot_wider(id_cols = c(Šalis, Metai), names_from = Kategorija, values_from = Reikšmė)

# Ištraukiame gėrimų kategorijas
kategorijos <- gerimu_duomenys %>%
  filter(grepl("Kiekis$", Kategorija)) %>%
  distinct(Kategorija) %>%
  pull(Kategorija)

# ====================== MODELIŲ PALYGINIMO TESTAI ======================
# Rezultatų lentelė
rezultatai <- data.frame(
  Kategorija = character(),
  F_test_p = numeric(),
  BP_test_p = numeric(),
  Hausman_p = numeric(),
  Rekomenduojamas_modelis = character(),
  stringsAsFactors = FALSE
)

# Atliekame testus kiekvienai kategorijai
for (kat in kategorijos) {
  # Paruošiame duomenis
  duom_kat <- gerimu_duomenys %>%
    filter(Kategorija == kat, !is.na(Reikšmė)) %>%
    mutate(log_Reiksme = log(Reikšmė + 1)) %>%
    select(Šalis, Metai, log_Reiksme)
  
  modelio_duomenys <- left_join(duom_kat, pop_kintamieji, by = c("Šalis", "Metai"))
  
  if (anyNA(modelio_duomenys)) next
  
  # Sukuriame panelinį duomenų rinkinį
  panel_data <- pdata.frame(modelio_duomenys, index = c("Šalis", "Metai"))
  nepriklausomi <- setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))
  formule <- as.formula(paste("log_Reiksme ~", paste(nepriklausomi, collapse = " + ")))
  
  # Sukuriame modelius
  model_pool <- plm(formule, data = panel_data, model = "pooling")
  model_fe <- plm(formule, data = panel_data, model = "within")
  model_re <- tryCatch(plm(formule, data = panel_data, model = "random", random.method = "amemiya"), 
                       error = function(e) NULL)
  
  # Atliekame testus
  f_test <- tryCatch(pFtest(model_fe, model_pool), error = function(e) NULL)
  bp_test <- tryCatch(plmtest(model_pool, type = "bp"), error = function(e) NULL)
  
  hausman_p <- NA
  if (!is.null(model_re)) {
    hausman_test <- tryCatch(phtest(model_fe, model_re), error = function(e) NULL)
    if (!is.null(hausman_test)) hausman_p <- hausman_test$p.value
  }
  
  # Pasirenkame modelį pagal testų rezultatus
  pasirinktas_modelis <- "Pooled"
  if (!is.null(f_test) && f_test$p.value < 0.05) pasirinktas_modelis <- "FE"
  if (!is.null(bp_test) && bp_test$p.value < 0.05) pasirinktas_modelis <- "RE"
  if (!is.na(hausman_p) && hausman_p < 0.05) pasirinktas_modelis <- "FE"
  
  # Išsaugome rezultatus
  rezultatai <- rezultatai %>% add_row(
    Kategorija = kat,
    F_test_p = ifelse(is.null(f_test), NA, f_test$p.value),
    BP_test_p = ifelse(is.null(bp_test), NA, bp_test$p.value),
    Hausman_p = hausman_p,
    Rekomenduojamas_modelis = pasirinktas_modelis
  )
}

# Formatuojame p-reikšmes
rezultatai <- rezultatai %>%
  mutate(
    F_test_p = ifelse(as.numeric(F_test_p) < 1e-5, "<0.00001", formatC(as.numeric(F_test_p), digits = 5, format = "e")),
    BP_test_p = ifelse(as.numeric(BP_test_p) < 1e-5, "<0.00001", formatC(as.numeric(BP_test_p), digits = 5, format = "e")),
    Hausman_p = ifelse(is.na(Hausman_p), NA,
                       ifelse(as.numeric(Hausman_p) < 1e-5, "<0.00001", formatC(as.numeric(Hausman_p), digits = 5, format = "e")))
  )

print(rezultatai)

# ====================== BENDRŲJŲ EFEKTŲ MODELIAI (POOLED OLS) ======================
modeliai_su_reiksmais <- list()

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
  panel_data <- pdata.frame(modelio_duomenys_df, index = c("Šalis", "Metai"))
  nepriklausomi <- setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))
  
  modelio_duomenys_std <- modelio_duomenys_df
  for (var in nepriklausomi) {
    if (is.numeric(modelio_duomenys_std[[var]])) {
      modelio_duomenys_std[[var]] <- scale(modelio_duomenys_std[[var]])
    }
  }
  
  panel_data_std <- pdata.frame(modelio_duomenys_std, index = c("Šalis", "Metai"))
  formule_pilna <- as.formula(paste("log_Reiksme ~", paste(nepriklausomi, collapse = " + ")))
  
  tryCatch({
    # Pradinė kintamųjų atranka su lm
    lm_pilnas <- lm(formule_pilna, data = modelio_duomenys_std)
    santrauka <- summary(lm_pilnas)
    koeficientai_df <- as.data.frame(santrauka$coefficients)
    
    # Atrenkame reikšmingus kintamuosius (p < 0.05)
    reiksmingu_indeksai <- which(koeficientai_df[,4] < 0.05 & rownames(koeficientai_df) != "(Intercept)")
    
    if (length(reiksmingu_indeksai) == 0) {
      cat("Nėra reikšmingų kintamųjų kategorijai (p < 0.05):", kat, "\n")
    } else {
      # Rikiuojame pagal p-reikšmes ir imame TOP 5
      reiksmingu_p_reiksmes <- koeficientai_df[reiksmingu_indeksai, 4]
      surikiuoti_indeksai <- order(reiksmingu_p_reiksmes)
      rikiuoti_reiksmingu_indeksai <- reiksmingu_indeksai[surikiuoti_indeksai]
      max_kintamuju <- min(5, length(rikiuoti_reiksmingu_indeksai))
      reiksmingi <- rownames(koeficientai_df)[rikiuoti_reiksmingu_indeksai[1:max_kintamuju]]
      
      cat("5 reikšmingiausi kintamieji:", paste(reiksmingi, collapse=", "), "\n")
      
      # Tikriname multikolinearumą
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
        # Sukuriame galutinį bendrųjų efektų modelį
        formule_final <- as.formula(paste("log_Reiksme ~", paste(reiksmingi, collapse = " + ")))
        
        tryCatch({
          modelis <- plm(formule_final, data = panel_data_std, model = "pooling")
          cat("Bendrųjų efektų modelio rezultatai:\n")
          print(summary(modelis))
          modeliai_su_reiksmais[[kat]] <- modelis
        }, error = function(e) {
          cat("Klaida kuriant bendrųjų efektų modelį:", kat, "\n")
        })
      } else {
        cat("Po VIF atrankos neliko reikšmingų kintamųjų\n")
      }
    }
  }, error = function(e) {
    cat("Klaida kuriant pradinį modelį:", kat, "\n")
  })
}

# ====================== FIKSUOTŲ EFEKTŲ MODELIAI ======================
modeliai_fe <- list()

for (kat in kategorijos) {
  cat("\n=== Analizuojama kategorija:", kat, "===\n")
  
  # Paruošiame duomenis (identiškai kaip anksčiau)
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
  panel_data <- pdata.frame(modelio_duomenys_df, index = c("Šalis", "Metai"))
  nepriklausomi <- setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))
  
  modelio_duomenys_std <- modelio_duomenys_df
  for (var in nepriklausomi) {
    if (is.numeric(modelio_duomenys_std[[var]])) {
      modelio_duomenys_std[[var]] <- scale(modelio_duomenys_std[[var]])
    }
  }
  
  panel_data_std <- pdata.frame(modelio_duomenys_std, index = c("Šalis", "Metai"))
  formule_pilna <- as.formula(paste("log_Reiksme ~", paste(nepriklausomi, collapse = " + ")))
  
  tryCatch({
    # Sukuriame pilną fiksuotų efektų modelį
    plm_pilnas <- plm(formule_pilna, data = panel_data_std, model = "within", effect = "individual")
    santrauka <- summary(plm_pilnas)
    
    koeficientai_df <- data.frame(
      p.value = santrauka$coefficients[,4],
      row.names = rownames(santrauka$coefficients)
    )
    
    # Kintamųjų atranka pagal p < 0.1
    reiksmingu_indeksai <- which(koeficientai_df$p.value < 0.1)
    
    # Taikome atrankos taisykles
    if (length(reiksmingu_indeksai) == 0) {
      visu_p_reiksmes <- koeficientai_df$p.value
      surikiuoti_visi <- order(visu_p_reiksmes)
      reiksmingu_indeksai <- surikiuoti_visi[1:min(2, length(surikiuoti_visi))]
    } else if (length(reiksmingu_indeksai) == 1) {
      likusiu_indeksai <- which(koeficientai_df$p.value >= 0.1)
      if (length(likusiu_indeksai) > 0) {
        likusiu_p_reiksmes <- koeficientai_df$p.value[likusiu_indeksai]
        min_likusiu_indeksas <- likusiu_indeksai[which.min(likusiu_p_reiksmes)]
        reiksmingu_indeksai <- c(reiksmingu_indeksai, min_likusiu_indeksas)
      }
    } else if (length(reiksmingu_indeksai) > 5) {
      reiksmingu_p_reiksmes <- koeficientai_df$p.value[reiksmingu_indeksai]
      surikiuoti_indeksai <- order(reiksmingu_p_reiksmes)
      reiksmingu_indeksai <- reiksmingu_indeksai[surikiuoti_indeksai[1:5]]
    }
    
    reiksmingi <- rownames(koeficientai_df)[reiksmingu_indeksai]
    cat("Atrinkti kintamieji modeliui:", paste(reiksmingi, collapse=", "), "\n")
    
    # VIF patikrinimas
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
      # Galutinis fiksuotų efektų modelis
      formule_final <- as.formula(paste("log_Reiksme ~", paste(reiksmingi, collapse = " + ")))
      modelis <- plm(formule_final, data = panel_data_std, model = "within", effect = "individual")
      cat("Fiksuotų efektų modelio rezultatai:\n")
      print(summary(modelis))
      modeliai_fe[[kat]] <- modelis
    } else {
      cat("Po VIF atrankos neliko reikšmingų kintamųjų\n")
    }
  }, error = function(e) {
    cat("Klaida kuriant modelį\n")
  })
}

# Ištraukiame ir vizualizuojame šalių fiksuotus efektus
visu_saliu_efektai <- list()

for (kat in names(modeliai_fe)) {
  modelis <- modeliai_fe[[kat]]
  
  if (!is.null(modelis)) {
    tryCatch({
      # Gauname fiksuotus efektus
      saliu_efektai <- fixef(modelis)
      saliu_efektai_df <- data.frame(
        Šalis = names(saliu_efektai),
        Fiksuotas_efektas = as.numeric(saliu_efektai),
        Kategorija = kat,
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(Fiksuotas_efektas))
      
      visu_saliu_efektai[[kat]] <- saliu_efektai_df
      
      cat("\n=== Šalių fiksuoti efektai kategorijai:", kat, "===\n")
      print(saliu_efektai_df)
    }, error = function(e) {
      cat("Klaida gaunant fiksuotus efektus kategorijai", kat, ":", conditionMessage(e), "\n")
    })
  }
}

# Sujungiame ir išsaugome rezultatus
if (length(visu_saliu_efektai) > 0) {
  visi_efektai_df <- do.call(rbind, visu_saliu_efektai)
  pivot_efektai <- reshape2::dcast(visi_efektai_df, Šalis ~ Kategorija, value.var = "Fiksuotas_efektas")
  
  write.csv(visi_efektai_df, "visi_fiksuoti_efektai.csv", row.names = FALSE)
  write.csv(pivot_efektai, "fiksuoti_efektai_pivot.csv", row.names = FALSE)
  
  cat("\nVisi fiksuoti efektai išsaugoti failuose 'visi_fiksuoti_efektai.csv' ir 'fiksuoti_efektai_pivot.csv'\n")
}

# ====================== ATSITIKTINIŲ EFEKTŲ MODELIAI ======================
modeliai_re <- list()

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
  modelio_duomenys_std <- modelio_duomenys_df
  for (var in setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))) {
    if (is.numeric(modelio_duomenys_std[[var]])) {
      modelio_duomenys_std[[var]] <- scale(modelio_duomenys_std[[var]])
    }
  }
  
  panel_data_std <- pdata.frame(modelio_duomenys_std, index = c("Šalis", "Metai"))
  nepriklausomi <- setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))
  
  tryCatch({
    # Pilnas random effects modelis
    formule_pilna <- as.formula(paste("log_Reiksme ~", paste(nepriklausomi, collapse = " + ")))
    plm_pilnas <- plm(formule_pilna, data = panel_data_std, model = "random", random.method = "amemiya")
    santrauka <- summary(plm_pilnas)
    
    koeficientai_df <- data.frame(
      p.value = santrauka$coefficients[,4],
      row.names = rownames(santrauka$coefficients)
    )
    
    # Kintamųjų atranka
    reiksmingu_indeksai <- which(koeficientai_df$p.value < 0.1 & rownames(koeficientai_df) != "(Intercept)")
    visu_indeksai <- which(rownames(koeficientai_df) != "(Intercept)")
    visu_p_reiksmes <- koeficientai_df$p.value[visu_indeksai]
    surikiuoti_visi <- visu_indeksai[order(visu_p_reiksmes)]
    
    # Atrankos logika
    if (length(reiksmingu_indeksai) >= 2 && length(reiksmingu_indeksai) <= 5) {
      selected_indeksai <- reiksmingu_indeksai
    } else if (length(reiksmingu_indeksai) > 5) {
      reiksmingu_p_reiksmes <- koeficientai_df$p.value[reiksmingu_indeksai]
      surikiuoti_indeksai <- order(reiksmingu_p_reiksmes)
      selected_indeksai <- reiksmingu_indeksai[surikiuoti_indeksai[1:5]]
    } else if (length(reiksmingu_indeksai) == 1) {
      remaining_indices <- setdiff(surikiuoti_visi, reiksmingu_indeksai)
      if (length(remaining_indices) > 0) {
        selected_indeksai <- c(reiksmingu_indeksai, remaining_indices[1])
      } else {
        selected_indeksai <- reiksmingu_indeksai
      }
    } else {
      selected_indeksai <- surikiuoti_visi[1:min(2, length(surikiuoti_visi))]
    }
    
    reiksmingi <- rownames(koeficientai_df)[selected_indeksai]
    
    # VIF patikrinimas
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
      
      # Papildomas nereikšmingų kintamųjų pašalinimas
      galutine_santrauka <- summary(modelis)
      galutiniai_koef <- data.frame(
        p.value = galutine_santrauka$coefficients[,4],
        row.names = rownames(galutine_santrauka$coefficients)
      )
      
      nereiksmingu_indeksai <- which(galutiniai_koef$p.value > 0.1 & 
                                       rownames(galutiniai_koef) != "(Intercept)")
      
      if (length(nereiksmingu_indeksai) > 0) {
        if (length(rownames(galutiniai_koef)) - length(nereiksmingu_indeksai) - 1 >= 2) {
          # Pašaliname nereikšmingus
          nereiksmingu_kintamieji <- rownames(galutiniai_koef)[nereiksmingu_indeksai]
          reiksmingi <- setdiff(reiksmingi, nereiksmingu_kintamieji)
          
          formule_final_refined <- as.formula(paste("log_Reiksme ~", paste(reiksmingi, collapse = " + ")))
          modelis <- plm(formule_final_refined, data = panel_data_std, model = "random", random.method = "amemiya")
        } else {
          # Paliekame tik 2 geriausius
          galutiniai_kintamieji_be_intercept <- setdiff(rownames(galutiniai_koef), "(Intercept)")
          galutines_p_reiksmes <- galutiniai_koef$p.value[rownames(galutiniai_koef) != "(Intercept)"]
          names(galutines_p_reiksmes) <- galutiniai_kintamieji_be_intercept
          surikiuoti_galutiniai <- names(sort(galutines_p_reiksmes))
          
          reiksmingi <- surikiuoti_galutiniai[1:min(2, length(surikiuoti_galutiniai))]
          
          formule_final_refined <- as.formula(paste("log_Reiksme ~", paste(reiksmingi, collapse = " + ")))
          modelis <- plm(formule_final_refined, data = panel_data_std, model = "random", random.method = "amemiya")
        }
      }
      
      cat("Atrinkti kintamieji modeliui:", paste(reiksmingi, collapse=", "), "\n")
      cat("Atsitiktinių efektų modelio rezultatai:\n")
      print(summary(modelis))
      
      modeliai_re[[kat]] <- modelis
    } else {
      cat("Po VIF atrankos neliko reikšmingų kintamųjų\n")
    }
  }, error = function(e) {
    cat("Klaida kuriant modelį\n")
  })
}