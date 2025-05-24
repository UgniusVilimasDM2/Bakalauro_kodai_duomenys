################### RANDOM EFFECTS SU P < 0.05 + GERIAUSIOS SĄVEIKOS ###################################
library(dplyr)
library(plm)
library(car)
library(combinat)

# Rezultatų saugojimui
modeliai_re_05_su_geriausia_saveika <- list()
reiksmingos_saveikos_modelyje <- list()

for (kat in kategorijos) {
  cat("=== Kategorija:", kat, "===\n")
  
  duom_kat <- gerimu_duomenys %>%
    filter(Kategorija == kat, !is.na(Reikšmė)) %>%
    mutate(log_Reiksme = log(Reikšmė + 1)) %>%
    dplyr::select(Šalis, Metai, log_Reiksme)
  
  modelio_duomenys <- left_join(duom_kat, pop_kintamieji, by = c("Šalis", "Metai"))
  
  if (anyNA(modelio_duomenys)) {
    next
  }
  
  # Konvertuojame į duomenų rinkinį ir tada į panel data frame
  modelio_duomenys_df <- as.data.frame(modelio_duomenys)
  panel_data <- pdata.frame(modelio_duomenys_df, index = c("Šalis", "Metai"))
  nepriklausomi <- setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))
  
  # Standartizuojame kintamuosius
  modelio_duomenys_std <- modelio_duomenys_df
  for (var in nepriklausomi) {
    if (is.numeric(modelio_duomenys_std[[var]])) {
      modelio_duomenys_std[[var]] <- scale(modelio_duomenys_std[[var]])
    }
  }
  
  panel_data_std <- pdata.frame(modelio_duomenys_std, index = c("Šalis", "Metai"))
  
  tryCatch({
    # Sukuriame pilną random effects modelį su visais kintamaisiais
    plm_pilnas <- plm(as.formula(paste("log_Reiksme ~", paste(nepriklausomi, collapse = " + "))), 
                      data = panel_data_std, model = "random", random.method = "amemiya")
    santrauka <- summary(plm_pilnas)
    
    # Išgauname koeficientus iš plm modelio
    koeficientai_df <- data.frame(
      p.value = santrauka$coefficients[,4],
      row.names = rownames(santrauka$coefficients)
    )
    
    # Atrenkame kintamuosius, kurių p < 0.05, išskyrus konstantą
    reiksmingu_indeksai <- which(koeficientai_df$p.value < 0.05 & 
                                   rownames(koeficientai_df) != "(Intercept)")
    
    # Atrenkame kintamuosius
    if (length(reiksmingu_indeksai) > 0) {
      reiksmingi <- rownames(koeficientai_df)[reiksmingu_indeksai]
      
      # VIF patikrinimas
      if (length(reiksmingi) > 1) {
        formule_vif <- as.formula(paste("log_Reiksme ~", paste(reiksmingi, collapse = " + ")))
        lm_vif <- lm(formule_vif, data = modelio_duomenys_std)
        
        tryCatch({
          vif_reiksmes <- vif(lm_vif)
          auksto_vif <- names(vif_reiksmes[vif_reiksmes > 10])
          if (length(auksto_vif) > 0) {
            reiksmingi <- setdiff(reiksmingi, auksto_vif)
          }
        }, error = function(e) {})
      }
      
      if (length(reiksmingi) > 0) {
        # Sukuriame atsitiktinių efektų modelį su atrinktais kintamaisiais
        formule_final <- as.formula(paste("log_Reiksme ~", paste(reiksmingi, collapse = " + ")))
        
        modelis_be_saveiku <- plm(formule_final, data = panel_data_std, model = "random", random.method = "amemiya")
        
        # Generuojame visas galimas dviejų kintamųjų sąveikų kombinacijas
        if (length(reiksmingi) >= 2) {
          kintamuju_kombinacijos <- combn(reiksmingi, 2)
          
          # Saugome reikšmingas sąveikas ir jų R² reikšmes
          reiksmingos_saveikos_kat <- c()
          saveiku_r2 <- c()
          
          # Tikriname kiekvieną sąveikos kombininaciją
          for (i in 1:ncol(kintamuju_kombinacijos)) {
            var1 <- kintamuju_kombinacijos[1, i]
            var2 <- kintamuju_kombinacijos[2, i]
            
            # Sąveikos formavimas
            saveikos_pavadinimas <- paste(var1, ":", var2, sep = "")
            
            # Formulės sudarymas su visais reikšmingais kintamaisiais ir viena sąveika
            formule_su_saveika <- as.formula(paste("log_Reiksme ~", paste(reiksmingi, collapse = " + "), 
                                                   " + ", saveikos_pavadinimas))
            
            tryCatch({
              # Atsitiktinių efektų modelis su visais reikšmingais kintamaisiais ir sąveika
              modelis_su_saveika <- plm(formule_su_saveika, data = panel_data_std, model = "random", random.method = "amemiya")
              santrauka_su_saveika <- summary(modelis_su_saveika)
              
              # Ieškome sąveikos kintamojo rezultatuose
              koeficientai_su_saveika <- santrauka_su_saveika$coefficients
              saveikos_eilute <- which(rownames(koeficientai_su_saveika) == saveikos_pavadinimas)
              
              # Jei sąveika rasta ir jos p < 0.05
              if (length(saveikos_eilute) > 0 && koeficientai_su_saveika[saveikos_eilute, 4] < 0.05) {
                saveika_info <- list(
                  saveika = saveikos_pavadinimas,
                  r2 = santrauka_su_saveika$r.squared[1],
                  modelis = modelis_su_saveika
                )
                
                reiksmingos_saveikos_kat <- c(reiksmingos_saveikos_kat, list(saveika_info))
                saveiku_r2 <- c(saveiku_r2, santrauka_su_saveika$r.squared[1])
              }
            }, error = function(e) {})
          }
          
          # Jei buvo rasta bent viena reikšminga sąveika
          if (length(reiksmingos_saveikos_kat) > 0) {
            # Atrenkame sąveiką su didžiausia R² reikšme
            r2_reiksmes <- sapply(reiksmingos_saveikos_kat, function(x) x$r2)
            geriausias_indeksas <- which.max(r2_reiksmes)
            geriausia_saveika <- reiksmingos_saveikos_kat[[geriausias_indeksas]]
            
            # Išsaugome modelį su geriausia sąveika
            modeliai_re_05_su_geriausia_saveika[[kat]] <- geriausia_saveika$modelis
          } else {
            # Išsaugome pradinį modelį be sąveikų
            modeliai_re_05_su_geriausia_saveika[[kat]] <- modelis_be_saveiku
          }
        } else {
          # Išsaugome pradinį modelį be sąveikų
          modeliai_re_05_su_geriausia_saveika[[kat]] <- modelis_be_saveiku
        }
        
        # Jei netikrinome sąveikų, išsaugome modelį be sąveikų
        if (!(kat %in% names(modeliai_re_05_su_geriausia_saveika))) {
          modeliai_re_05_su_geriausia_saveika[[kat]] <- modelis_be_saveiku
        }
      }
    }
  }, error = function(e) {})
}

# Galutinių modelių apžvalga
for (kat in names(modeliai_re_05_su_geriausia_saveika)) {
  modelis <- modeliai_re_05_su_geriausia_saveika[[kat]]
  print(summary(modelis))
}