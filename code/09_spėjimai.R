# bibliotekos
library(dplyr)
library(ggplot2)
library(plm)
library(tidyr)
library(gridExtra)
library(readr)

# Atrenkame tik tuos stulpelius, kurie mums reikalingi iš abiejų lentelių
duomenys_atrinkti <- duomenys %>%
  select(Šalis, Kategorija, Metai, Reikšmė, Priklausomas)

all_forecasts_atrinkti <- all_forecasts_df %>%
  select(Šalis, Kategorija, Metai, Reikšmė, Priklausomas)

# Alternatyvus būdas sujungti lenteles, kuris turėtų išvengti tipo nesuderinamumo
# Pirma konvertuosime visus stulpelius į tą patį tipą, o tada sujungsime
duomenys_str <- duomenys %>%
  mutate(across(everything(), as.character))

all_forecasts_str <- all_forecasts_df %>%
  mutate(across(everything(), as.character))

# Atrenkame tik reikalingus stulpelius
duomenys_atrinkti <- duomenys_str %>%
  select(Šalis, Kategorija, Metai, Reikšmė, Priklausomas)

all_forecasts_atrinkti <- all_forecasts_str %>%
  select(Šalis, Kategorija, Metai, Reikšmė, Priklausomas)

# Sujungiame duomenis
spejimo_duomenys <- bind_rows(duomenys_atrinkti, all_forecasts_atrinkti)

# Dabar konvertuojame į tinkamus tipus po sujungimo
spejimo_duomenys <- spejimo_duomenys %>%
  mutate(
    Metai = as.integer(Metai),
    Reikšmė = as.numeric(Reikšmė),
    Priklausomas = as.integer(Priklausomas)
  )

# Pašaliname lietuviškas raides ir pakeičiame tarpus į "_" simbolį Kategorija stulpelyje
spejimo_duomenys <- spejimo_duomenys %>%
  mutate(Kategorija = Kategorija %>%
           stri_trans_general("Latin-ASCII") %>%
           gsub(" ", "_", .)) %>%
  filter(Metai >= 2025 & Metai <= 2029)

# Surūšiuojame duomenis pagal Šalį, Kategoriją ir Metus (didėjančia tvarka)
spejimo_duomenys <- spejimo_duomenys %>%
  arrange(Šalis, Kategorija, Metai)

write.csv(spejimo_duomenys, "spejimo_duomenys.csv", row.names = FALSE)

# Funkcija prognozėms atlikti naudojant modelių koeficientus ir formules
prognozuoti_gerimus <- function() {
  # Pakrauname reikalingus paketus
  library(plm)
  library(dplyr)
  library(tidyr)
  
  # Nuskaitome spėjimo duomenis
  spejimo_duomenys <- read.csv("spejimo_duomenys.csv", stringsAsFactors = FALSE)
  
  # Spausdiname pirmas eilutes, kad patikrintume duomenų struktūrą
  print("Spėjimo duomenų pirmos eilutės:")
  print(head(spejimo_duomenys))
  
  # Sukuriame rezultatų duomenų rinkinį
  rezultatai <- data.frame()
  
  # SVARBU: Privalote patikrinti, ar modeliai yra prieinami aplinkoje
  print("Patikrinkime, ar modeliai yra prieinami:")
  modeliu_pvz <- tryCatch({
    # Jei modelis neprieinamas, bus klaida
    if (exists("model1_saveika")) {
      print(summary(model1_saveika))
      TRUE
    } else {
      print("model1_saveika neprieinamas")
      FALSE
    }
  }, error = function(e) {
    print(paste("Klaida ieškant modelio:", e$message))
    FALSE
  })
  
  if (!modeliu_pvz) {
    stop("Modeliai neprieinami. Įsitikinkite, kad pirmiausia įkėlėte ir sukūrėte visus modelius.")
  }
  
  # Kategorijų sąrašas
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
  
  # Paruošiame duomenis visų šalių ir metų kombinacijoms
  salys <- unique(spejimo_duomenys$Šalis)
  metai <- unique(spejimo_duomenys$Metai)
  print(paste("Unikalios šalys:", paste(salys, collapse=", ")))
  print(paste("Unikalūs metai:", paste(metai, collapse=", ")))
  
  # Konstruojame dataframe su nepriklausomais kintamaisiais
  nepriklausomi_duomenys <- spejimo_duomenys %>%
    filter(Priklausomas == 0) %>%  # Filtruojame tik nepriklausomus kintamuosius
    pivot_wider(names_from = Kategorija, values_from = Reikšmė)
  
  print("Nepriklausomų kintamųjų stulpeliai:")
  print(colnames(nepriklausomi_duomenys))
  
  # Funkcija standartizacijai - naudojame tą pačią metodologiją kaip modelių kūrimo metu
  # Standartizacija atliekama visai duomenų aibei, ne atskiram stebiniui
  visas_aibos_standartizuoti <- function(duomenys) {
    # Gauname visus stulpelius, išskyrus Šalis ir Metai
    kintamieji <- setdiff(colnames(duomenys), c("Šalis", "Metai"))
    
    # Standartizuojame kiekvieną kintamąjį
    for (var in kintamieji) {
      if (is.numeric(duomenys[[var]])) {
        # Pašaliname NA reikšmes prieš standartizaciją
        valid_values <- duomenys[[var]][!is.na(duomenys[[var]])]
        if (length(valid_values) > 0) {
          # Apskaičiuojame vidurkį ir standartinį nuokrypį
          mean_val <- mean(valid_values)
          sd_val <- sd(valid_values)
          if (sd_val > 0) { # Išvengiame dalybos iš nulio
            duomenys[[var]] <- (duomenys[[var]] - mean_val) / sd_val
          }
        }
      }
    }
    return(duomenys)
  }
  
  # Standartizuojame visus duomenis vienu metu
  nepriklausomi_duomenys_std <- visas_aibos_standartizuoti(nepriklausomi_duomenys)
  
  # Funkcija modelio gavimui pagal kategoriją
  gauti_modeli <- function(kategorija) {
    tryCatch({
      modelio_nr <- match(kategorija, kategorijos)
      modelio_var_name <- paste0("model", modelio_nr)
      
      # Taikome ypatingus atvejus
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
      
      # Tikriname, ar modelis egzistuoja
      if (!exists(modelio_var_name)) {
        stop(paste("Modelis", modelio_var_name, "neegzistuoja"))
      }
      
      # Gauname modelį
      get(modelio_var_name)
    }, error = function(e) {
      print(paste("Klaida gaunant modelį", kategorija, ":", e$message))
      NULL
    })
  }
  
  # Prognozuojame kiekvienai kategorijai
  for (kategorija in kategorijos) {
    tryCatch({
      cat("\nPrognozuojame kategoriją:", kategorija, "\n")
      
      # Gauname modelį
      modelis <- gauti_modeli(kategorija)
      if (is.null(modelis)) {
        cat("Praleista kategorija:", kategorija, "- Modelis neprieinamas\n")
        next
      }
      
      # Išgauname modelio formulę
      model_formula <- formula(modelis)
      
      # Spausdiname formulę derinimui
      cat("Modelio formulė:", deparse(model_formula), "\n")
      
      # Ištraukiame kintamųjų pavadinimus iš modelio formulės
      visi_kintamieji <- all.vars(model_formula)[-1]  # Pirmas kintamasis yra log_Reiksme, jo nereikia
      
      # Atskiriame sąveikos narius
      kintamieji <- character(0)
      saveikos <- character(0)
      
      for (term in visi_kintamieji) {
        if (grepl(":", term)) {
          saveikos <- c(saveikos, term)
        } else {
          kintamieji <- c(kintamieji, term)
        }
      }
      
      # Gauname modelio koeficientus
      koeficientai <- coef(modelis)
      cat("Modelio koeficientai:\n")
      print(koeficientai)
      
      # Gauname atsitiktinius efektus kiekvienai šaliai
      atsitiktiniai_efektai <- ranef(modelis)
      cat("Atsitiktiniai efektai:\n")
      print(str(atsitiktiniai_efektai))
      
      # Patikriname, kokie kintamieji yra nepriklausomų duomenų rinkinyje
      kintamieji_duomenyse <- intersect(kintamieji, colnames(nepriklausomi_duomenys_std))
      print(paste("Kintamieji, esantys duomenyse:", paste(kintamieji_duomenyse, collapse=", ")))
      
      # Tikriname, ar visi reikalingi kintamieji yra duomenyse
      truksta_kintamieji <- setdiff(kintamieji, colnames(nepriklausomi_duomenys_std))
      if (length(truksta_kintamieji) > 0) {
        print(paste("DĖMESIO: Trūksta kintamųjų:", paste(truksta_kintamieji, collapse=", ")))
      }
      
      # Iteruojame per kiekvieną šalį ir metus
      for (salis in salys) {
        for (metai_val in metai) {
          # Filtruojame duomenis tai šaliai ir metams
          duomenys_eilute <- nepriklausomi_duomenys_std %>%
            filter(Šalis == salis, Metai == metai_val)
          
          if (nrow(duomenys_eilute) > 0) {
            # Inicializuojame prognozę su konstantos koeficientu
            prognoze <- koeficientai["(Intercept)"]
            
            # Pridedame kintamųjų įtaką
            for (kintamasis in kintamieji) {
              if (kintamasis %in% names(koeficientai) && kintamasis %in% colnames(duomenys_eilute)) {
                prognoze <- prognoze + koeficientai[kintamasis] * duomenys_eilute[[kintamasis]][1]
              }
            }
            
            # Pridedame sąveikos narių įtaką
            for (saveika in saveikos) {
              terms <- strsplit(saveika, ":")[[1]]
              if (all(terms %in% colnames(duomenys_eilute))) {
                # Apskaičiuojame sąveikos reikšmę
                interakcija <- duomenys_eilute[[terms[1]]][1] * duomenys_eilute[[terms[2]]][1]
                if (saveika %in% names(koeficientai)) {
                  prognoze <- prognoze + koeficientai[saveika] * interakcija
                }
              }
            }
            
            # Pridedame šalies atsitiktinį efektą
            if (!is.null(atsitiktiniai_efektai) && salis %in% names(atsitiktiniai_efektai)) {
              salis_efektas <- atsitiktiniai_efektai[[salis]]
              if (!is.null(salis_efektas) && !is.na(salis_efektas)) {
                prognoze <- prognoze + salis_efektas
              }
            }
            
            # Konvertuojame atgal iš logaritminės skalės
            prognoze_orig <- exp(prognoze) - 1
            
            # Pridedame rezultatus
            rezultatai <- rbind(rezultatai, data.frame(
              Šalis = salis,
              Kategorija = kategorija,
              Metai = metai_val,
              Reikšmė = prognoze_orig
            ))
          }
        }
      }
    }, error = function(e) {
      cat("Klaida apdorojant kategoriją", kategorija, ":", e$message, "\n")
    })
  }
  
  # Jei nėra rezultatų, sukurkime tuščią dataframe
  if (nrow(rezultatai) == 0) {
    # Sukuriame visas kombinacijas
    kombinacijos <- expand.grid(
      Šalis = salys,
      Kategorija = kategorijos,
      Metai = metai,
      stringsAsFactors = FALSE
    )
    # Pridedame Reikšmė stulpelį su NA
    kombinacijos$Reikšmė <- NA
    rezultatai <- kombinacijos
  }
  
  # Išsaugome rezultatus
  write.csv(rezultatai, "prognozuoti_kiekiai.csv", row.names = FALSE)
  cat("Prognozavimas baigtas. Rezultatai išsaugoti faile 'prognozuoti_kiekiai.csv'.\n")
  cat("Eilučių skaičius rezultatuose:", nrow(rezultatai), "\n")
  cat("NA reikšmių skaičius:", sum(is.na(rezultatai$Reikšmė)), "\n")
  
  return(rezultatai)
}

prognozuoti_gerimus()