# Funkcija modeliui sukurti pagal kategoriją su AIC ir BIC rodikliais
create_model_for_category <- function(kat, formula_text, modelio_pavadinimas) {
  # Filtruojame duomenis pagal kategoriją
  duom_kat <- gerimu_duomenys %>%
    filter(Kategorija == kat, !is.na(Reikšmė)) %>%
    mutate(log_Reiksme = log(Reikšmė + 1)) %>%
    dplyr::select(Šalis, Metai, log_Reiksme)
  
  # Sujungiame su populiacijos kintamaisiais
  modelio_duomenys <- left_join(duom_kat, pop_kintamieji, by = c("Šalis", "Metai"))
  
  # Konvertuojame į duomenų rinkinį
  modelio_duomenys_df <- as.data.frame(modelio_duomenys)
  
  # Gauname nepriklausomus kintamuosius
  nepriklausomi <- setdiff(colnames(modelio_duomenys), c("Šalis", "Metai", "log_Reiksme"))
  
  # Standartizuojame kintamuosius
  modelio_duomenys_std <- modelio_duomenys_df
  for (var in nepriklausomi) {
    if (is.numeric(modelio_duomenys_std[[var]])) {
      modelio_duomenys_std[[var]] <- scale(modelio_duomenys_std[[var]])
    }
  }
  
  # Sukuriame panel data objektą
  panel_data_std <- pdata.frame(modelio_duomenys_std, index = c("Šalis", "Metai"))
  
  # Sukuriame formulę iš teksto
  formula_obj <- as.formula(formula_text)
  
  # Sukuriame modelį
  model <- plm(formula_obj, data = panel_data_std, model = "random", random.method = "amemiya")
  
  # Gauname santrauką
  model_summary <- summary(model)
  
  # Apskaičiuojame rankiniu būdu AIC ir BIC
  n <- length(model$residuals)                    # Stebėjimų skaičius
  k <- length(coef(model))                       # Parametrų skaičius
  rss <- sum(model$residuals^2)                 # Liekamųjų paklaidų kvadratų suma
  sigma2 <- rss / n                             # Dispersijos įvertis
  
  # Logaritminė tikėtinumo funkcija
  loglik <- -n/2 * log(2 * pi) - n/2 * log(sigma2) - rss/(2 * sigma2)
  
  # AIC ir BIC skaičiavimas
  aic_value <- -2 * loglik + 2 * k
  bic_value <- -2 * loglik + log(n) * k
  
  # Išspausdiname informaciją
  cat("\n====================================================\n")
  cat("Modelis:", modelio_pavadinimas, "\n")
  cat("Kategorija:", kat, "\n")
  cat("Formulė:", formula_text, "\n")
  cat("AIC:", round(aic_value, 4), "\n")
  cat("BIC:", round(bic_value, 4), "\n")
  cat("====================================================\n\n")
  
  # Išspausdiname pilną santrauką
  print(model_summary)
  
  # Grąžiname modelį
  return(model)
}

# Modelis 1 - Gazuotas_vanduo_Kiekis
model1 <- create_model_for_category(
  "Gazuotas_vanduo_Kiekis", 
  "log_Reiksme ~ Nutukimas + Populiacija + Diabeto_paplitimas + Mirtingumas + BVP_zmogui + Virssvoris",
  "model1"
)

# Modelis 2 - Vanduo_su_skoniu_Kiekis
model2 <- create_model_for_category(
  "Vanduo_su_skoniu_Kiekis", 
  "log_Reiksme ~ BVP_zmogui + Nutukimas + Diabeto_paplitimas + Virssvoris + Migracijos_rodiklis",
  "model2"
)

# Modelis 3 - Funkcinis_vanduo_Kiekis
model3 <- create_model_for_category(
  "Funkcinis_vanduo_Kiekis", 
  "log_Reiksme ~ Diabeto_paplitimas + Islaikomu_asmenu_santykis + BVP_zmogui + Nedarbingumas + Populiacijos_tankis",
  "model3"
)

# Modelis 4 - Stalo_vanduo_Kiekis
model4 <- create_model_for_category(
  "Stalo_vanduo_Kiekis", 
  "log_Reiksme ~ Nutukimas + Populiacijos_tankis + BVP_zmogui + Islaikomu_asmenu_santykis + Vidutine_temperatura",
  "model4"
)

# Modelis 5 - Iprastine_kola_Kiekis
model5 <- create_model_for_category(
  "Iprastine_kola_Kiekis", 
  "log_Reiksme ~ Virssvoris + Nedarbingumas + Plotas + BVP_zmogui + Populiacijos_tankis",
  "model5"
)

# Modelis 6 - Kola_su_maziau_cukraus_Kiekis
model6 <- create_model_for_category(
  "Kola_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Nutukimas + Populiacijos_tankis + Islaikomu_asmenu_santykis + Plotas",
  "model6"
)

# Modelis 7 - Iprasta_citrina/Laimas_Kiekis
model7 <- create_model_for_category(
  "Iprasta_citrina/Laimas_Kiekis", 
  "log_Reiksme ~ Mirtingumas + Populiacijos_tankis + Virssvoris + Nedarbingumas + BVP_zmogui",
  "model7"
)

# Modelis 8 - Citrina/Laimas_su_maziau_cukraus_Kiekis
model8 <- create_model_for_category(
  "Citrina/Laimas_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Nutukimas + BVP_zmogui + Diabeto_paplitimas",
  "model8"
)

# Modelis 9 - Tonikai/Mikseriai_Kiekis
model9 <- create_model_for_category(
  "Tonikai/Mikseriai_Kiekis", 
  "log_Reiksme ~ Nutukimas + Islaikomu_asmenu_santykis + Virssvoris + Populiacija + Diabeto_paplitimas",
  "model9"
)

# Modelis 10 - Tonikai/Mikseriai_su_maziau_cukraus_Kiekis
model10 <- create_model_for_category(
  "Tonikai/Mikseriai_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Diabeto_paplitimas + Nutukimas + Nedarbingumas",
  "model10"
)

# Modelis 11 - Iprastiniai_apelsinu_gerimai_Kiekis
model11 <- create_model_for_category(
  "Iprastiniai_apelsinu_gerimai_Kiekis", 
  "log_Reiksme ~ Virssvoris + BVP_zmogui + Nedarbingumas + Populiacijos_tankis + Populiacija",
  "model11"
)

# Modelis 12 - Apelsinu_gerimai_su_maziau_cukraus_Kiekis
model12 <- create_model_for_category(
  "Apelsinu_gerimai_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Nutukimas + BVP_zmogui + Diabeto_paplitimas + Mirtingumas",
  "model12"
)

# Modelis 13 - Kiti_iprasti_gerimai_Kiekis
model13 <- create_model_for_category(
  "Kiti_iprasti_gerimai_Kiekis", 
  "log_Reiksme ~ Nutukimas + Mirtingumas + BVP_zmogui + Nedarbingumas + Plotas",
  "model13"
)

# Modelis 14 - Kiti_gerimai_su_maziau_cukraus_Kiekis
model14 <- create_model_for_category(
  "Kiti_gerimai_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Nutukimas + Populiacija + Islaikomu_asmenu_santykis + BVP_zmogui + Diabeto_paplitimas",
  "model14"
)

# Modelis 15 - Skysti_koncentratai_Kiekis
model15 <- create_model_for_category(
  "Skysti_koncentratai_Kiekis", 
  "log_Reiksme ~ Populiacija + Populiacijos_tankis + Vidutine_temperatura",
  "model15"
)

# Modelis 16 - Milteliu_koncentratai_Kiekis
model16 <- create_model_for_category(
  "Milteliu_koncentratai_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Mirtingumas + Plotas + Vidutine_temperatura + Diabeto_paplitimas",
  "model16"
)

# Modelis 17 - Grynosios_sultys_Kiekis
model17 <- create_model_for_category(
  "Grynosios_sultys_Kiekis", 
  "log_Reiksme ~ Virssvoris + Populiacija + Islaikomu_asmenu_santykis + Populiacijos_tankis + Nutukimas",
  "model17"
)

# Modelis 18 - Kokosu/Augalu_vanduo_Kiekis
model18 <- create_model_for_category(
  "Kokosu/Augalu_vanduo_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Populiacijos_tankis + Diabeto_paplitimas + Vidutine_temperatura + BVP_zmogui",
  "model18"
)

# Modelis 19 - Sulciu_gerimai_Kiekis
model19 <- create_model_for_category(
  "Sulciu_gerimai_Kiekis", 
  "log_Reiksme ~ Diabeto_paplitimas + Populiacija + Islaikomu_asmenu_santykis + BVP_zmogui + Plotas",
  "model19"
)

# Modelis 20 - Nektarai_Kiekis
model20 <- create_model_for_category(
  "Nektarai_Kiekis", 
  "log_Reiksme ~ Populiacija + BVP_zmogui + Nutukimas + Plotas + Nedarbingumas",
  "model20"
)

# Modelis 21 - Paruosta_kava_Kiekis
model21 <- create_model_for_category(
  "Paruosta_kava_Kiekis", 
  "log_Reiksme ~ BVP_zmogui + Nutukimas + Islaikomu_asmenu_santykis + Virssvoris + Diabeto_paplitimas",
  "model21"
)

# Modelis 22 - Gazuota_arbata/Kombuca_Kiekis
model22 <- create_model_for_category(
  "Gazuota_arbata/Kombuca_Kiekis", 
  "log_Reiksme ~ Nutukimas + Populiacija + Diabeto_paplitimas + Nedarbingumas + Virssvoris",
  "model22"
)

# Modelis 23 - Iprasta_salta_arbata_Kiekis
model23 <- create_model_for_category(
  "Iprasta_salta_arbata_Kiekis", 
  "log_Reiksme ~ Diabeto_paplitimas + Nedarbingumas + BVP_zmogui + Populiacijos_tankis + Populiacija",
  "model23"
)

# Modelis 24 - Salta_arbata_su_maziau_cukraus_Kiekis (jei yra)
if(any(gerimu_duomenys$Kategorija == "Salta_arbata_su_maziau_cukraus_Kiekis")) {
  model24 <- create_model_for_category(
    "Salta_arbata_su_maziau_cukraus_Kiekis", 
    "log_Reiksme ~ Islaikomu_asmenu_santykis + Nutukimas + Virssvoris + Nedarbingumas",
    "model24"
  )
}

