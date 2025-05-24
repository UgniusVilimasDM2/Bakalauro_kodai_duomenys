# Funkcija modeliui sukurti pagal kategoriją ir parodyti AIC bei BIC
create_and_evaluate_model <- function(kat, formula_text, modelio_pavadinimas) {
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
model1_saveika <- create_and_evaluate_model(
  "Gazuotas_vanduo_Kiekis", 
  "log_Reiksme ~ Populiacija + Mirtingumas + Diabeto_paplitimas + Nutukimas + Virssvoris + BVP_zmogui + Mirtingumas:BVP_zmogui",
  "model1_saveika"
)
ranef(model1_saveika)
residuals(model1_saveika)

# Modelis 2 - Vanduo_su_skoniu_Kiekis
model2_saveika <- create_and_evaluate_model(
  "Vanduo_su_skoniu_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Migracijos_rodiklis + Vidutine_temperatura + Diabeto_paplitimas + Nutukimas + Virssvoris + BVP_zmogui + Islaikomu_asmenu_santykis:Vidutine_temperatura",
  "model2_saveika"
)

# Modelis 3 - Funkcinis_vanduo_Kiekis
model3_saveika <- create_and_evaluate_model(
  "Funkcinis_vanduo_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Populiacijos_tankis + Diabeto_paplitimas + Nedarbingumas + BVP_zmogui + Populiacijos_tankis:Diabeto_paplitimas",
  "model3_saveika"
)

# Modelis 4 - Stalo_vanduo_Kiekis
model4_saveika <- create_and_evaluate_model(
  "Stalo_vanduo_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Populiacijos_tankis + Plotas + Vidutine_temperatura + Nutukimas + Virssvoris + BVP_zmogui + Islaikomu_asmenu_santykis:Nutukimas",
  "model4_saveika"
)

# Modelis 5 - Iprastine_kola_Kiekis
model5_saveika <- create_and_evaluate_model(
  "Iprastine_kola_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Populiacijos_tankis + Plotas + Nutukimas + Virssvoris + Nedarbingumas + BVP_zmogui + Populiacija:Islaikomu_asmenu_santykis",
  "model5_saveika"
)

# Modelis 6 - Kola_su_maziau_cukraus_Kiekis
model6_saveika <- create_and_evaluate_model(
  "Kola_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Populiacijos_tankis + Plotas + Nutukimas + Virssvoris + Nedarbingumas + BVP_zmogui + Populiacija:Islaikomu_asmenu_santykis",
  "model6_saveika"
)

# Modelis 7 - Iprasta_citrina/Laimas_Kiekis
model7_saveika <- create_and_evaluate_model(
  "Iprasta_citrina/Laimas_Kiekis", 
  "log_Reiksme ~ Mirtingumas + Populiacijos_tankis + Virssvoris + Nedarbingumas + BVP_zmogui + Mirtingumas:Nedarbingumas",
  "model7_saveika"
)

# Modelis 8 - Citrina/Laimas_su_maziau_cukraus_Kiekis
model8_saveika <- create_and_evaluate_model(
  "Citrina/Laimas_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Mirtingumas + Migracijos_rodiklis + Plotas + Vidutine_temperatura + Diabeto_paplitimas + Nutukimas + Virssvoris + BVP_zmogui + Diabeto_paplitimas:BVP_zmogui",
  "model8_saveika"
)

# Modelis 9 - Tonikai/Mikseriai_Kiekis
model9_saveika <- create_and_evaluate_model(
  "Tonikai/Mikseriai_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Mirtingumas + Populiacijos_tankis + Diabeto_paplitimas + Nutukimas + Virssvoris + BVP_zmogui + Populiacija:Nutukimas",
  "model9_saveika"
)

# nereikia # Modelis 10 - Tonikai/Mikseriai_su_maziau_cukraus_Kiekis
model10_saveika <- create_and_evaluate_model(
  "Tonikai/Mikseriai_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Mirtingumas + Populiacijos_tankis + Plotas + Vidutine_temperatura + Diabeto_paplitimas + Nutukimas + Virssvoris + BVP_zmogui + Populiacija:Islaikomu_asmenu_santykis",
  "model10_saveika"
)

# Modelis 11 - Iprastiniai_apelsinu_gerimai_Kiekis
model11_saveika <- create_and_evaluate_model(
  "Iprastiniai_apelsinu_gerimai_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Populiacijos_tankis + Virssvoris + Nedarbingumas + BVP_zmogui + Populiacija:Virssvoris",
  "model11_saveika"
)

# nereikia # Modelis 12 - Apelsinu_gerimai_su_maziau_cukraus_Kiekis
model12_saveika <- create_and_evaluate_model(
  "Apelsinu_gerimai_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Populiacijos_tankis + Vidutine_temperatura + Diabeto_paplitimas + BVP_zmogui + Islaikomu_asmenu_santykis:Vidutine_temperatura",
  "model12_saveika"
)

# nereikia # Modelis 13 - Kiti_iprasti_gerimai_Kiekis
model13_saveika <- create_and_evaluate_model(
  "Kiti_iprasti_gerimai_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Mirtingumas + Plotas + Vidutine_temperatura + Diabeto_paplitimas + Plotas:Vidutine_temperatura",
  "model13_saveika"
)

# nereikia # Modelis 14 - Kiti_gerimai_su_maziau_cukraus_Kiekis
model14_saveika <- create_and_evaluate_model(
  "Kiti_gerimai_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Mirtingumas + Populiacijos_tankis + Plotas + Vidutine_temperatura + Diabeto_paplitimas + Nutukimas + Virssvoris + Nedarbingumas + BVP_zmogui + Islaikomu_asmenu_santykis:Diabeto_paplitimas",
  "model14_saveika"
)

# Modelis 15 - Skysti_koncentratai_Kiekis
model15_saveika <- create_and_evaluate_model(
  "Skysti_koncentratai_Kiekis", 
  "log_Reiksme ~ Populiacija + Populiacijos_tankis + Vidutine_temperatura + Populiacija:Populiacijos_tankis",
  "model15_saveika"
)

# nereikia # Modelis 16 - Milteliu_koncentratai_Kiekis
model16_saveika <- create_and_evaluate_model(
  "Milteliu_koncentratai_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Mirtingumas + Plotas + Vidutine_temperatura + Diabeto_paplitimas + Plotas:Vidutine_temperatura",
  "model16_saveika"
)

# nereikia Modelis 17 - Grynosios_sultys_Kiekis
model17_saveika <- create_and_evaluate_model(
  "Grynosios_sultys_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Populiacijos_tankis + Plotas + Diabeto_paplitimas + BVP_zmogui + Populiacija:Plotas",
  "model17_saveika"
)

# nereikia # Modelis 18 - Kokosu/Augalu_vanduo_Kiekis
model18_saveika <- create_and_evaluate_model(
  "Kokosu/Augalu_vanduo_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Vidutine_temperatura + Diabeto_paplitimas + BVP_zmogui + Islaikomu_asmenu_santykis:Vidutine_temperatura",
  "model18_saveika"
)

# Modelis 19 - Sulciu_gerimai_Kiekis
model19_saveika <- create_and_evaluate_model(
  "Sulciu_gerimai_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Diabeto_paplitimas + Nedarbingumas + BVP_zmogui + Populiacija:BVP_zmogui",
  "model19_saveika"
)

# nereikia Modelis 20 - Nektarai_Kiekis
model20_saveika <- create_and_evaluate_model(
  "Nektarai_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Diabeto_paplitimas + Virssvoris + Diabeto_paplitimas:Virssvoris",
  "model20_saveika"
)

# nereikia # Modelis 21 - Paruosta_kava_Kiekis
model21_saveika <- create_and_evaluate_model(
  "Paruosta_kava_Kiekis", 
  "log_Reiksme ~ Populiacija + Diabeto_paplitimas + Nutukimas + Virssvoris + Nedarbingumas + Populiacija:Diabeto_paplitimas",
  "model21_saveika"
)

# nereikia # Modelis 22 - Gazuota_arbata/Kombuca_Kiekis
model22_saveika <- create_and_evaluate_model(
  "Gazuota_arbata/Kombuca_Kiekis", 
  "log_Reiksme ~ Populiacija + Populiacijos_tankis + Vidutine_temperatura + Diabeto_paplitimas + Nedarbingumas + BVP_zmogui + Nedarbingumas:BVP_zmogui",
  "model22_saveika"
)

# nereikia Modelis 23 - Iprasta_salta_arbata_Kiekis
model23_saveika <- create_and_evaluate_model(
  "Iprasta_salta_arbata_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Nutukimas + Virssvoris + Nedarbingumas + Islaikomu_asmenu_santykis:Nedarbingumas",
  "model23_saveika"
)

# Modelis 24 - Salta_arbata_su_maziau_cukraus_Kiekis (jei yra)
if(any(gerimu_duomenys$Kategorija == "Salta_arbata_su_maziau_cukraus_Kiekis")) {
  model24_saveika <- create_and_evaluate_model(
    "Salta_arbata_su_maziau_cukraus_Kiekis", 
    "log_Reiksme ~ Islaikomu_asmenu_santykis + Nutukimas + Nedarbingumas + Islaikomu_asmenu_santykis:Nedarbingumas",
    "model24_saveika"
  )
}
