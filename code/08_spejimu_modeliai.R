# Funkcija modeliui sukurti pagal kategoriją su AIC ir BIC rodikliais
create <- function(kat, formula_text, modelio_pavadinimas) {
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
  
  cat("Kategorija:", kat, "\n")
  
  # Išspausdiname pilną santrauką
  print(model_summary)
  
  # Grąžiname modelį
  return(model)
}

# Modelis 1 - Gazuotas_vanduo_Kiekis
model1_saveika <- create(
  "Gazuotas_vanduo_Kiekis", 
  "log_Reiksme ~ Populiacija + Mirtingumas + Diabeto_paplitimas + Nutukimas + Virssvoris + BVP_zmogui + Mirtingumas:BVP_zmogui",
  "model1_saveika"
)

# Modelis 2 - Vanduo_su_skoniu_Kiekis
model2_saveika <- create(
  "Vanduo_su_skoniu_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Migracijos_rodiklis + Vidutine_temperatura + Diabeto_paplitimas + Nutukimas + Virssvoris + BVP_zmogui + Islaikomu_asmenu_santykis:Vidutine_temperatura",
  "model2_saveika"
)

# Modelis 3 - Funkcinis_vanduo_Kiekis
model3_saveika <- create(
  "Funkcinis_vanduo_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Populiacijos_tankis + Diabeto_paplitimas + Nedarbingumas + BVP_zmogui + Populiacijos_tankis:Diabeto_paplitimas",
  "model3_saveika"
)

# Modelis 4 - Stalo_vanduo_Kiekis
model4_saveika <- create(
  "Stalo_vanduo_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Populiacijos_tankis + Plotas + Vidutine_temperatura + Nutukimas + Virssvoris + BVP_zmogui + Islaikomu_asmenu_santykis:Nutukimas",
  "model4_saveika"
)

# Modelis 5 - Iprastine_kola_Kiekis
model5_saveika <- create(
  "Iprastine_kola_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Populiacijos_tankis + Plotas + Nutukimas + Virssvoris + Nedarbingumas + BVP_zmogui + Populiacija:Islaikomu_asmenu_santykis",
  "model5_saveika"
)

# Modelis 6 - Kola_su_maziau_cukraus_Kiekis
model6_saveika <- create(
  "Kola_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Populiacijos_tankis + Plotas + Nutukimas + Virssvoris + Nedarbingumas + BVP_zmogui + Populiacija:Islaikomu_asmenu_santykis",
  "model6_saveika"
)

# Modelis 7 - Iprasta_citrina/Laimas_Kiekis
model7_saveika <- create(
  "Iprasta_citrina/Laimas_Kiekis", 
  "log_Reiksme ~ Mirtingumas + Populiacijos_tankis + Virssvoris + Nedarbingumas + BVP_zmogui + Mirtingumas:Nedarbingumas",
  "model7_saveika"
)

# Modelis 8 - Citrina/Laimas_su_maziau_cukraus_Kiekis
model8_saveika <- create(
  "Citrina/Laimas_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Mirtingumas + Migracijos_rodiklis + Plotas + Vidutine_temperatura + Diabeto_paplitimas + Nutukimas + Virssvoris + BVP_zmogui + Diabeto_paplitimas:BVP_zmogui",
  "model8_saveika"
)

# Modelis 9 - Tonikai/Mikseriai_Kiekis
model9_saveika <- create(
  "Tonikai/Mikseriai_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Mirtingumas + Populiacijos_tankis + Diabeto_paplitimas + Nutukimas + Virssvoris + BVP_zmogui + Populiacija:Nutukimas",
  "model9_saveika"
)

# Modelis 10 - Tonikai/Mikseriai_su_maziau_cukraus_Kiekis
model10 <- create(
  "Tonikai/Mikseriai_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Diabeto_paplitimas + Nutukimas + Nedarbingumas",
  "model10"
)

# Modelis 11 - Iprastiniai_apelsinu_gerimai_Kiekis
model11_saveika <- create(
  "Iprastiniai_apelsinu_gerimai_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Populiacijos_tankis + Virssvoris + Nedarbingumas + BVP_zmogui + Populiacija:Virssvoris",
  "model11_saveika"
)

# Modelis 12 - Apelsinu_gerimai_su_maziau_cukraus_Kiekis
model12 <- create(
  "Apelsinu_gerimai_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Populiacija + Nutukimas + BVP_zmogui + Diabeto_paplitimas + Mirtingumas",
  "model12"
)


# Modelis 13 - Kiti_iprasti_gerimai_Kiekis
model13 <- create(
  "Kiti_iprasti_gerimai_Kiekis", 
  "log_Reiksme ~ Nutukimas + Mirtingumas + BVP_zmogui + Nedarbingumas + Plotas",
  "model13"
)

# Modelis 14 - Kiti_gerimai_su_maziau_cukraus_Kiekis
model14 <- create(
  "Kiti_gerimai_su_maziau_cukraus_Kiekis", 
  "log_Reiksme ~ Nutukimas + Populiacija + Islaikomu_asmenu_santykis + BVP_zmogui + Diabeto_paplitimas",
  "model14"
)

# Modelis 15 - Skysti_koncentratai_Kiekis
model15_saveika <- create(
  "Skysti_koncentratai_Kiekis", 
  "log_Reiksme ~ Populiacija + Populiacijos_tankis + Vidutine_temperatura + Populiacija:Populiacijos_tankis",
  "model15_saveika"
)

# Modelis 16 - Milteliu_koncentratai_Kiekis
model16 <- create(
  "Milteliu_koncentratai_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Mirtingumas + Plotas + Vidutine_temperatura + Diabeto_paplitimas",
  "model16"
)

# Modelis 17 - Grynosios_sultys_Kiekis
model17 <- create(
  "Grynosios_sultys_Kiekis", 
  "log_Reiksme ~ Virssvoris + Populiacija + Islaikomu_asmenu_santykis + Populiacijos_tankis + Nutukimas",
  "model17"
)

# Modelis 18 - Kokosu/Augalu_vanduo_Kiekis
model18 <- create(
  "Kokosu/Augalu_vanduo_Kiekis", 
  "log_Reiksme ~ Islaikomu_asmenu_santykis + Populiacijos_tankis + Diabeto_paplitimas + Vidutine_temperatura + BVP_zmogui",
  "model18"
)

# Modelis 19 - Sulciu_gerimai_Kiekis
model19_saveika <- create(
  "Sulciu_gerimai_Kiekis", 
  "log_Reiksme ~ Populiacija + Islaikomu_asmenu_santykis + Diabeto_paplitimas + Nedarbingumas + BVP_zmogui + Populiacija:BVP_zmogui",
  "model19_saveika"
)

# Modelis 20 - Nektarai_Kiekis
model20 <- create(
  "Nektarai_Kiekis", 
  "log_Reiksme ~ Populiacija + BVP_zmogui + Nutukimas + Plotas + Nedarbingumas",
  "model20"
)

# Modelis 21 - Paruosta_kava_Kiekis
model21 <- create(
  "Paruosta_kava_Kiekis", 
  "log_Reiksme ~ BVP_zmogui + Nutukimas + Islaikomu_asmenu_santykis + Virssvoris + Diabeto_paplitimas",
  "model21"
)

# Modelis 22 - Gazuota_arbata/Kombuca_Kiekis
model22 <- create(
  "Gazuota_arbata/Kombuca_Kiekis", 
  "log_Reiksme ~ Nutukimas + Populiacija + Diabeto_paplitimas + Nedarbingumas + Virssvoris",
  "model22"
)

# Modelis 23 - Iprasta_salta_arbata_Kiekis
model23 <- create(
  "Iprasta_salta_arbata_Kiekis", 
  "log_Reiksme ~ Diabeto_paplitimas + Nedarbingumas + BVP_zmogui + Populiacijos_tankis + Populiacija",
  "model23"
)

# Modelis 24 - Salta_arbata_su_maziau_cukraus_Kiekis (jei yra)
model24_saveika <- create(
    "Salta_arbata_su_maziau_cukraus_Kiekis", 
    "log_Reiksme ~ Islaikomu_asmenu_santykis + Nutukimas + Nedarbingumas + Islaikomu_asmenu_santykis:Nedarbingumas",
    "model24_saveika"
  )
