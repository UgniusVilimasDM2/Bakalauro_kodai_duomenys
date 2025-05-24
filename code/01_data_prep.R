# Išvalome darbo aplinką ir įkeliame bibliotekas
rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(Cairo)

# Nuskaitome gėrimų duomenis
soda_data <- read_excel(
  path = "final_soda.xlsx",
  sheet = 2,
  col_names = FALSE,     
  skip = 1               
)

# Nustatome stulpelių pavadinimus
col_names <- as.character(unlist(soda_data[1,]))
colnames(soda_data) <- col_names
soda_data <- soda_data[-c(1,2),]
rownames(soda_data) <- NULL

# Filtruojame ir transformuojame duomenis
data <- soda_data %>%
  filter(!(Subcategory %in% c("Regular Carbonates", "Reduced Sugar Carbonates", 
                              "Regular Non-Cola Carbonates", "Reduced Sugar Non-Cola Carbonates"))) %>%
  mutate(`Data Type` = case_when(
    `Data Type` == "Total Volume" ~ "Volume",
    `Data Type` == "Total Value RSP" ~ "Value",
    TRUE ~ `Data Type`
  )) %>%
  select(Country, Subcategory, `Data Type`, as.character(2010:2024)) %>%
  filter(!Country %in% c("Andorra", "Puerto Rico", "Papua New Guinea")) %>%
  filter(!is.na(Country)) %>%
  mutate(Subcategory = paste(Subcategory, `Data Type`, sep = " ")) %>%
  select(-`Data Type`)

# Šalių vertimo žodynas
saliu_vertimai <- data.frame(
  original = c("Armenia", "Mongolia", "Australia", "Bahamas", "Belize", 
               "Brazil", "Seychelles", "Canada", "USA", "France", 
               "Germany", "Iceland", "Luxembourg", "Switzerland", "Malta"),
  lietuviskai = c("Armėnija", "Mongolija", "Australija", "Bahamos", "Belizas", 
                  "Brazilija", "Seišeliai", "Kanada", "JAV", "Prancūzija", 
                  "Vokietija", "Islandija", "Liuksemburgas", "Šveicarija", "Malta")
)

# Kategorijų vertimo žodynas
kategoriju_vertimai <- data.frame(
  original = c(
    "Carbonated Bottled Water", "Flavoured Bottled Water", "Functional Bottled Water",
    "Still Bottled Water", "Regular Cola Carbonates", "Reduced Sugar Cola Carbonates",
    "Regular Lemonade/Lime", "Reduced Sugar Lemonade/Lime", 
    "Regular Tonic Water/Mixers/Other Bitters", "Reduced Sugar Tonic Water/Mixers/Other Bitters",
    "Regular Orange Carbonates", "Reduced Sugar Orange Carbonates",
    "Regular Other Non-Cola Carbonates", "Reduced Sugar Other Non-Cola Carbonates",
    "Liquid Concentrates", "Powder Concentrates", "100% Juice",
    "Coconut and Other Plant Waters", "Juice Drinks (up to 24% Juice)",
    "Nectars", "RTD Coffee", "Carbonated RTD Tea and Kombucha",
    "Regular Still RTD Tea", "Reduced Sugar Still RTD Tea"
  ),
  lietuviskai = c(
    "Gazuotas vanduo", "Vanduo su skoniu", "Funkcinis vanduo", "Stalo vanduo",
    "Įprastinė kola", "Kola su mažiau cukraus", "Įprasta citrina/Laimas",
    "Citrina/Laimas su mažiau cukraus", "Tonikai/Mikseriai",
    "Tonikai/Mikseriai su mažiau cukraus", "Įprastiniai apelsinų gėrimai",
    "Apelsinų gėrimai su mažiau cukraus", "Kiti įprasti gėrimai",
    "Kiti gėrimai su mažiau cukraus", "Skysti koncentratai", "Miltelių koncentratai",
    "Grynosios sultys", "Kokosų/Augalų vanduo", "Sulčių gėrimai", "Nektarai",
    "Paruošta kava", "Gazuota arbata/Kombuča", "Įprasta šalta arbata",
    "Šalta arbata su mažiau cukraus"
  )
)

# Lietuviško vertimo funkcija
translate_data <- function(data) {
  data_translated <- data %>%
    mutate(
      MeasureType = case_when(
        str_detect(Subcategory, "Value$") ~ "Vertė",
        str_detect(Subcategory, "Volume$") ~ "Kiekis",
        TRUE ~ NA_character_
      ),
      BaseCategory = str_remove(Subcategory, " ?(Value|Volume)$")
    )
  
  # Verčiame šalių ir kategorijų pavadinimus
  data_translated <- data_translated %>%
    mutate(
      Country = case_when(
        Country %in% saliu_vertimai$original ~ 
          saliu_vertimai$lietuviskai[match(Country, saliu_vertimai$original)],
        TRUE ~ Country
      ),
      BaseCategory_LT = case_when(
        BaseCategory %in% kategoriju_vertimai$original ~ 
          kategoriju_vertimai$lietuviskai[match(BaseCategory, kategoriju_vertimai$original)],
        TRUE ~ BaseCategory
      ),
      Subcategory_LT = paste0(BaseCategory_LT, " ", MeasureType)
    )
  
  return(data_translated)
}

# Verčiame duomenis į lietuvių kalbą
lietuviski_duomenys <- translate_data(data)

# NA reikšmių statistikos skaičiavimas
year_columns <- as.character(2010:2024)
rows_with_nas <- sum(apply(lietuviski_duomenys[, year_columns], 1, function(x) any(is.na(x))))
total_rows <- nrow(lietuviski_duomenys)
percentage_rows_with_nas <- (rows_with_nas / total_rows) * 100

total_cells <- total_rows * length(year_columns)
total_nas <- sum(is.na(lietuviski_duomenys[, year_columns]))
percentage_nas <- (total_nas / total_cells) * 100

nas_by_year <- colSums(is.na(lietuviski_duomenys[, year_columns]))
percentage_nas_by_year <- (nas_by_year / total_rows) * 100

summary_by_year <- data.frame(
  Metai = year_columns,
  NA_Kiekis = nas_by_year,
  NA_Procentai = round(percentage_nas_by_year, 2)
)

results <- list(
  total_rows = total_rows,
  rows_with_nas = rows_with_nas,
  percentage_rows_with_nas = round(percentage_rows_with_nas, 2),
  total_cells = total_cells,
  total_nas = total_nas,
  percentage_nas = round(percentage_nas, 2),
  summary_by_year = summary_by_year
)

print(results)

# NA reikšmių vizualizavimas
rows_with_nas <- which(apply(lietuviski_duomenys[, year_columns], 1, function(x) any(is.na(x))))
na_data <- lietuviski_duomenys[rows_with_nas, c("Country", "Subcategory_LT", "BaseCategory_LT", "MeasureType", year_columns)]

# Keičiame duomenų formatą vizualizacijai
na_data_long <- na_data %>%
  group_by(Country, Subcategory_LT, MeasureType) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols = all_of(year_columns),
    names_to = "Metai",
    values_to = "Reikšmė"
  ) %>%
  mutate(Metai = as.numeric(Metai))

# Kuriame grafikus pagal šalis
plots_by_country <- na_data_long %>%
  filter(!is.na(MeasureType)) %>%
  group_by(Country) %>%
  group_split() %>%
  map(function(country_data) {
    country_name <- unique(country_data$Country)
    
    country_data <- country_data %>%
      mutate(Legend_Label = paste0(BaseCategory_LT, " (", MeasureType, ")"))
    
    ggplot(country_data, aes(x = Metai, y = Reikšmė, 
                             group = interaction(Subcategory_LT, row_id, MeasureType),
                             color = Legend_Label)) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      geom_point(data = filter(country_data, is.na(Reikšmė)), 
                 aes(x = Metai), y = 0, shape = 4, size = 3) +
      scale_x_continuous(breaks = seq(2010, 2024, by = 1)) +
      scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
      labs(
        title = paste("Duomenys su trūkstamomis reikšmėmis:", country_name),
        subtitle = "Abiejų matavimo tipų duomenys (Vertė ir Kiekis)",
        x = "Metai",
        y = "Reikšmė"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })

# Išsaugome trūkstamų reikšmių grafikus
CairoPDF("Trukstamos_reiksmes.pdf", width = 12, height = 8)
for(plot in plots_by_country) {
  print(plot)
}
dev.off()

# Pakeičiame NA reikšmes nuliais
lietuviski_duomenys <- lietuviski_duomenys %>%
  mutate(across(all_of(year_columns), ~ifelse(is.na(.), 0, .)))

# Funkcijos nulinių reikšmių vizualizavimui
reshape_for_comparison <- function(data) {
  data %>%
    select(Country, BaseCategory, BaseCategory_LT, MeasureType, all_of(year_columns)) %>%
    pivot_longer(
      cols = all_of(year_columns),
      names_to = "Metai",
      values_to = "Reikšmė"
    ) %>%
    mutate(Metai = as.numeric(Metai))
}

create_comparison_plots <- function(long_data) {
  combinations_with_zeros <- long_data %>%
    group_by(Country, BaseCategory, BaseCategory_LT) %>%
    filter(any(Reikšmė == 0, na.rm = TRUE)) %>%
    select(Country, BaseCategory, BaseCategory_LT) %>%
    distinct()
  
  plots <- list()
  
  for(i in 1:nrow(combinations_with_zeros)) {
    country <- combinations_with_zeros$Country[i]
    subcategory <- combinations_with_zeros$BaseCategory[i]
    subcategory_lt <- combinations_with_zeros$BaseCategory_LT[i]
    
    subset_data <- long_data %>%
      filter(Country == country & BaseCategory == subcategory)
    
    if(all(c("Vertė", "Kiekis") %in% unique(subset_data$MeasureType))) {
      p <- ggplot(subset_data, aes(x = Metai, y = Reikšmė, 
                                   group = interaction(MeasureType),
                                   color = MeasureType)) +
        geom_line() +
        geom_point() +
        geom_point(data = filter(subset_data, Reikšmė == 0), 
                   aes(x = Metai), y = 0, shape = 4, size = 3, color = "red") +
        scale_x_continuous(breaks = seq(2010, 2024, by = 1)) +
        scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
        labs(
          title = paste(country, "-", subcategory_lt),
          subtitle = "Nulinės reikšmės pažymėtos raudonais X",
          x = "Metai",
          y = "Reikšmė"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
      plot_id <- paste(country, subcategory, sep = " ")
      plots[[plot_id]] <- p
    }
  }
  
  return(plots)
}

compare_volume_value <- function(data) {
  long_data <- reshape_for_comparison(data)
  plots <- create_comparison_plots(long_data)
  
  summary <- long_data %>%
    group_by(Country, BaseCategory, BaseCategory_LT, MeasureType) %>%
    summarize(
      zero_count = sum(Reikšmė == 0, na.rm = TRUE),
      na_count = sum(is.na(Reikšmė)),
      total_years = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      id_cols = c(Country, BaseCategory, BaseCategory_LT),
      names_from = MeasureType,
      values_from = c(zero_count, na_count)
    ) %>%
    mutate(
      has_mismatched_zeros = zero_count_Kiekis != zero_count_Vertė,
      description = case_when(
        zero_count_Kiekis > 0 & zero_count_Vertė > 0 ~ "Abu turi nulius",
        zero_count_Kiekis > 0 ~ "Kiekis turi nulius",
        zero_count_Vertė > 0 ~ "Vertė turi nulius",
        TRUE ~ "Nėra nulių"
      )
    ) %>%
    arrange(desc(has_mismatched_zeros))
  
  return(list(plots = plots, summary = summary))
}

# Analizuojame nulines reikšmes
results <- compare_volume_value(lietuviski_duomenys)

# Išsaugome nulinių reikšmių grafikus
CairoPDF("Nulines_reiksmes.pdf", width = 12, height = 8)
for(plot in head(results$plots, 28)) {
  print(plot)
}
dev.off()

# Funkcija nelogiškų nulių koregavimui
fix_inconsistent_volume_data <- function(data) {
  data_processed <- data
  year_columns <- grep("^\\d{4}$", names(data_processed), value = TRUE)
  
  # Transformuojame į ilgą formatą
  data_long <- data_processed %>%
    pivot_longer(
      cols = all_of(year_columns),
      names_to = "Metai",
      values_to = "Reikšmė"
    ) %>%
    mutate(Metai = as.integer(Metai))
  
  # Pertvarkome į platų formatą pagal matavimo tipą
  data_wide <- data_long %>%
    pivot_wider(
      id_cols = c(Country, BaseCategory, BaseCategory_LT, Metai),
      names_from = MeasureType,
      values_from = Reikšmė
    )
  
  # Randame nesuderintus įrašus
  inconsistent_entries <- data_wide %>%
    filter(Vertė > 0 & Kiekis == 0) %>%
    arrange(Country, BaseCategory, Metai)
  
  cat("Rasta", nrow(inconsistent_entries), "nesuderintų įrašų (vertė > 0, bet kiekis = 0)\n")
  
  # Skaičiuojame santykius koregavimui
  valid_pairs <- data_wide %>%
    filter(Vertė > 0 & Kiekis > 0)
  
  if (nrow(valid_pairs) > 0) {
    ratios <- valid_pairs %>%
      group_by(Country, BaseCategory, BaseCategory_LT) %>%
      summarize(
        avg_ratio = mean(Vertė / Kiekis, na.rm = TRUE),
        min_ratio = min(Vertė / Kiekis, na.rm = TRUE),
        max_ratio = max(Vertė / Kiekis, na.rm = TRUE),
        entries = n(),
        .groups = "drop"
      )
    
    inconsistent_with_ratios <- inconsistent_entries %>%
      left_join(ratios, by = c("Country", "BaseCategory", "BaseCategory_LT"))
    
    global_category_ratios <- valid_pairs %>%
      group_by(BaseCategory, BaseCategory_LT) %>%
      summarize(
        global_category_ratio = mean(Vertė / Kiekis, na.rm = TRUE),
        entries = n(),
        .groups = "drop"
      )
    
    global_ratio <- mean(valid_pairs$Vertė / valid_pairs$Kiekis, na.rm = TRUE)
  } else {
    global_ratio <- 2.0
    inconsistent_with_ratios <- inconsistent_entries
    global_category_ratios <- data.frame(
      BaseCategory = character(), 
      BaseCategory_LT = character(),
      global_category_ratio = numeric(), 
      entries = integer()
    )
  }
  
  # Taikome pataisymus
  fixes <- inconsistent_with_ratios %>%
    left_join(global_category_ratios, by = c("BaseCategory", "BaseCategory_LT")) %>%
    mutate(
      estimated_volume = case_when(
        !is.na(avg_ratio) ~ Vertė / avg_ratio,
        !is.na(global_category_ratio) ~ Vertė / global_category_ratio,
        TRUE ~ Vertė / global_ratio
      ),
      fix_method = case_when(
        !is.na(avg_ratio) ~ "šalies_kategorijos_santykis",
        !is.na(global_category_ratio) ~ "pasaulinis_kategorijos_santykis",
        TRUE ~ "pasaulinis_santykis"
      )
    )
  
  fix_lookup <- fixes %>%
    select(Country, BaseCategory, BaseCategory_LT, Metai, estimated_volume)
  
  # Pritaikome pataisymus
  fixed_data_long <- data_long %>%
    left_join(fix_lookup, by = c("Country", "BaseCategory", "BaseCategory_LT", "Metai")) %>%
    mutate(
      Reikšmė = case_when(
        MeasureType == "Kiekis" & Reikšmė == 0 & !is.na(estimated_volume) ~ estimated_volume,
        TRUE ~ Reikšmė
      )
    ) %>%
    select(-estimated_volume)
  
  # Grąžiname į platų formatą
  fixed_data_wide <- fixed_data_long %>%
    pivot_wider(
      id_cols = c(Country, Subcategory, Subcategory_LT, BaseCategory, BaseCategory_LT, MeasureType),
      names_from = Metai,
      values_from = Reikšmė
    )
  
  fixed_data_numeric <- fixed_data_wide %>%
    mutate(across(all_of(year_columns), ~round(., 2)))
  
  return(fixed_data_numeric)
}

# Taisome nesuderintus duomenis
fixed_lietuviski_duomenys <- fix_inconsistent_volume_data(lietuviski_duomenys)

# Vizualizacijos funkcija
create_plots <- function(data) {
  CairoPDF("Gaiviuju_Gerimu_Tendencijos.pdf", width = 12, height = 8)
  
  plot_count <- 0
  countries <- unique(data$Country)
  base_subcats <- unique(data$BaseCategory_LT)
  year_cols <- as.character(2010:2024)
  
  for (country in countries) {
    country_data <- data %>% filter(Country == country)
    if (nrow(country_data) == 0) next
    
    for (base_subcat in base_subcats) {
      subcat_data <- country_data %>%
        filter(BaseCategory_LT == base_subcat)
      
      if (nrow(subcat_data) < 2) next
      
      plot_data <- subcat_data %>%
        select(Country, BaseCategory_LT, MeasureType, all_of(year_cols)) %>%
        pivot_longer(cols = all_of(year_cols), 
                     names_to = "Metai", 
                     values_to = "Reikšmė") %>%
        mutate(Metai = as.numeric(Metai))
      
      p <- ggplot(plot_data, aes(x = Metai, y = Reikšmė, 
                                 group = interaction(MeasureType),
                                 color = MeasureType)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = seq(2010, 2024, by = 1)) +
        scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
        labs(
          title = paste0(country, " - ", base_subcat),
          subtitle = "Vertės ir kiekio tendencijos (2010-2024)",
          x = "Metai",
          y = "Reikšmė"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
      print(p)
      plot_count <- plot_count + 1
      cat(sprintf("Sukurtas grafikas %d: %s - %s\n", plot_count, country, base_subcat))
    }
  }
  
  dev.off()
  return(plot_count)
}

total_plots <- create_plots(fixed_lietuviski_duomenys)
cat(sprintf("\nPDF generavimas baigtas. Sukurta %d grafikų.\n", total_plots))

# Paruošiame galutinį duomenų rinkinį
options(scipen = 999)

Galutinis1 <- fixed_lietuviski_duomenys %>%
  mutate(across(
    .cols = all_of(as.character(2010:2024)),
    .fns = ~ .* 1000000
  )) %>%  
  rename(
    Kategorija = Subcategory_LT,
    Šalis = Country
  ) %>%
  select(Šalis, Kategorija, as.character(2010:2024))

# ANTRAS DUOMENŲ RINKINYS - ŠALIŲ STATISTIKA
library(corrplot)
library(ggplot2)
library(reshape2)

# Nuskaitome pirmus šalių duomenis
countries1 <- read_excel(
  path = "final_countries.xlsx",
  sheet = 2,
  col_names = FALSE,     
  skip = 1               
)

col_names1 <- as.character(unlist(countries1[1,]))
colnames(countries1) <- col_names1
countries <- countries1[-c(1,2),]
rownames(countries1) <- NULL

data2 <- countries %>%
  select(Country, Subcategory, Unit, as.character(2010:2024)) %>%
  filter(!Country %in% c("Andorra", "Puerto Rico", "Papua New Guinea")) %>%
  filter(!is.na(Country)) %>%
  filter(!Subcategory %in% c("% of Population Aged 15+ with Higher Education",
                             "Foreign Citizens", "Index of Food and Non-Alcoholic Beverage Prices",
                             "Possession of Mobile Telephone", "Possession of Personal Computer",
                             "Consumer Expenditure on Food and Non-Alcoholic Beverages",
                             "Disposable Income", "Imports", "Exports"))

# Nuskaitome antrus šalių duomenis
countries2 <- read_excel(
  path = "countries3.xlsx",
  sheet = 2,
  col_names = FALSE,     
  skip = 1               
)

col_names1 <- as.character(unlist(countries2[1,]))
colnames(countries2) <- col_names1
countries1 <- countries2[-c(1,2),]
rownames(countries2) <- NULL

data3 <- countries1 %>%
  select(Country, Subcategory, Unit, as.character(2010:2024)) %>%
  filter(!Country %in% c("Andorra", "Puerto Rico", "Papua New Guinea")) %>%
  filter(!is.na(Country)) %>%
  filter(!Subcategory %in% c("Foreign Citizens", "Dental Health", "Imports", "Exports"))

# Sujungiame duomenis ir pašaliname dublikatus
categories_to_deduplicate <- c(
  "Inflation", "Total Population", "Male Population", "Female Population", 
  "Population Density", "Birth Rates", "Death Rates", "Diabetes Prevalence", 
  "Obese Population", "Overweight Population", "Net Migration Rate", "Land Area", 
  "Average Household Size", "Percentage of Households with Access to Internet", 
  "GDP", "Unemployed Population", "Wage per Hour", "Mean Temperature", "Dependency Ratio"
)

data_pop <- bind_rows(data2, data3) %>%
  arrange(Country) %>%
  group_by(Country, Subcategory) %>%
  filter(!(Subcategory %in% categories_to_deduplicate) | row_number() == 1) %>%
  ungroup()

# Vertimo žodynai
country_translations <- data.frame(
  original = c("Armenia", "Mongolia", "Australia", "Bahamas", "Belize", 
               "Brazil", "Seychelles", "Canada", "USA", "France", 
               "Germany", "Iceland", "Luxembourg", "Switzerland", "Malta"),
  lietuviskai = c("Armėnija", "Mongolija", "Australija", "Bahamos", "Belizas", 
                  "Brazilija", "Seišeliai", "Kanada", "JAV", "Prancūzija", 
                  "Vokietija", "Islandija", "Liuksemburgas", "Šveicarija", "Malta")
)

category_translations <- data.frame(
  original = c(
    "Inflation", "Total Population", "Male Population", "Female Population", 
    "Population Density", "Birth Rates", "Death Rates", "Diabetes Prevalence", 
    "Obese Population (BMI 30kg/sq m or More)", "Overweight Population (BMI 25-30kg/sq m)",
    "Net Migration Rate", "Land Area", "Average Household Size", 
    "Percentage of Households with Access to Internet", "GDP", 
    "Unemployed Population", "Wage per Hour", "Mean Temperature", "Dependency Ratio"
  ),
  lietuviskai = c(
    "Infliacija", "Populiacija", "Vyrų populiacija", "Moterų populiacija", 
    "Populiacijos tankis", "Gimstamumas", "Mirtingumas", "Diabeto paplitimas", 
    "Nutukimas", "Viršsvoris", "Migracijos rodiklis", "Plotas", "Namų ūkio dydis", 
    "Namų ūkiai su internetu", "BVP", "Nedarbingumas", "Valandinis atlyginimas", 
    "Vidutinė temperatūra", "Išlaikomų asmenų santykis"
  )
)

# Vertimo funkcija
translate_data <- function(df, country_col = "Country", category_col = "Subcategory") {
  result_df <- df
  
  if(country_col %in% names(df)) {
    country_lookup <- function(x) {
      idx <- match(x, country_translations$original)
      ifelse(is.na(idx), x, country_translations$lietuviskai[idx])
    }
    result_df[[country_col]] <- sapply(df[[country_col]], country_lookup)
  }
  
  if(category_col %in% names(df)) {
    category_lookup <- function(x) {
      idx <- match(x, category_translations$original)
      ifelse(is.na(idx), x, category_translations$lietuviskai[idx])
    }
    result_df[[category_col]] <- sapply(df[[category_col]], category_lookup)
  }
  
  return(result_df)
}

# Verčiame duomenis
translated_data <- translate_data(data_pop)

# Transformuojame vienetų reikšmes
options(scipen = 999)

Galutinis2 <- translated_data %>%
  mutate(across(
    .cols = all_of(as.character(2010:2024)),
    .fns = ~case_when(
      Unit == '000' ~ . * 1000,
      Unit == 'EUR million' ~ . * 1000000,
      Unit == '000 sq km' ~ . * 1000,
      T ~ .
    )
  )) %>%
  mutate(Unit = case_when(
    Unit == '000' ~ 'real',
    Unit == 'EUR million' ~ 'EUR',
    Unit == '000 sq km' ~ 'sq km',
    T ~ Unit
  )) %>%
  rename(
    Šalis = Country,
    Kategorija = Subcategory,
    Vienetai = Unit
  )

# Skaičiuojame išvestinius parametrus
Galutinis2_long <- Galutinis2 %>%
  pivot_longer(
    cols = `2010`:`2024`,
    names_to = "Year",
    values_to = "Value"
  )

# Gauname populiacijos duomenis
populiacija_by_country_year <- Galutinis2_long %>%
  filter(Kategorija == "Populiacija") %>%
  select(Šalis, Year, Population = Value)

# Perskaičiuojame nedarbingumo procentą
nedarbingumas_by_country_year <- Galutinis2_long %>%
  filter(Kategorija == "Nedarbingumas")

nedarbingumas_percentage <- nedarbingumas_by_country_year %>%
  left_join(populiacija_by_country_year, by = c("Šalis", "Year")) %>%
  mutate(
    Value = (Value / Population) * 100,
    Vienetai = "%"
  ) %>%
  select(-Population) %>%
  rename(Metai = Year, Reikšmė = Value)

# Skaičiuojame BVP vienam žmogui
bvp_by_country_year <- Galutinis2_long %>%
  filter(Kategorija == "BVP")

bvp_per_capita <- bvp_by_country_year %>%
  left_join(populiacija_by_country_year, by = c("Šalis", "Year")) %>%
  mutate(
    Kategorija = "BVP žmogui",
    Value = Value / Population
  ) %>%
  select(-Population) %>%
  rename(Metai = Year, Reikšmė = Value)

# Atnaujiname duomenis su perskaičiuotomis reikšmėmis
Galutinis2_updated <- Galutinis2_long %>%
  rename(Metai = Year, Reikšmė = Value) %>%
  filter(Kategorija != "Nedarbingumas") %>%
  bind_rows(nedarbingumas_percentage) %>%
  bind_rows(bvp_per_capita) %>%
  arrange(Šalis)

# Paruošiame koreliacijos analizei
wide_data <- Galutinis2_updated %>%
  filter(!is.na(Reikšmė)) %>%
  select(-Vienetai) %>%
  pivot_wider(names_from = Kategorija, values_from = Reikšmė)

# Apskaičiuojame koreliacijos matricą
numeric_data <- wide_data %>%
  select(-Šalis, -Metai) %>%
  select(where(is.numeric)) %>%
  select_if(~sd(., na.rm = TRUE) > 0)

global_corr <- cor(numeric_data, use = "pairwise.complete.obs")

# Išsaugome koreliacijos matricą
pdf("global_correlation_matrix.pdf", width = 10, height = 8)
corrplot(global_corr,
         method = "color",
         type = "upper",
         tl.cex = 0.7,
         number.cex = 0.7,
         addCoef.col = "black",
         title = "Vidutinė koreliacija tarp kintamųjų",
         mar = c(0, 0, 2, 0))
dev.off()

# Sujungiame abu duomenų rinkinius
Galutinis1_long <- Galutinis1 %>%
  pivot_longer(
    cols = `2010`:`2024`,
    names_to = "Metai",
    values_to = "Reikšmė"
  ) %>%
  mutate(Metai = as.character(Metai))

Galutinis2_updated <- Galutinis2_updated %>%
  mutate(Metai = as.character(Metai))

combined_data <- bind_rows(Galutinis1_long, Galutinis2_updated)

# Filtruojame nereikalingas kategorijas
combined_data_final <- combined_data %>%
  filter(!Kategorija %in% c("BVP", "Vyrų populiacija", "Moterų populiacija", 
                            "Namų ūkiai su internetu", "Valandinis atlyginimas")) %>%
  select(-Vienetai) %>%
  mutate(
    Priklausomas = ifelse(str_ends(Kategorija, "Kiekis"), 1, 0),
    Kintamasis = ifelse(str_ends(Kategorija, "Vertė") | str_ends(Kategorija, "Kiekis"), 0, 1)
  )

# Išsaugome galutinį rezultatą
library(openxlsx)
write.xlsx(combined_data_final, "combined_data_final.xlsx")