### Yarfraz Analysis 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(visdat)
library(psych)



# data --------------------------------------------------------------------

df <- read_delim("data/dataset_ie_ansiedad_ccs_vzla_202105.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
df <- df1

# Data scanning -----------------------------------------------------------

colnames(df)

### some names will be changed 

str(df)

### all factor cols are character when they must be numbers. This is due to decimal expression using comma


### NA

vis_miss(df)
  # 8.1% missing 



df_na <- df %>% 
  select(1:ncol(df)) %>% 
  summarise(miss = sum(is.na(df)))

# Data cleaning -----------------------------------------------------------

### col names

df1 <- df %>% 
  rename(
    c_r = ciudad_de_residencia,
    mun_r = municipio_de_residencia,
    fam_ven_nucleo = tiene_algun_familiar_en_venezuela__nucleo_mama_papa_o_hermanos,
    prin_gasto_fam = seleccione_el_principal_gasto_que_posee_su_familia,
    perc_fut_pais = como_percibe_el_futuro_en_el_pais,
    emigrar = desea_emigrar,
    perc_sit_act_pais = como_percibe_la_situacion_actual_del_pais,
    close_person_covid = alguna_persona_cercana_amigoa_o_allegado_ha_sufrido_de_covid19,
    n_close_person_covid_no_hosp = de_ser_afirmativa_su_respuesta_ingrese_la_cantidad_de_allegados_que_contrajeron_covid19_pero_no_fueron_hospitalizados,
    n_close_person_covid_yes_hosp = de_ser_afirmativa_su_respuesta_ingrese_la_cantidad_de_personas_cercanas_que_han_sido_hospitalizados_por_covid19,
    n_close_person_covid_deaths = de_ser_afirmativa_su_respuesta_ingrese_la_cantidad_de_personas_cercanas_que_han_muerto_de_covid19,
    fam_covid = algun_integrante_de_su_familia_ha_sufrido_de_covid19,
    n_fam_covid_no_hosp = de_ser_afirmativa_su_respuesta_ingrese_la_cantidad_de_familiares_que_contrajeron_covid19_pero_no_fueron_hospitalizados,
    n_fam_covid_yes_hosp = de_ser_afirmativa_su_respuesta_ingrese_la_cantidad_de_familiares_que_han_sido_hospitalizados_por_covid19,
    n_fam_covid_deaths = de_ser_afirmativa_su_respuesta_ingrese_la_cantidad_de_familiares_que_han_muerto_por_covid19,
    graffar_escala_total = graffar_escala_puntuacion_total,
    monthly_fam_income = cual_es_el_ingreso_aproximado_mensual_de_la_familia
  )

### col data type

## factor values conversion

df1[, 2:7] <- lapply(df1[, 2:7], function(x) str_replace(x, ",", "."))
  #correct formatting of numbers

df1[, 2:7] <- lapply(df1[, 2:7], as.numeric)
  #conversion to numeric

### categories 

## categories of city
unique(df1$c_r)
  # 31 different entries for city
  # City or area of research is "Caracas"

count_c_r <- df1 %>% 
  select(c_r) %>% 
  group_by(c_r) %>% 
  count()
  # 5 subjects don't belong to  Caracas, they'll be eliminated

df1 <- df1 %>% 
  filter(c_r != c("Puerto Ordaz")) %>% 
  filter(c_r != c("guarenas")) %>% 
  filter(c_r != c("Guarenas")) %>% 
  filter(c_r != c("Guatire")) %>% 
  filter(c_r != c("Ciudad Charallave"))
  # 5 records were excluded 

df1$c_r <- if_else(df1$c_r == "Caracas", df1$c_r, "Caracas")
  # all 'ciudad de residencia' records have been converted to "Caracas"

categories <- df1 %>% 
  select(where(is.character))

categories_list <- lapply(categories, unique)
  #View all the categories of character cols 


## Sexo
## this col has one record "helicóptero de combate" which is not a sex category
## I will eliminate it because it might be some kind of internet troll 

df1 <- df1 %>% 
  filter(sexo != "helicóptero de combate")

## The rest of the data categories seem correct



### NA

vis_miss(df1)



# Factor NA analysis ------------------------------------------------------

sum(is.na(df1$factor_ansiedad_rasgo))

# The Anxiety factors have 20 NA 


mean(df1$factor_ansiedad_rasgo, na.rm = TRUE)

describe(df1$factor_ansiedad_rasgo)

df1 %>% 
  select(2:7) %>% 
  filter(!(is.na(factor_ansiedad_rasgo))) %>% 
  summarise(mean_AE = mean(factor_ie_atencion_emocional),
            mean_CE = mean(factor_ie_comprension_emocional),
            mean_RE = mean(factor_ie_regulacion_emocional))

df1 %>% 
  select(2:4) %>% 
  summarise(m_AE = mean(factor_ie_atencion_emocional),
            m_CE = mean(factor_ie_comprension_emocional),
            m_RE = mean(factor_ie_regulacion_emocional))

# The mean values of ie factors change significantly with NA removed



# NA in the rest of the data ----------------------------------------------

### Convert NA to 0


df1[which(df1$close_person_covid == "No"), 20:22] <- 0

df1[which(df1$fam_covid == "No"), 24:26] <- 0



# Exploratory data analysis -----------------------------------------------

### Descriptive variables

## Sexo

df1 %>% 
  select(sexo) %>% 
  group_by(sexo) %>% 
  count()
  
  # Femenio: 173
  # Masculino: 168

## Edad

describe(df1$edad)

hist(df1$edad)

## tiene_algun_familiar_en_venezuela__nucleo_mama_papa_o_hermanos

df1 %>% 
  select(fam_ven_nucleo) %>% 
  group_by(fam_ven_nucleo) %>% 
  count()
  # No                16
  # Sí               325


## Summary of close person covid cases 

close_person_covid_summary <- df1 %>% 
  select(close_person_covid, n_close_person_covid_no_hosp, n_close_person_covid_yes_hosp, n_close_person_covid_deaths) %>% 
  group_by(close_person_covid) %>% 
  summarise(n = n(),
            perc = round(n()/nrow(df1) * 100, 2),
            no_hosp = sum(n_close_person_covid_no_hosp), 
            yes_hosp = sum(n_close_person_covid_yes_hosp), 
            deaths = sum(n_close_person_covid_deaths))



  # The result of 2615 no hosp people  seems unrealistic
  

close_no_hosp_review <- df1 %>% 
  select(n_close_person_covid_no_hosp) %>% 
  group_by(n_close_person_covid_no_hosp) %>% 
  count()

boxplot(df1$n_close_person_covid_no_hosp)

close_yes_hosp_review <- df1 %>% 
  select(n_close_person_covid_yes_hosp) %>% 
  group_by(n_close_person_covid_yes_hosp) %>% 
  count()

boxplot(df1$n_close_person_covid_yes_hosp)

close_deaths_review <- df1 %>% 
  select(n_close_person_covid_deaths) %>% 
  group_by(n_close_person_covid_deaths) %>% 
  count()

boxplot(df1$n_close_person_covid_deaths)

## Summary of family covid cases 

fam_covid_summary <- df1 %>% 
  select(fam_covid, n_fam_covid_no_hosp, n_fam_covid_yes_hosp, n_fam_covid_deaths) %>% 
  group_by(fam_covid) %>% 
  summarise(n = n(),
            perc = round(n() / nrow(df1) * 100, 2),
            no_hosp = sum(n_fam_covid_no_hosp), 
            yes_hosp = sum(n_fam_covid_yes_hosp), 
            deaths = sum(n_fam_covid_deaths))

fam_no_hosp_review <- df1 %>% 
  select(n_fam_covid_no_hosp) %>% 
  group_by(n_fam_covid_no_hosp) %>% 
  count()

boxplot(df1$n_fam_covid_no_hosp)

fam_yes_hosp_review <- df1 %>% 
  select(n_fam_covid_yes_hosp) %>% 
  group_by(n_fam_covid_yes_hosp) %>% 
  count()

boxplot(df1$n_fam_covid_yes_hosp)

fam_deaths_review <- df1 %>% 
  select(n_fam_covid_deaths) %>% 
  group_by(n_fam_covid_deaths) %>% 
  count()

boxplot(df1$n_fam_covid_deaths)

  #in all covid variables there are a considerable quantity of outliers


### summary of Graffar scale


## Stratification
df1 <- df1 %>%
  mutate(graffar_estratos = if_else(graffar_escala_total >= 4 & graffar_escala_total<= 6, "I", 
                             if_else(graffar_escala_total >= 7 & graffar_escala_total <= 9, "II", 
                                     if_else(graffar_escala_total >= 10 & graffar_escala_total <= 12, "III", 
                                             if_else(graffar_escala_total >= 13 & graffar_escala_total <= 16, "IV", "V")
                                                     )
                                             )
                                     ) 
                              
         )

## Summary

graffar_table <- df1 %>% 
  select(graffar_estratos) %>% 
  group_by(graffar_estratos) %>% 
  summarise(n = n(),
            perc = round(n() / nrow(df1) * 100, 2))

