# LIBRERIAS
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(missRanger)
library(lme4)
library(ggeffects)


# VARIABLES
# APV: SI/NO
# CONTRATO: SI/NO
# DEPRESION: SI/NO
# ESTADO CIVIL: CASADO/NO CASADO
# EXPECTATIVA DE TRABAJAR: ESPERA TRABAJAR/NO ESPERA TRABAJAR
# NEDU: BASICA/MEDIA/UNIVERSITARIA/POSGRADO
# OFICIO: EMPLEADO/EMPLEADOR/INDEPENDIENTE/OTRO
# SALUD: BUENA/REGULAR O MALA
# SES_CHILD: INDIGENTE, POBRE, BUENA, MUY BUENA
# SIT_LABORAL: TRABAJANDO/NO TRABAJANDO
# VIVIENDA: PROPIA/NO PROPIA
# EDAD
# FOLIO
# FEMALE: 1 MUJER/O HOMBRE
# DIFICULTAD: 0 A 7
# HOUSEHOLD: 1 JEFE DE HOGAR/2 Cónyuge/Pareja/3 Hijo(a)/4 Padre/Madre/5  Suegro(a)
# /6 Yerno/Nuera/7 Nieto(a)/8 Hermano(a)/9 Cuñado/10 Otro Familiar/11  No Familiar
# /12 S.Domestico puerta adentro

# 16 VARIABLES EN TOTAL


# 2002 DATABASE
base1_02 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2002/2002/base1.dta")
base2_02 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2002/2002/base2.dta")

base2_02 <- base2_02 %>% 
  group_by(folio_n20) %>%
  filter(orden == max(orden)) %>%
  ungroup()
eps02 <- base1_02 %>% 
  inner_join(base2_02, by = "folio_n20") %>% 
  select(household = ip2, folio_n20, expect_trab = iiip2,edad_retiro = iiip2eda, cotiza_volu = iiip6, ahorro_volu = iiip7, 
         sexo = ip4, edad = ip5, ecivil = ip6, sit_laboral = iip1a, oficio = iip11, contrato = iip12,salud = vip25, ses_child = vp21, vivienda = iip30, nedu = ip9t)%>% 
  mutate(vivienda = case_when(vivienda == 1 ~ 1,
                              between(vivienda, 2, 9)~0,
                              between(vivienda, 88, 99)~ NA),
         cotiza_volu = case_when(between(cotiza_volu, 1, 5)~ 1,
                                 cotiza_volu > 5 ~ 0),
         ahorro_volu = case_when(between(ahorro_volu, 1, 5)~ 1,
                                 ahorro_volu > 5 ~ 0),
         apv = ifelse(cotiza_volu == 1 | ahorro_volu == 1,1,0),
         pos_retiro = case_when(expect_trab == 1 ~ 0,
                                expect_trab == 2 | expect_trab == 7 ~ 1,
                                expect_trab == 3 & sexo == 2 & edad_retiro > 60 ~ 1,
                                expect_trab == 3 & sexo == 2 & edad_retiro <= 60 ~ 0,
                                expect_trab == 4 & sexo == 2 & edad_retiro > 60 ~ 1,
                                expect_trab == 4 & sexo == 2 & edad_retiro <= 60 ~ 0,
                                expect_trab == 3 & sexo == 1 & edad_retiro > 65 ~ 1,
                                expect_trab == 3 & sexo == 1 & edad_retiro <= 65 ~ 0,
                                expect_trab == 4 & sexo == 1 & edad_retiro > 65 ~ 1,
                                expect_trab == 4 & sexo == 1 & edad_retiro <= 65 ~ 0,
                                between(expect_trab, 5, 6) ~ 3),
         contrato = case_when(between(contrato, 1, 3)~ 1,
                              TRUE~ 0),
         salud = ifelse(salud < 3,1,0),
         female = ifelse(sexo == 2,1,0),
         nedu = case_when(between(nedu, 2, 4) ~ 1,
                          between(nedu, 5, 8) ~ 2,
                          between(nedu, 9, 14)~ 3,
                          nedu == 15 ~ 4,
                          nedu == 16 ~ 0,
                          nedu == 17 ~ NA),
         oficio = case_when(oficio == 1 ~ 1,
                               between(oficio, 3, 4)~ 2,
                            oficio == 2 ~ 3,
                               TRUE ~ 4),
         ecivil = ifelse(ecivil == 1, 1, 0)) %>% 
  
  filter(edad >= 40) %>% 
  filter((edad < 60 & female == 1) | (edad < 65 & female == 0)) %>% 
  select(-expect_trab, -edad_retiro, - cotiza_volu, -ahorro_volu,-sexo)


##########
# LABELS #
##########

lab <- function(df){
df <- df %>% 
dplyr::mutate(salud = factor(salud, levels = c(1,0), labels = c("Buena", "Mala o regular")),
       nedu = factor(nedu, levels = c(0:4), labels = c("Ninguna","Básica","Media","Universitaria","Posgrado")),
       ecivil = factor(ecivil, levels  = c(1,0), labels = c("Casado", "No casado")),
       vivienda = factor(vivienda, levels = c(1,0), labels = c("Propia","No propia")),
       apv = factor(apv, levels = c(1,0), labels = c("Si","No")),
       sit_laboral = factor(sit_laboral, levels = c(1,2), labels = c("Trabajando", "No trabajando")),
       pos_retiro = factor(pos_retiro, levels = c(1,0), labels = c("Espera trabajar","No espera trabajar")),
       oficio = factor(oficio, levels = c(1:4), labels = c("Empleador","Empleado","Independiente","Otro")),
       contrato = factor(contrato, levels = c(1,0), labels = c("Si", "No")),
       ses_child = factor(ses_child, levels = c(1:4), labels = c("Indigente","Pobre","Buena","Muy buena")),
       female = factor(female, levels = c(1,0), labels = c("Female","Male")))
}



eps02 <- lab(eps02)



write_dta(eps02, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2002/2002/eps02_filtrada.dta")

# NOTA: DIFICULTADES PARA HACER COSAS NO EXISTE EN 2002, DEPRESION TAMPOCO EXISTE EN 2002

# 2004 DATABASE
# NOTA: DIFICULTADES PARA HACER COSAS NO EXISTE EN 2002, COMENZÓ EN 2004. ESO GENERA MUCHOS NA.
# NOTA: DEPRESION TAMPOCO EXISTE EN 2002

base1_04 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2004/2004/entrevistado.dta")
base2_04 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2004/2004/h_laboral.dta")
base1_04 <- base1_04 %>% 
  select(household = a4b, sexo = a5, edad = a6, ecivil = a7, salud = a8, nedu = a10n, vivienda = d7, apv = d23_03, f16_01:f16_08, depresion = f33_02,
         ses_child = h12, expect_trab = e23,edad_retiro = e24, folio_n20)

base2_04 <- base2_04 %>% 
  group_by(folio_n20) %>%
  filter(orden == max(orden)) %>%
  ungroup() %>% 
  select(oficio = b7, sit_laboral = b2, contrato = b8, folio_n20)

eps04 <- base1_04 %>% 
  inner_join(base2_04, by = "folio_n20") %>% 
  mutate(vivienda = case_when(vivienda == 1 ~ 1,
                              between(vivienda, 2, 9)~0,
                              between(vivienda, 88, 99)~ NA),
         apv = ifelse(apv == 1,1,0),
         pos_retiro = case_when(expect_trab == 1 ~ 0,
                                expect_trab == 2 | expect_trab == 5 ~ 1,
                                expect_trab == 3 & sexo == 2 & edad_retiro > 60 ~ 1,
                                expect_trab == 3 & sexo == 2 & edad_retiro <= 60 ~ 0,
                                expect_trab == 4 & sexo == 2 & edad_retiro > 60 ~ 1,
                                expect_trab == 4 & sexo == 2 & edad_retiro <= 60 ~ 0,
                                expect_trab == 3 & sexo == 1 & edad_retiro > 65 ~ 1,
                                expect_trab == 3 & sexo == 1 & edad_retiro <= 65 ~ 0,
                                expect_trab == 4 & sexo == 1 & edad_retiro > 65 ~ 1,
                                expect_trab == 4 & sexo == 1 & edad_retiro <= 65 ~ 0,
                                between(expect_trab, 6, 7) ~ 3),
         contrato = case_when(contrato == 2~ 1,
                              contrato == -4 ~ NA,
                              TRUE~ 0),
         salud = na_if(salud, -4),
         salud = ifelse(salud <= 3,1,0),
         female = ifelse(sexo == 2,1,0),
         nedu = case_when(between(nedu, 2, 4) ~ 1,
                          between(nedu, 5, 8) ~ 2,
                          between(nedu, 9, 14)~ 3,
                          nedu == 15 ~ 4,
                          nedu == 16 ~ 0,
                          nedu == 17 ~ NA),
         oficio = case_when(oficio == 1 ~ 1,
                            between(oficio, 3, 4)~ 2,
                            oficio == 2 ~ 3,
                            TRUE ~ 4),
         ecivil = ifelse(ecivil == 1, 1, 0),
         ses_child = na_if(ses_child, -4),
         depresion = na_if(depresion, -4),
         depresion = ifelse(depresion == 2, 0,1)) %>% 
  filter(edad >= 40) %>% 
  filter((edad < 60 & female == 1) | (edad < 65 & female == 0)) %>% 
  mutate_at(vars(f16_01:f16_07), ~ifelse(. == 1, 1, 0)) %>% 
  rowwise() %>%
  mutate(f16_sum = sum(c(f16_01, f16_02, f16_03, f16_04, f16_05, f16_06, f16_07), na.rm = TRUE),
         dad = ifelse(f16_sum > 0 | f16_08 == 2, 1, 0)) %>%
  ungroup() %>% 
  select(-f16_01,-f16_02, -f16_03, -f16_04, -f16_05, -f16_06, -f16_07, -f16_08,-expect_trab, -edad_retiro,-sexo, -f16_sum) 

lab2 <- function(df){
  df <- df %>% 
    dplyr::mutate(salud = factor(salud, levels = c(1,0), labels = c("Buena", "Mala o regular")),
                  nedu = factor(nedu, levels = c(0:4), labels = c("Ninguna","Básica","Media","Universitaria","Posgrado")),
                  ecivil = factor(ecivil, levels  = c(1,0), labels = c("Casado", "No casado")),
                  vivienda = factor(vivienda, levels = c(1,0), labels = c("Propia","No propia")),
                  apv = factor(apv, levels = c(1,0), labels = c("Si","No")),
                  sit_laboral = factor(sit_laboral, levels = c(1,2), labels = c("Trabajando", "No trabajando")),
                  pos_retiro = factor(pos_retiro, levels = c(1,0), labels = c("Espera trabajar","No espera trabajar")),
                  oficio = factor(oficio, levels = c(1:4), labels = c("Empleador","Empleado","Independiente","Otro")),
                  contrato = factor(contrato, levels = c(1,0), labels = c("Si", "No")),
                  ses_child = factor(ses_child, levels = c(1:4), labels = c("Indigente","Pobre","Buena","Muy buena")),
                  female = factor(female, levels = c(1,0), labels = c("Female","Male")),
                  depresion = factor(depresion, levels = c(1,0), labels = c("Si", "No")),
                  dad = factor(dad, levels = c(1,0), labels = c("Dificultad", "No dificultad")))
  }

table(eps04$salud)
eps04 <- lab2(eps04)

write_dta(eps04, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2004/2004/eps04_filtrada.dta")


# 2006 

base1_06 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/entrevistado.dta")
base2_06 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/historialaboral.dta")
base3_06 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/salud.dta")

base1_06 <- base1_06 %>% 
  select(household = a5, factor = factor_EPS06, sexo = a8, edad = a9, salud = a10, 
         nedu = a12n, expect_trab = e27, ecivil = i1, apv = e21, vivienda = d7, folio_n20)%>% 
  mutate(vivienda = case_when(vivienda == 1 ~ 1,
                              between(vivienda, 2, 9)~0,
                              between(vivienda, 88, 99)~ NA))

base2_06 <- base2_06 %>% 
  group_by(folio_n20) %>%
  filter(orden == max(orden)) %>%
  ungroup() %>% 
  select(ocupacion = b8, sit_laboral = b2, contrato = b9, folio_n20)

base3_06 <- base3_06 %>% 
  select(depresion = f40_2, f18_1:f18_7, folio_n20) %>% 
  mutate_at(vars(f18_1:f18_7), ~ifelse(. == 1, 1, 0)) %>% 
  rowwise() %>%
  mutate(f18_sum = sum(c(f18_1, f18_2, f18_3, f18_4, f18_5, f18_6, f18_7), na.rm = TRUE)) %>%
  ungroup() %>% 
  select(depresion, dad = f18_sum, folio_n20)

eps06 <- base1_06 %>% 
  inner_join(base2_06, by = "folio_n20") %>% 
  inner_join(base3_06, by = "folio_n20")
write_dta(eps06, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/eps06.dta")


## EPS 2009 
base1_09 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/entrevistado.dta")
base2_09 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/hindividual.dta")
base3_09 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/salud.dta")
base4_09 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/hlaboral.dta")
  
#NOTA: SES CHILD NO ESTÁ EN 2009-IMPUTAR PORQUE ES TIME INVARIANT

base1_09 <- base1_09 %>% 
  select(household = a5, sexo = a8, edad = a9, salud = a10, 
         nedu = a12n, expect_trab = e49, apv = e40, 
         vivienda = d7, folio_n20)%>% 
  mutate(vivienda = case_when(vivienda == 1 ~ 1,
                              between(vivienda, 2, 9)~0,
                              between(vivienda, 88, 99)~ NA))
base2_09 <- base2_09 %>% 
  select(ecivil = i1, folio_n20)

base3_09 <- base3_09 %>% 
  select(depresion = f38_02, f16_01:f16_07, folio_n20) %>% 
mutate_at(vars(f16_01:f16_07), ~ifelse(. == 1, 1, 0)) %>% 
  rowwise() %>%
  mutate(f16_sum = sum(c(f16_01, f16_02, f16_03, f16_04, f16_05, f16_06, f16_07), na.rm = TRUE)) %>%
  ungroup() %>% 
  select(depresion, dad = f16_sum, folio_n20)

base4_09 <- base4_09 %>% 
  group_by(folio_n20) %>%
  filter(orden == max(orden)) %>%
  ungroup() %>% 
  select(ocupacion = b8, sit_laboral = b2, contrato = b9, folio_n20)

eps09 <- base1_09 %>% 
  inner_join(base2_09, by = "folio_n20") %>% 
  inner_join(base3_09, by = "folio_n20") %>% 
  inner_join(base4_09, by = "folio_n20")
write_dta(eps09, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/eps09.dta")


# EPS 2015
base1_15 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/MODULOA_entrevistado.dta")
base2_15 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/MODULOB_historia_laboral.dta")
base3_15 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/MODULOD_entrevistado.dta")
base4_15 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/MODULOE_entrevistado.dta")
base5_15 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/MODULOF_entrevistado.dta")
base6_15 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/MODULOI_entrevistado.dta")
base7_15 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/MODULOH_entrevistado.dta")

base1_15 <- base1_15 %>% 
  select(sexo = a8, edad = edad_corregida, household = a4, salud = a10, nedu = a12n, folio_n20)

base2_15 <- base2_15 %>% 
  group_by(folio_n20) %>%
  filter(orden == max(orden)) %>%
  ungroup() %>% 
  select(ocupacion = b8, sit_laboral = b2, contrato = b9a, folio_n20)

base3_15 <- base3_15 %>% 
  select(vivienda = d7, folio_n20)%>% 
  mutate(vivienda = case_when(vivienda == 1 ~ 1,
                              between(vivienda, 2, 9)~0,
                              between(vivienda, 88, 99)~ NA))

base4_15 <- base4_15 %>% 
  select(apv = e34, expect_trab = e42,folio_n20)

base5_15 <- base5_15 %>% 
  select(depresion = f38_2, f16_1:f16_7, folio_n20) %>% 
  mutate_at(vars(f16_1:f16_7), ~ifelse(. == 1, 1, 0)) %>% 
  rowwise() %>%
  mutate(f16_sum = sum(c(f16_1, f16_2, f16_3, f16_4, f16_5, f16_6, f16_7), na.rm = TRUE)) %>%
  ungroup() %>% 
  select(depresion, dad = f16_sum, folio_n20)

base6_15 <- base6_15 %>% 
  select(ecivil = i1, folio_n20)

base7_15 <- base7_15 %>% 
  select(ses_child = h12, folio_n20)


eps15 <- base1_15 %>% 
  inner_join(base2_15, by = "folio_n20") %>% 
  inner_join(base3_15, by = "folio_n20") %>% 
  inner_join(base4_15, by = "folio_n20") %>% 
  inner_join(base5_15, by = "folio_n20") %>% 
  inner_join(base6_15, by = "folio_n20") %>% 
  left_join(base7_15, by = "folio_n20") %>% 
  arrange(folio_n20)

write_dta(eps15, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/eps15.dta")

# EPS 2020
base1_20 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/MODULO_A_entrevistado_in.dta")
base2_20 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/MODULO_B_Historia_Laboral_in.dta")
base3_20 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/MODULO_D_Entrevistado_in.dta")
base4_20 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/MODULO_E_Entrevistado_in.dta")
base5_20 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/MODULO_F_Entrevistado_in.dta")
base6_20 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/MODULO_I_Entrevistado_in.dta")

base1_20 <- base1_20 %>% 
  select(sexo = a8, edad = a9, household = a4, salud = a10, nedu = a12_n, folio_n20)

base2_20 <- base2_20 %>% 
  group_by(folio_n20) %>%
  filter(orden == max(orden)) %>%
  ungroup() %>% 
  select(ocupacion = b8, sit_laboral = b2a_SO, contrato = b9_a, folio_n20)

base3_20 <- base3_20 %>% 
  select(vivienda = d7, folio_n20) %>% 
  mutate(vivienda = case_when(vivienda == 1 ~ 1,
                              between(vivienda, 2, 9)~0,
                              between(vivienda, 88, 99)~ NA))

base4_20 <- base4_20 %>% 
  select(apv = e34, expect_trab = e42,folio_n20)

base5_20 <- base5_20 %>% 
  select(depresion = f38_2, folio_n20)

base6_20 <- base6_20 %>% 
  select(ecivil = i1, folio_n20)

eps20 <- base1_20 %>% 
  inner_join(base2_20, by = "folio_n20") %>% 
  inner_join(base3_20, by = "folio_n20") %>% 
  inner_join(base4_20, by = "folio_n20") %>% 
  inner_join(base5_20, by = "folio_n20") %>% 
  inner_join(base6_20, by = "folio_n20") 

write_dta(eps20, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/eps20.dta")

# UNION DE DATOS
eps02 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2002/2002/eps02.dta")
eps04 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2004/2004/eps04.dta")
eps06 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/eps06.dta")
eps09 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/eps09.dta")
eps15 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/eps15.dta")
eps20 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/eps20.dta")


eps_unido <- bind_rows("2002" = eps02, "2004" = eps04, "2006" = eps06, "2009" = eps09, "2015" = eps15, "2020" = eps20, .id = "año") %>% 
  arrange(folio_n20)


write_dta(eps_unido, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/eps0220.dta")



######################
## DATA MANIPULATION #
######################
data <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/eps0220.dta")

data_filtrada <- data %>% 
  filter(edad >= 40) %>% 
  filter((sexo == 1 & edad < 60) | (sexo == 0 & edad < 65)) %>% 
  arrange(folio_n20)

write_dta(data_filtrada, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/eps0220_filtrada.dta")

#SES CHILD NO VARÍA EN EL TIEMPO Y HAY MUCHOS VALORES PERDIDOS
# SE PUEDE IMPUTAR FACILMENTE SI HAY ALGUN REGISTRO DURANTE EL SEGUIMIENTO

na_count <- sapply(data_filtrada, function(y) sum(length(which(is.na(y)))))
print(na_count)

data_filtrada <- data_filtrada %>%
  group_by(folio_n20) %>%
  fill(ses_child, .direction = "downup")




single_folio_data <- data_filtrada %>%
  group_by(folio_n20) %>%
  summarise(count = n()) %>%
  # Filtra para quedarse solo con los folios que aparecen una única vez
  filter(is.na(single_folio_data))

data_filtrada <- data_filtrada %>% 
  left_join(single_folio_data, by = "folio_n20") %>% 
  filter(single_folio_data != 1)
# hay muchos valores perdidos en este grupo de personas, creo que deberia eliminarse.



# EN PROMEDIO HAY 3.22 WAVES
data_filtrada %>%
  group_by(folio_n20) %>%
  summarise(count = n()) %>%
  summarise(avg_count = mean(count))

# RELLENAR SES_CHILD SI TIENEN OBSERVACIONES EN ALGUNA WAVE
mi_data_filled <- mi_data %>%
  group_by(folio_n20) %>%
  fill(ses_child, .direction = "downup")

############
# ANALYSIS #
############



# SUBSET
data <- data %>% 
  filter((sexo == 1 & edad < 60) | (sexo == 0 & edad < 65))

# TRANSFORMACION
data_raw <- data_raw %>% 
  mutate(salud = na_if(salud, 9),
         salud = ifelse(salud >= 4, 1,0),
         salud = factor(salud, levels = c(1,0), labels = c("Buena", "Mala o regular")),
         nedu = factor(nedu, levels = c(0:4), labels = c("Ninguna","Básica","Media","Universitaria","Posgrado")),
         act_fisica = ifelse(act_fisica > 1, 1, 0),
         act_fisica = factor(act_fisica, levels = c(1,0), labels = c("Hace deporte","No hace deporte")),
         depresion = factor(depresion, levels = c(1,0), labels = c("Depresión", "No tiene depresión")),
         ecivil = factor(ecivil, levels  = c( 1,0), labels = c("Casado", "No casado")),
         vivienda = factor(vivienda, levels = c(1,2,3), labels = c("Propia pagada","Propia pagando","No propia")),
         apv = factor(apv, levels = c(1,0), labels = c("Si","No")),
         sit_laboral = ifelse(sit_laboral == 1,1,0),
         sit_laboral = factor(sit_laboral, levels =c(1,0), labels = c("Trabajando", "No trabajando")),
         expect_trab = factor(expect_trab, levels =c(1,0), labels = c("Espera trabajar","No espera trabajar")),
         oficio = factor(oficio, levels = c(1:4), labels = c("Empleador","Independiente","Empleado","Otro")),
         contrato = factor(contrato, levels = c(1,0), labels = c("Si", "No")),
         ses_nino = factor(ses_nino, levels = c(1:4), labels = c("Indigente","Pobre","Buena","Muy buena")))

# IMPUTATION
set.seed(2125)
data_imp <- missRanger(
  data_raw, 
  formula = . ~ .  - folio_n,
  num.trees = 200, 
  returnOOB=T,
  maxiter=20,
  verbose = 2, 
  seed = 2125)

data_imp <- data_imp %>% 
  arrange(folio_n)

# MODELOS

mixlogit <- glmer(expect_trab ~ ses_nino + vivienda + apv+ contrato + sit_laboral + oficio + salud + depresion +dad+ecivil*female + edad + (1|folio_n),
                       data = data_imp,
                       family = binomial(link = "logit"))

mixlogit_edad <- glmer(expect_trab ~ ses_nino + vivienda + apv+ contrato + sit_laboral + oficio + salud + depresion +dad+ecivil + edad*female + (1|folio_n),
                              data = data_imp,
                              family = binomial(link = "logit"))

summary(mixlogit)

summary(mixlogit_edad)
# Calcula los efectos marginales
efectos_marginales <- ggpredict(mixlogit, terms = "edad [all]")

efectos_marginales2 <- ggpredict(mixlogit, terms = c("edad [all]", "female [all]"))

# GRÁFICOS
ggplot(efectos_marginales, aes(x = x, y = predicted)) +
  geom_smooth(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_minimal() +
  labs(x = "Age",
       y = "Probability of expecting to work after retirement age") +
  scale_y_continuous(limits = c(0.2, 0.6), breaks = seq(0.2, 0.6, by = 0.1))

ggplot(efectos_marginales2, aes(x = x, y = predicted)) +
  geom_smooth(aes(color = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  facet_wrap(~group, ncol = 2, scales = "free_y") +
  theme_minimal() +
  labs(x = "Age",
       y = "Probability of expecting to work after retirement age") +
  scale_color_grey(start = 0, end = .7) + 
  scale_fill_grey(start = 0, end = .7)   

## ANALISIS SECUNDARIO
mixlogit_time_random <- glmer(expect_trab ~ ses_nino + vivienda + apv + contrato + sit_laboral + oficio + salud + depresion + dad + ecivil*female + edad + (1 + time|folio_n),
                              data = data_imp,
                              family = binomial(link = "logit"))
mixlogit_time_age_interaction <- glmer(expect_trab ~ ses_nino + vivienda + apv + contrato + sit_laboral + oficio + salud + depresion + dad + ecivil + edad*female*Time + (1|folio_n),
                                       data = data_imp,
                                       family = binomial(link = "logit"))
# Cargar las bibliotecas necesarias
library(ggplot2)

# Obtener las probabilidades predichas
pred_probs <- predict(mixlogit_time_age_interaction, newdata = data_imp, type = "response")

# Agregar las probabilidades predichas al conjunto de datos
data_imp$pred_probs <- pred_probs

# Crear el gráfico
ggplot(data_imp, aes(x = edad, y = pred_probs)) +
  geom_point() + 
  facet_grid(Time ~ female) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Probabilidad predicha de expect_trab") +
  xlab("Edad") +
  ggtitle("Efecto de la edad en expect_trab a lo largo del tiempo y por género")
# AIC y BIC del modelo
AIC(mixlogit_time_age_interaction)
BIC(mixlogit_time_age_interaction)
# Calcular los residuos
residuos <- residuals(mixlogit_time_age_interaction, type = "pearson")

# Gráfico de residuos
ggplot(data_imp, aes(x = pred_probs, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Probabilidades predichas") +
  ylab("Residuos") +
  ggtitle("Gráfico de residuos")
# Instala e importa DHARMa
install.packages("DHARMa")
library(DHARMa)

# Crear los residuos simulados
simulationOutput <- simulateResiduals(fittedModel = mixlogit_time_age_interaction)

# Realizar pruebas de bondad de ajuste
plot(simulationOutput)
