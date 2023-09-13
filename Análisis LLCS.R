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
         sit_laboral = ifelse(sit_laboral == 1, 1, 0),
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
  select(- cotiza_volu, -ahorro_volu,-sexo)



eps02_main <- eps02%>% 
  filter((edad < 60 & female == 1) | (edad < 65 & female == 0))

eps02_sens <- eps02 %>% 
  filter(edad < 65)



write_dta(eps02, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2002/2002/eps02_40.dta")
write_dta(eps02_main, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2002/2002/eps02_main.dta")
write_dta(eps02_sens, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2002/2002/eps02_sens.dta")


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
  mutate(vivienda = ifelse(vivienda == 1,1,0),
         sit_laboral = ifelse(sit_laboral == 1,1,0),
         apv = na_if(apv, -4),
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
         contrato = case_when(contrato == 1~ 1,
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
  mutate_at(vars(f16_01:f16_07), ~ifelse(. == 1, 1, 0)) %>% 
  rowwise() %>%
  mutate(f16_sum = sum(c(f16_01, f16_02, f16_03, f16_04, f16_05, f16_06, f16_07), na.rm = TRUE),
         dad = ifelse(f16_sum > 0 | f16_08 == 2, 1, 0)) %>%
  ungroup() %>% 
  select(-f16_01,-f16_02, -f16_03, -f16_04, -f16_05, -f16_06, -f16_07, -f16_08,-sexo, -f16_sum)

str(eps04)

eps04_main <-eps04 %>% 
  filter((edad < 60 & female == 1) | (edad < 65 & female == 0))
 
eps04_sens <- eps04 %>% 
  filter(edad < 65)


# Write database in STATA
write_dta(eps04, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2004/2004/eps04_40.dta")
write_dta(eps04_main, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2004/2004/eps04_main.dta")
write_dta(eps04_sens, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2004/2004/eps04_sens.dta")




# 2006 
#NOTA: SES CHILD NO ESTÁ EN 2006-IMPUTAR PORQUE ES TIME INVARIANT

base1_06 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/entrevistado.dta")
base2_06 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/historialaboral.dta")
base3_06 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/salud.dta")

base1_06 <- base1_06 %>% 
  select(household = a5, sexo = a8, edad = a9, salud = a10, 
         nedu = a12n, expect_trab = e27, edad_retiro = e28, 
         ecivil = i1, apv = e21, vivienda = d7, folio_n20)


base2_06 <- base2_06 %>% 
  group_by(folio_n20) %>%
  filter(orden == max(orden)) %>%
  ungroup() %>% 
  select(oficio = b8, sit_laboral = b2, contrato = b9, folio_n20)

base3_06 <- base3_06 %>% 
  select(depresion = f40_2, f18_1:f18_7, folio_n20) %>% 
  mutate_at(vars(f18_1:f18_7), ~ifelse(. == 1, 1, 0)) %>% 
  rowwise() %>%
  mutate(f18_sum = sum(c(f18_1, f18_2, f18_3, f18_4, f18_5, f18_6, f18_7), na.rm = TRUE),
         dad = ifelse(f18_sum > 0, 1, 0)) %>%
  ungroup() %>% 
  select(depresion, dad, folio_n20)

eps06 <- base1_06 %>% 
  inner_join(base2_06, by = "folio_n20") %>% 
  inner_join(base3_06, by = "folio_n20") %>% 
  mutate(vivienda = ifelse(vivienda == 1, 1, 0),
         sit_laboral = ifelse(sit_laboral == 1, 1, 0),
    apv = ifelse(apv == 1,1,0),
    edad_retiro = ifelse(edad_retiro == -4, 999, edad_retiro),
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
  contrato = case_when(contrato == 1~ 1,
                     contrato == 9 ~ NA,
                     TRUE~ 0),
  salud = na_if(salud, 9),
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
  depresion = na_if(depresion, 9),
  depresion = ifelse(depresion == 2, 0,1)) %>% 
  filter(edad >= 40) %>% 
  select(-sexo)
  

eps06_main <- eps06 %>% 
  filter((edad < 60 & female == 1) | (edad < 65 & female == 0)) 

eps06_sens <- eps06 %>% 
  filter(edad < 65)

# Write database in STATA
write_dta(eps06, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/eps06_40.dta")
write_dta(eps06_main, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/eps06_main.dta")
write_dta(eps06_sens, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2006/2006/eps06_sens.dta")


## EPS 2009 
base1_09 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/entrevistado.dta")
base2_09 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/hindividual.dta")
base3_09 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/salud.dta")
base4_09 <- read_dta("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/hlaboral.dta")
  
#NOTA: SES CHILD NO ESTÁ EN 2009-IMPUTAR PORQUE ES TIME INVARIANT

base1_09 <- base1_09 %>% 
  select(household = a5, sexo = a8, edad = a9, salud = a10, 
         nedu = a12n, expect_trab = e49,edad_retiro = e50, apv = e40, 
         vivienda = d7, folio_n20)
table(is.na(base1_09$edad_retiro))

base2_09 <- base2_09 %>% 
  select(ecivil = i1, folio_n20)

base3_09 <- base3_09 %>% 
  select(depresion = f38_02, f16_01:f16_07, folio_n20) %>% 
mutate_at(vars(f16_01:f16_07), ~ifelse(. == 1, 1, 0)) %>% 
  rowwise() %>%
  mutate(f16_sum = sum(c(f16_01, f16_02, f16_03, f16_04, f16_05, f16_06, f16_07), na.rm = TRUE),
         dad = ifelse(f16_sum>0,1,0)) %>%
  ungroup() %>% 
  select(depresion, dad, folio_n20)

base4_09 <- base4_09 %>% 
  group_by(folio_n20) %>%
  filter(orden == max(orden)) %>%
  ungroup() %>% 
  select(oficio = b8, sit_laboral = b2, contrato = b9, folio_n20)

eps09 <- base1_09 %>% 
  inner_join(base2_09, by = "folio_n20") %>% 
  inner_join(base3_09, by = "folio_n20") %>% 
  inner_join(base4_09, by = "folio_n20") %>% 
  mutate(apv = ifelse(apv == 1,1,0),
         vivienda = ifelse(vivienda == 1, 1, 0),
         sit_laboral = ifelse(sit_laboral == 1, 1, 0),
         edad_retiro = ifelse(edad_retiro == -4, 999, edad_retiro),
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
         contrato = case_when(contrato == 1~ 1,
                              between(contrato, 8, 9) ~ NA,
                              TRUE~ 0),
         salud = na_if(salud, 9),
         salud = ifelse(salud <= 3,1,0),
         female = ifelse(sexo == 2,1,0),
         nedu = case_when(between(nedu, 3, 5) ~ 1,
                          between(nedu, 6, 8) ~ 2,
                          between(nedu, 9, 12)~ 3,
                          nedu == 13 ~ 4,
                          nedu == 1 ~ 0,
                          TRUE ~ NA),
         oficio = case_when(oficio == 1 ~ 1,
                            between(oficio, 3, 4)~ 2,
                            oficio == 2 ~ 3,
                            TRUE ~ 4),
         ecivil = ifelse(ecivil == 1, 1, 0),
         depresion = case_when(depresion == 1 ~ 1,
                               depresion == 2 ~ 0,
                               TRUE ~ NA)) %>% 
  filter(edad >= 40) %>% 
  select(-sexo)
  

eps09_main <- eps09 %>% 
  filter((edad < 60 & female == 1) | (edad < 65 & female == 0)) 
  
eps09_sens <- eps09 %>% 
  filter(edad < 65)

# Write database in STATA
write_dta(eps09, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/eps09_40.dta")
write_dta(eps09_main, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/eps09_main.dta")
write_dta(eps09_sens, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2009/2009/eps09_sens.dta")


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
  select(oficio = b8, sit_laboral = b2, contrato = b9a, folio_n20)

base3_15 <- base3_15 %>% 
  select(vivienda = d7, folio_n20)


base4_15 <- base4_15 %>% 
  select(apv = e34, expect_trab = e42,edad_retiro = e43, folio_n20)

base5_15 <- base5_15 %>% 
  select(depresion = f38_2, f16_1:f16_7, folio_n20) %>% 
  mutate_at(vars(f16_1:f16_7), ~ifelse(. == 1, 1, 0)) %>% 
  rowwise() %>%
  mutate(f16_sum = sum(c(f16_1, f16_2, f16_3, f16_4, f16_5, f16_6, f16_7), na.rm = TRUE),
         dad = ifelse(f16_sum > 0,1,0)) %>%
  ungroup() %>% 
  select(depresion, dad, folio_n20)

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
  full_join(base7_15, by = "folio_n20") %>% 
  arrange(folio_n20) %>% 
  mutate(apv = ifelse(apv == 1,1,0),
         vivienda = ifelse(vivienda == 1, 1,0),
         ses_child = na_if(ses_child, 8),
         ses_child = na_if(ses_child, 9),
         expect_trab = case_when(between(expect_trab, 8, 99)~NA,
                                 TRUE ~ expect_trab),
         sit_laboral = ifelse(sit_laboral == 1, 1, 0),
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
         contrato = case_when(contrato == 1~ 1,
                              contrato < 4 ~ 0,
                              TRUE~ NA),
         salud = case_when(salud <= 3~ 1,
                           between(salud,3,6)~0,
                           TRUE ~ NA),
         female = ifelse(sexo == 2,1,0),
         nedu = case_when(between(nedu, 1, 5) ~ 1,
                          between(nedu, 6, 8) ~ 2,
                          between(nedu, 9, 11)~ 3,
                          nedu == 12 ~ 4,
                          nedu == 13 ~ 0,
                          TRUE~ NA),
         oficio = case_when(oficio == 1 ~ 1,
                            between(oficio, 3, 4)~ 2,
                            oficio == 2 ~ 3,
                            oficio == 88 ~ NA,
                            TRUE ~ 4),
         ecivil = ifelse(ecivil == 1, 1, 0),
         depresion = case_when(depresion == 1 ~ 1,
                   depresion == 2 ~ 0,
                   TRUE ~ NA)) %>% 
  filter(edad >= 40) %>% 
  select(-sexo)


str(eps15)
table(is.na(base4_15$edad_retiro))

eps15_main <- eps15 %>% 
  filter((edad < 60 & female == 1) | (edad < 65 & female == 0))

eps15_sens <- eps15 %>% 
  filter(edad < 65)

# Write database in STATA
write_dta(eps15, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/eps15_40.dta")
write_dta(eps15_main, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/eps15_main.dta")
write_dta(eps15_sens, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2015/2015/eps15_sens.dta")


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
  select(oficio = b8, sit_laboral = b2a_SO, contrato = b9_a, folio_n20)

base3_20 <- base3_20 %>% 
  select(vivienda = d7, folio_n20) 

base4_20 <- base4_20 %>% 
  select(apv = e34, expect_trab = e42, edad_retiro = e43,folio_n20)

base5_20 <- base5_20 %>% 
  select(depresion = f38_2, folio_n20)

base6_20 <- base6_20 %>% 
  select(ecivil = i1, folio_n20)

eps20 <- base1_20 %>% 
  inner_join(base2_20, by = "folio_n20") %>% 
  inner_join(base3_20, by = "folio_n20") %>% 
  inner_join(base4_20, by = "folio_n20") %>% 
  inner_join(base5_20, by = "folio_n20") %>% 
  inner_join(base6_20, by = "folio_n20") %>% 
  arrange(folio_n20) %>% 
  mutate(apv = ifelse(apv == 1,1,0),
         vivienda = case_when(vivienda == 1 ~ 1,
                              between(vivienda, 2, 9)~0,
                              TRUE~ NA),
         expect_trab = case_when(between(expect_trab, 8, 99)~NA,
                                 TRUE ~ expect_trab),
         edad_retiro = ifelse(edad_retiro == 8, 888, edad_retiro),
         edad_retiro = ifelse(edad_retiro == 9, 999, edad_retiro),
         sit_laboral = ifelse(sit_laboral == 1, 1, 0),
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
         contrato = case_when(contrato == 1~ 1,
                              contrato > 7 ~ NA,
                              TRUE~ 0),
         salud = na_if(salud, 9),
         salud = ifelse(salud <= 3,1,0),
         female = ifelse(sexo == 2,1,0),
         nedu = case_when(between(nedu, 1, 5) ~ 1,
                          between(nedu, 6, 8) ~ 2,
                          between(nedu, 9, 11)~ 3,
                          nedu == 12 ~ 4,
                          nedu == 13 ~ 0,
                          TRUE ~ NA),
         oficio = case_when(oficio == 1 ~ 1,
                            between(oficio, 3, 4)~ 2,
                            oficio == 2 ~ 3,
                            TRUE ~ 4),
         ecivil = ifelse(ecivil == 1, 1, 0),
         depresion = case_when(depresion == 1 ~ 1,
                               depresion == 2 ~ 0,
                               TRUE ~ NA)) %>% 
  filter(edad >= 40) %>% 
  select(-sexo)

eps20_main <- eps20 %>% 
  filter((edad < 60 & female == 1) | (edad < 65 & female == 0))

eps20_sens <- eps20 %>% 
  filter(edad < 65)


write_dta(eps20, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/eps20_40.dta")
write_dta(eps20_main, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/eps20_main.dta")
write_dta(eps20_sens, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/2020/2020/eps20_sens.dta")

# LIMPIEZA DEL ENVIRONMENT
objetos_a_mantener <- grep("(_main|_sens)$", ls(), value = TRUE)

# Encontrar los objetos que no están en la lista de 'objetos_a_mantener'
objetos_a_eliminar <- setdiff(ls(), objetos_a_mantener)

# Eliminar los objetos que no queremos mantener
rm(list = objetos_a_eliminar)


# ANALISIS DESCRIPTIVO

# EPS02
lapply(eps02_main[,c(-1,-2)], table)

# EPS04
lapply(eps04_main[,c(-1,-12)], table)


# EPS06
lapply(eps06_main[,c(-1,-11)], table)

# EPS09
lapply(eps09_main[,c(-1,-10)], table)


# EPS15
lapply(eps15_main[,c(-3,-6)], table)


# EPS20
lapply(eps20_main[,c(-2,-5)], table)

######################
## DATA MANIPULATION #
######################

# UNION DE DATOS MAIN

eps_main <- bind_rows("2002" = eps02_main, "2004" = eps04_main, "2006" = eps06_main, 
                      "2009" = eps09_main, "2015" = eps15_main, "2020" = eps20_main, .id = "año") %>% 
  arrange(folio_n20, año) %>% 
  select(-ses_child, -household,-edad_retiro,-expect_trab) %>% 
  group_by(folio_n20) %>%
  fill(apv, .direction = "down") %>%
  fill(depresion, .direction = "down") %>%
  fill(dad, .direction = "down") %>% 
  ungroup() %>% 
  filter(pos_retiro != 3) %>% 
  mutate(salud = factor(salud, levels = c(1,0), labels = c("Buena", "Mala o regular")),
         nedu = factor(nedu, levels = c(0:4), labels = c("Ninguna","Básica","Media","Universitaria","Posgrado")),
         ecivil = factor(ecivil, levels  = c(1,0), labels = c("Casado", "No casado")),
         vivienda = factor(vivienda, levels = c(1,0), labels = c("Propia","No propia")),
         apv = factor(apv, levels = c(1,0), labels = c("Si","No")),
         sit_laboral = factor(sit_laboral, levels = c(1,0), labels = c("Trabajando", "No trabajando")),
         pos_retiro = factor(pos_retiro, levels = c(1,0), labels = c("Espera trabajar","No espera trabajar")),
         oficio = factor(oficio, levels = c(1:4), labels = c("Empleador","Empleado","Independiente","Otro")),
         contrato = factor(contrato, levels = c(1,0), labels = c("Si", "No")),
         female = factor(female, levels = c(1,0), labels = c("Female","Male")),
         depresion = factor(depresion, levels = c(1,0), labels = c("Si", "No")),
         dad = factor(dad, levels = c(1,0), labels = c("Dificultad","Sin dificultad")))

single_folio_main <- eps_main %>%
  group_by(folio_n20) %>%
  summarise(count = n())

# FILTRAMOS A QUIENES TIENEN SOLO 1 WAVE
eps_main <- eps_main %>% 
  left_join(single_folio_data, by = "folio_n20") %>% 
  filter(count != 1)
# hay muchos valores perdidos en este grupo de personas, creo que deberia eliminarse.

# EN PROMEDIO HAY 2.97 WAVES
eps_main %>%
  group_by(folio_n20) %>%
  summarise(count = n()) %>%
  summarise(avg_count = mean(count))

na_main <- sapply(eps_main, function(y) sum(length(which(is.na(y)))))
print(na_main)
lapply(eps_main[,-2], table)

write_dta(eps_main, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/eps0220_main.dta")


# UNION DE DATOS SENSITIVITY

eps_sens <- bind_rows("2002" = eps02_sens, "2004" = eps04_sens, "2006" = eps06_sens, 
                      "2009" = eps09_sens, "2015" = eps15_sens, "2020" = eps20_sens, .id = "año") %>% 
  arrange(folio_n20, año) %>% 
  select(-ses_child, -household, -edad_retiro, -expect_trab) %>% 
  group_by(folio_n20) %>%
  fill(apv, .direction = "down") %>%
  fill(depresion, .direction = "down") %>%
  fill(dad, .direction = "down") %>% 
  ungroup()%>% 
  filter(pos_retiro != 3) %>% 
  mutate(salud = factor(salud, levels = c(1,0), labels = c("Buena", "Mala o regular")),
         nedu = factor(nedu, levels = c(0:4), labels = c("Ninguna","Básica","Media","Universitaria","Posgrado")),
         ecivil = factor(ecivil, levels  = c(1,0), labels = c("Casado", "No casado")),
         vivienda = factor(vivienda, levels = c(1,0), labels = c("Propia","No propia")),
         apv = factor(apv, levels = c(1,0), labels = c("Si","No")),
         sit_laboral = factor(sit_laboral, levels = c(1,0), labels = c("Trabajando", "No trabajando")),
         pos_retiro = factor(pos_retiro, levels = c(1,0), labels = c("Espera trabajar","No espera trabajar")),
         oficio = factor(oficio, levels = c(1:4), labels = c("Empleador","Empleado","Independiente","Otro")),
         contrato = factor(contrato, levels = c(1,0), labels = c("Si", "No")),
         female = factor(female, levels = c(1,0), labels = c("Female","Male")),
         depresion = factor(depresion, levels = c(1,0), labels = c("Si", "No")),
         dad = factor(dad, levels = c(1,0), labels = c("Dificultad","Sin dificultad")))

single_folio_sens <- eps_sens %>%
  group_by(folio_n20) %>%
  summarise(count = n())

# FILTRAMOS A QUIENES TIENEN SOLO 1 WAVE
eps_sens <- eps_sens %>% 
  left_join(single_folio_data, by = "folio_n20") %>% 
  filter(count != 1)
# hay muchos valores perdidos en este grupo de personas, creo que deberia eliminarse.

# EN PROMEDIO HAY 3 WAVES
eps_sens %>%
  group_by(folio_n20) %>%
  summarise(count = n()) %>%
  summarise(avg_count = mean(count))

na_sens <- sapply(eps_sens, function(y) sum(length(which(is.na(y)))))
print(na_sens)

write_dta(eps_sens, "C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/eps0220_sens.dta")

rm(list= setdiff(ls(), c("eps_main", "eps_sens")))

save.image("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/eps_final.RData")



############
# ANALYSIS #
############
load("C:/Users/Jose/Desktop/LLCS-D-22-00037 (1)/eps_final.RData")

# IMPUTATION
set.seed(2125)
eps_main_imp <- missRanger(
  eps_main, 
  formula = . ~ .  - folio_n,
  num.trees = 200, 
  returnOOB=T,
  maxiter=20,
  verbose = 2, 
  seed = 2125)
set.seed(2125)

eps_sens_imp <- missRanger(
  eps_sens, 
  formula = . ~ .  - folio_n,
  num.trees = 200, 
  returnOOB=T,
  maxiter=20,
  verbose = 2, 
  seed = 2125)

eps_main_imp <- eps_main_imp %>% 
  mutate(dad = trunc(dad),
         pos_retiro = relevel(pos_retiro, ref = "No espera trabajar")) %>% 
  select(-count)
  

eps_sens_imp <- eps_sens_imp %>% 
  mutate(dad = trunc(dad),
         pos_retiro = relevel(pos_retiro, ref = "No espera trabajar")) %>% 
  select(-count)

# DESCRIPTIVE

lapply(eps_main_imp[,c(-1,-2)], table)
lapply(eps_main_sens[,c(-1,-2)], table)


# MODELOS

mixlogit <- glmer(pos_retiro ~ vivienda + apv+ contrato + sit_laboral + oficio + 
                    salud + depresion +dad+ecivil*female + edad + (1|folio_n20),
                       data = eps_main_imp,
                       family = binomial(link = "logit"))

mixlogit_edad <- glmer(pos_retiro ~ vivienda + apv + contrato + sit_laboral + oficio 
                       + salud + depresion +dad+ecivil + edad*female + (1|folio_n20),
                              data = eps_main_imp,
                              family = binomial(link = "logit"))

summary(mixlogit)

summary(mixlogit_edad)

# Calcula los efectos marginales
efectos_marginales <- ggpredict(mixlogit, terms = "edad [all]")

efectos_marginales2 <- ggpredict(mixlogit_edad, terms = c("edad [all]", "female [all]"))

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

## SENSITIVITY ANALYSIS
mixlogit_sens <- glmer(pos_retiro ~ vivienda + apv+ contrato + sit_laboral + oficio + 
                         salud + depresion +dad+ecivil*female + edad + (1|folio_n20),
                       data = eps_sens_imp,
                       family = binomial(link = "logit"))
mixlogit_edad_sens <- glmer(pos_retiro ~ vivienda + apv + contrato + sit_laboral + oficio 
                            + salud + depresion +dad+ecivil + edad*female + (1|folio_n20),
                            data = eps_sens_imp,
                            family = binomial(link = "logit"))

# Calcula los efectos marginales
efectos_marginales_sens <- ggpredict(mixlogit_sens, terms = "edad [all]")

efectos_marginales2_sens <- ggpredict(mixlogit_edad_sens, terms = c("edad [all]", "female [all]"))

# GRÁFICOS
ggplot(efectos_marginales_sens, aes(x = x, y = predicted)) +
  geom_smooth(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_minimal() +
  labs(x = "Age",
       y = "Probability of expecting to work after retirement age") +
  scale_y_continuous(limits = c(0.2, 0.6), breaks = seq(0.2, 0.6, by = 0.1))

ggplot(efectos_marginales2_sens, aes(x = x, y = predicted)) +
  geom_smooth(aes(color = group), size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  facet_wrap(~group, ncol = 2, scales = "free_y") +
  theme_minimal() +
  labs(x = "Age",
       y = "Probability of expecting to work after retirement age") +
  scale_color_grey(start = 0, end = .7) + 
  scale_fill_grey(start = 0, end = .7)   