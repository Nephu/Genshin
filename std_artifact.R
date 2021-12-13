library(tidyverse)
library(ggplot2)
library(dplyr)

Flower <- tribble(~HP_flat_F, ~uchwyt, 4780, "A_r")

Plume <- tribble(~Atk_flat_P, ~uchwyt, 311, "A_r")

Sands <- tribble(~Atk_proc_S, ~S_typ, ~Waga_S, ~uchwyt,
                 46.6, "Atk", 1, "A_r")

Goblet <- tribble(~Atk_proc_G, ~Phys_G, ~El_dmg_G, ~G_typ, ~Waga_G, ~uchwyt,
                  46.6, 0, 0, "Atk", 1, "A_r",
                  0, 58.3, 0, "Phys", 0, "A_r",
                  0, 0, 46.6, "EL", 4, "A_r")

Circlet <- tribble(~Atk_proc_C, ~CR_C, ~CD_C, ~Heal_C, ~C_typ, ~Waga_C, ~uchwyt,
                   46.6, 0, 0, 0, "Atk", 1, "A_r",
                   0, 31.1, 0, 0, "CR", 7, "A_r",
                   0, 0, 62.2, 0, "CD", 13, "A_r",
                   0, 0, 0, 35.9, "Heal", 19, "A_r")

avgaf <- tribble(~sub_af, ~waf, ~ile, ~uchwyt,
                   "a", 0, 0, "A_r",
                   "a", 16.535, 1, "A_r",
                   "a", 99.21, 6, "A_r")

avgap <- tribble(~sub_ap, ~wap, ~ile, ~uchwyt,
                   "A", 0, 0, "A_r",
                   "A", 4.955, 1, "A_r",
                   "A", 29.73, 6, "A_r")

avgcr <- tribble(~sub_cr, ~wcr, ~ile, ~uchwyt,
                 "C", 0, 0, "A_r",
                 "C", 3.305, 1, "A_r",
                 "C", 19.83, 6, "A_r")

avgcd <- tribble(~sub_cd, ~wcd, ~ile, ~uchwyt,
                 "X", 0, 0, "A_r",
                 "X", 6.605, 1, "A_r",
                 "X", 39.63, 6, "A_r")

avg_sub_stat <- left_join( avgap, avgaf, "uchwyt") %>%
  left_join( avgcr, "uchwyt") %>%
  left_join( avgcd, "uchwyt") %>%
  mutate( ile_razem = ile.x + ile.y + ile.x.x + ile.y.y) %>%
  filter( ile_razem %in% c(8,9) ) %>%
  mutate( Art_sub_type = paste( if_else(ile.x <= 1, "", sub_ap ),
                                if_else(ile.y <= 1, "", sub_af ),
                                if_else(ile.x.x <= 1, "", sub_cr ),
                                if_else(ile.y.y <= 1, "", sub_cd ) ))

avg_sub_stat1 <- avg_sub_stat %>%
  select( -ile.x, -ile.y, -ile.x.x, -ile.y.y ) %>%
  mutate( Art_sub_type = trimws(Art_sub_type) ) %>%
  select( -sub_cr, -sub_cd, -sub_af, -sub_ap)

FFlower <- left_join(Flower, avg_sub_stat1, "uchwyt") %>% 
  filter( ile_razem == 9) %>%
  select( -ile_razem)

PPlume <- left_join(Plume, avg_sub_stat1, "uchwyt") %>% 
  filter( ile_razem == 9) %>%
  select( -ile_razem)

ile_subart <- function( wejscie ){
  a_ilosc = str_count( wejscie, "a")
  A_ilosc = str_count( wejscie, "A")
  C_ilosc = str_count( wejscie, "C")
  X_ilosc = str_count( wejscie, "X")
  a_war = if_else( a_ilosc == 0, "", if_else( a_ilosc == 1, "a ", paste( a_ilosc, "a ", sep = "" ) )  )
  A_war = if_else( A_ilosc == 0, "", if_else( A_ilosc == 1, "A ", paste( A_ilosc, "A ", sep = "" ) )  )
  C_war = if_else( C_ilosc == 0, "", if_else( C_ilosc == 1, "C ", paste( C_ilosc, "C ", sep = "" ) )  )
  X_war = if_else( X_ilosc == 0, "", if_else( X_ilosc == 1, "X ", paste( X_ilosc, "X ", sep = "" ) )  )
  wyjscie = paste( a_war, A_war, C_war, X_war, sep = "")
}

Art_FP <- left_join(FFlower, PPlume, "uchwyt") %>%
  mutate( Art_sub_fp = paste(Art_sub_type.x, Art_sub_type.y, sep = "") ) %>%
  select( -Art_sub_type.x, -Art_sub_type.y) %>%
  mutate( wap = wap.x + wap.y, waf = waf.x + waf.y, wcr = wcr.x + wcr.y, wcd = wcd.x + wcd.y) %>%
  select( -wap.x, -waf.x, -wcr.x, -wcd.x, -wap.y, -waf.y, -wcr.y,  -wcd.y)

SSands <- left_join(Sands, avg_sub_stat1, "uchwyt") %>%
  filter( wap == 0) %>%
  select( -ile_razem, -Waga_S)
 
ARt_SFP <- left_join( Art_FP, SSands, "uchwyt") %>%
  mutate( Art_sub_sfp = paste(Art_sub_fp, Art_sub_type, sep = "") ) %>%
  select( -Art_sub_fp, -Art_sub_type) %>%
  mutate( wap = wap.x + wap.y, waf = waf.x + waf.y, wcr = wcr.x + wcr.y, wcd = wcd.x + wcd.y) %>%
  select( -wap.x, -waf.x, -wcr.x, -wcd.x, -wap.y, -waf.y, -wcr.y,  -wcd.y)

GGoblet <- left_join(Goblet, avg_sub_stat1, "uchwyt") %>%
  filter( !(G_typ == "Atk" & wap > 0), !(G_typ == "Phys" & ile_razem == 8), !(G_typ == "EL" & ile_razem == 8) ) %>%
  select( -ile_razem, -Waga_G)
  
ARt_GSFP <- left_join( ARt_SFP, GGoblet, "uchwyt") %>%
  mutate( Art_sub_gsfp = paste(Art_sub_sfp, Art_sub_type, sep = "") ) %>%
  select( -Art_sub_sfp, -Art_sub_type) %>%
  mutate( wap = wap.x + wap.y, waf = waf.x + waf.y, wcr = wcr.x + wcr.y, wcd = wcd.x + wcd.y) %>%
  select( -wap.x, -waf.x, -wcr.x, -wcd.x, -wap.y, -waf.y, -wcr.y,  -wcd.y)

CCirclet <- left_join(Circlet, avg_sub_stat1, "uchwyt") %>%
  filter( !(Waga_C == 1 & wap > 0), !(Waga_C == 7 & wcr > 0), !(Waga_C == 13 & wcd > 0) ) %>%
  filter( !(Waga_C == 19 & ile_razem == 8) ) %>%
  select( -ile_razem, -Waga_C)

Artifact_X <- left_join( ARt_GSFP, CCirclet, "uchwyt")

Artifact <- Artifact_X %>%
  mutate( Art_sub_x = paste(Art_sub_gsfp, Art_sub_type, sep = "") ) %>%
  select( -Art_sub_gsfp, -Art_sub_type) %>%
  mutate( wap = wap.x + wap.y, waf = waf.x + waf.y, wcr = wcr.x + wcr.y, wcd = wcd.x + wcd.y) %>%
  select( -wap.x, -waf.x, -wcr.x, -wcd.x, -wap.y, -waf.y, -wcr.y,  -wcd.y) %>%
  mutate( Art_type = paste(S_typ, G_typ, C_typ, sep = " ") ) %>%
  select( -S_typ, -G_typ, -C_typ) %>%
  mutate( Art_AP = Atk_proc_S + Atk_proc_G + Atk_proc_C + wap) %>%
  select( -Atk_proc_S, -Atk_proc_G, -Atk_proc_C, -wap) %>%
  mutate( Art_AF = Atk_flat_P + waf) %>%
  select( -Atk_flat_P, -waf) %>%
  mutate( Art_CR = CR_C + wcr, Art_CD = CD_C + wcd) %>%
  select( -CR_C, -wcr, -CD_C, -wcd) %>%
  mutate( Art_sub_x = trimws(ile_subart(Art_sub_x)) ) %>%
  distinct(Art_type, Art_sub_x, .keep_all = TRUE)
