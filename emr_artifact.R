library(tidyverse)
library(ggplot2)
library(dplyr)

Flower <- tribble(~HP_flat_F, ~uchwyt, 4780, "A_r")

Plume <- tribble(~Atk_flat_P, ~uchwyt, 311, "A_r")

Sands <- tribble(~Atk_proc_S, ~E_S, ~S_typ, ~Waga_S, ~uchwyt,
                 46.6, 0, "Atk", 1, "A_r",
                 0, 1, "E", 0, "A_r")

Goblet <- tribble(~Atk_proc_G, ~Phys_G, ~El_dmg_G, ~E_G, ~G_typ, ~Waga_G, ~uchwyt,
                  46.6, 0, 0, 0, "Atk", 1, "A_r",
                  0, 58.3, 0, 0, "Phys", 7, "A_r",
                  0, 0, 46.6, 0, "EL", 4, "A_r",
                  0, 0, 0, 1, "E", 0, "A_r")

Circlet <- tribble(~Atk_proc_C, ~CR_C, ~CD_C, ~Heal_C, ~E_C, ~C_typ, ~Waga_C, ~uchwyt,
                   46.6, 0, 0, 0, 0, "Atk", 1, "A_r",
                   0, 31.1, 0, 0, 0, "CR", 10, "A_r",
                   0, 0, 62.2, 0, 0, "CD", 19, "A_r",
                   0, 0, 0, 35.9, 0, "Heal", 28, "A_r",
                   0, 0, 0, 0, 1, "E", 0, "A_r")

avgaf <- tribble(~sub_af,  ~ileaf, ~uchwyt,
                 "a", 0, "A_r",
                 "a", 1, "A_r",
                 "a", 6, "A_r")

avgap <- tribble(~sub_ap, ~ileap, ~uchwyt,
                 "A", 0, "A_r",
                 "A", 1, "A_r",
                 "A", 6, "A_r")

avgcr <- tribble(~sub_cr, ~ilecr, ~uchwyt,
                 "C", 0, "A_r",
                 "C", 1, "A_r",
                 "C", 6, "A_r")

avgcd <- tribble(~sub_cd, ~ilecd, ~uchwyt,
                 "X", 0, "A_r",
                 "X", 1, "A_r",
                 "X", 6, "A_r")

avgemr <- tribble(~sub_emr, ~ileemr, ~uchwyt,
                 "E", 0, "A_r",
                 "E", 1, "A_r",
                 "E", 6, "A_r")


avg_sub_stat <- left_join( avgap, avgaf, "uchwyt") %>%
  left_join( avgcr, "uchwyt") %>%
  left_join( avgcd, "uchwyt") %>%
  left_join( avgemr, "uchwyt") %>%
  mutate( ile_razem = ileap + ileaf + ilecr + ilecd + ileemr) %>%
  filter( ile_razem == 9 ) %>%
  mutate( Art_sub_type = paste( if_else(ileap <= 1, "", sub_ap ),
                                if_else(ileaf <= 1, "", sub_af ),
                                if_else(ilecr <= 1, "", sub_cr ),
                                if_else(ilecd <= 1, "", sub_cd ),
                                if_else(ileemr <= 1, "", sub_emr )))

avg_sub_stat1 <- avg_sub_stat %>%
  mutate( Art_sub_type = trimws(Art_sub_type) ) %>%
  select( -sub_cr, -sub_cd, -sub_af, -sub_ap, -sub_emr)

FFlower <- left_join(Flower, avg_sub_stat1, "uchwyt") %>% 
  select( -ile_razem)

PPlume <- left_join(Plume, avg_sub_stat1, "uchwyt") %>% 
  select( -ile_razem)

ile_subart <- function( wejscie ){
  a_ilosc = str_count( wejscie, "a")
  A_ilosc = str_count( wejscie, "A")
  C_ilosc = str_count( wejscie, "C")
  X_ilosc = str_count( wejscie, "X")
  E_ilosc = str_count( wejscie, "E")
  a_war = if_else( a_ilosc == 0, "", if_else( a_ilosc == 1, "a ", paste( a_ilosc, "a ", sep = "" ) )  )
  A_war = if_else( A_ilosc == 0, "", if_else( A_ilosc == 1, "A ", paste( A_ilosc, "A ", sep = "" ) )  )
  C_war = if_else( C_ilosc == 0, "", if_else( C_ilosc == 1, "C ", paste( C_ilosc, "C ", sep = "" ) )  )
  X_war = if_else( X_ilosc == 0, "", if_else( X_ilosc == 1, "X ", paste( X_ilosc, "X ", sep = "" ) )  )
  E_war = if_else( E_ilosc == 0, "", if_else( E_ilosc == 1, "E ", paste( E_ilosc, "E ", sep = "" ) )  )
  wyjscie = paste( a_war, A_war, C_war, X_war, E_war, sep = "")
}

Art_FP <- left_join(FFlower, PPlume, "uchwyt") %>%
  mutate( Art_sub_fp = paste(Art_sub_type.x, Art_sub_type.y, sep = "") ) %>%
  select( -Art_sub_type.x, -Art_sub_type.y) %>%
  mutate( ileap = ileap.x + ileap.y, ileaf = ileaf.x + ileaf.y, ilecr = ilecr.x + ilecr.y, ilecd = ilecd.x + ilecd.y, ileemr = ileemr.x + ileemr.y) %>%
  select( -ileap.x, -ileaf.x, -ilecr.x, -ilecd.x, -ileap.y, -ileaf.y, -ilecr.y, -ilecd.y, -ileemr.x, -ileemr.y) %>%
  distinct( Art_sub_fp, ileap, ileaf, ilecr, ilecd, ileemr, .keep_all = TRUE  )

SSands <- left_join(Sands, avg_sub_stat1, "uchwyt") %>%
  filter( (Waga_S == 1 & ileap == 0) | (Waga_S == 0 & ileemr == 0)) %>%
  select( -ile_razem)

Art_SFP <- left_join( Art_FP, SSands, "uchwyt") %>%
  mutate( Art_sub_sfp = paste(Art_sub_fp, Art_sub_type, sep = "") ) %>%
  select( -Art_sub_fp, -Art_sub_type) %>%
  mutate( ileap = ileap.x + ileap.y, ileaf = ileaf.x + ileaf.y, ilecr = ilecr.x + ilecr.y, ilecd = ilecd.x + ilecd.y, ileemr = ileemr.x + ileemr.y) %>%
  select( -ileap.x, -ileaf.x, -ilecr.x, -ilecd.x, -ileap.y, -ileaf.y, -ilecr.y, -ilecd.y, -ileemr.x, -ileemr.y) %>%
  distinct( Art_sub_sfp, ileap, ileaf, ilecr, ilecd, ileemr, .keep_all = TRUE  )

GGoblet <- left_join(Goblet, avg_sub_stat1, "uchwyt") %>%
  filter( (Waga_G == 1 & ileap == 0) | (Waga_G == 0 &  ileemr == 0) | Waga_G == 7 | Waga_G == 4 ) %>%
  select( -ile_razem)

Art_GSFP <- left_join( Art_SFP, GGoblet, "uchwyt") %>%
  mutate( Art_sub_gsfp = paste(Art_sub_sfp, Art_sub_type, sep = "") ) %>%
  select( -Art_sub_sfp, -Art_sub_type) %>%
  mutate( ileap = ileap.x + ileap.y, ileaf = ileaf.x + ileaf.y, ilecr = ilecr.x + ilecr.y, ilecd = ilecd.x + ilecd.y, ileemr = ileemr.x + ileemr.y) %>%
  select( -ileap.x, -ileaf.x, -ilecr.x, -ilecd.x, -ileap.y, -ileaf.y, -ilecr.y, -ilecd.y, -ileemr.x, -ileemr.y) %>%
  distinct( Art_sub_gsfp, S_typ, G_typ, ileap, ileaf, ilecr, ilecd, ileemr, .keep_all = TRUE  )

CCirclet <- left_join(Circlet, avg_sub_stat1, "uchwyt") %>%
  filter( (Waga_C == 1 & ileap == 0) | (Waga_C == 10 & ilecr == 0) | 
            (Waga_C == 19 & ilecd == 0) | (Waga_C == 0 & ileemr == 0) | Waga_C == 28 ) %>%
  select( -ile_razem)

Artifact_X <- left_join( Art_GSFP, CCirclet, "uchwyt")

Artifact <- Artifact_X %>%
  mutate( Art_sub_x = paste(Art_sub_gsfp, Art_sub_type, sep = "") ) %>%
  select( -Art_sub_gsfp, -Art_sub_type) %>%
  mutate( ileap = ileap.x + ileap.y, ileaf = ileaf.x + ileaf.y, ilecr = ilecr.x + ilecr.y, ilecd = ilecd.x + ilecd.y, ileemr = ileemr.x + ileemr.y) %>%
  select( -ileap.x, -ileaf.x, -ilecr.x, -ilecd.x, -ileap.y, -ileaf.y, -ilecr.y, -ilecd.y, -ileemr.x, -ileemr.y) %>%
  mutate( Art_type = paste(S_typ, G_typ, C_typ, sep = " ") ) %>%
  distinct( Art_sub_x, Art_type, ileap, ileaf, ilecr, ilecd, ileemr, .keep_all = TRUE  ) %>%
  select( -S_typ, -G_typ, -C_typ) %>%
  mutate( ileap = ileap * 4.955,
          ileaf = ileaf * 16.535,
          ilecr = ilecr * 3.305,
          ilecd = ilecd * 6.605,
          ileem = ileemr * 19.815,
          ileer = ileemr * 5.505
          ) %>%
  mutate( Art_AP = Atk_proc_S + Atk_proc_G + Atk_proc_C + ileap) %>%
  select( -Atk_proc_S, -Atk_proc_G, -Atk_proc_C, -ileap) %>%
  mutate( Art_AF = Atk_flat_P + ileaf) %>%
  select( -Atk_flat_P, -ileaf) %>%
  mutate( Art_CR = CR_C + ilecr, Art_CD = CD_C + ilecd) %>%
  select( -CR_C, -ilecr, -CD_C, -ilecd) %>%
  mutate( Art_EM = (E_C + E_G + E_S)*186.5 + ileem,
          Art_ER = E_S *  51.8 + ileer ) %>%
  select( -E_S, -ileemr, -ileem, -ileer ) %>%
  mutate( Art_sub_x = trimws(ile_subart(Art_sub_x)) ) %>%
  distinct(Art_type, Art_sub_x, Art_AP, Art_AF, Art_CR, Art_CD, Art_EM, Art_ER, .keep_all = TRUE)

Artifact_EM <- Artifact %>% filter( !( Waga_S == 0 & Waga_G == 1) & !( Waga_S == 0 & Waga_C == 1) ) %>%
  select(-E_G, -E_C, -Art_ER, -Waga_S, -Waga_G, -Waga_C)

Artifact_ER <- Artifact %>% filter( E_G == 0 & E_C == 0 ) %>% 
  select( -E_G, -E_C, -Art_EM, -Waga_S, -Waga_G, -Waga_C)
