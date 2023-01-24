library(tidyverse)
library(ggplot2)
library(dplyr)

Flower <- tribble(~HP_flat_F, ~uchwyt, 4780, "A_r")

Plume <- tribble(~Atk_flat_P, ~uchwyt, 311, "A_r")

Sands <- tribble(~Atk_proc_S, ~P_S, ~S_typ, ~Waga_S, ~uchwyt,
                 46.6, 0, "Atk", 1, "A_r",
                 0, 1, "P", 0, "A_r")

Goblet <- tribble(~Atk_proc_G, ~Phys_G, ~El_dmg_G, ~P_G, ~G_typ, ~Waga_G, ~uchwyt,
                  46.6, 0, 0, 0, "Atk", 1, "A_r",
                  0, 58.3, 0, 0, "Phys", 7, "A_r",
                  0, 0, 46.6, 0, "EL", 4, "A_r",
                  0, 0, 0, 1, "P", 0, "A_r")

Circlet <- tribble(~Atk_proc_C, ~CR_C, ~CD_C, ~Heal_C, ~P_C, ~C_typ, ~Waga_C, ~uchwyt,
                   46.6, 0, 0, 0, 0, "Atk", 1, "A_r",
                   0, 31.1, 0, 0, 0, "CR", 10, "A_r",
                   0, 0, 62.2, 0, 0, "CD", 19, "A_r",
                   0, 0, 0, 35.9, 0, "Heal", 28, "A_r",
                   0, 0, 0, 0, 1, "P", 0, "A_r")

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

avghdp <- tribble(~sub_hdp, ~ilehdp, ~uchwyt,
                  "P", 0, "A_r",
                  "P", 1, "A_r",
                  "P", 6, "A_r")

avghdf <- tribble(~sub_hdf, ~ilehdf, ~uchwyt,
                  "f", 0, "A_r",
                  "f", 1, "A_r",
                  "f", 6, "A_r")


#avg_sub_stat <- left_join( avgap, avgaf, "uchwyt") %>%
#left_join( avgcr, "uchwyt") %>%
avg_sub_stat <- left_join( avgap, avgcr, "uchwyt") %>%
  left_join( avgcd, "uchwyt") %>%
  left_join( avghdp, "uchwyt") %>%
  left_join( avghdf, "uchwyt") %>%
#  mutate( ile_razem = ileap + ileaf + ilecr + ilecd + ilehdp + ilehdf) %>%
  mutate( ile_razem = ileap + ilecr + ilecd + ilehdp + ilehdf) %>%
  filter( ile_razem == 9 ) %>%
  mutate( Art_sub_type = paste( if_else(ileap <= 1, "", sub_ap ),
#                                if_else(ileaf <= 1, "", sub_af ),
                                if_else(ilecr <= 1, "", sub_cr ),
                                if_else(ilecd <= 1, "", sub_cd ),
                                if_else(ilehdp <= 1, "", sub_hdp ),
                                if_else(ilehdf <= 1, "", sub_hdf )))

avg_sub_stat1 <- avg_sub_stat %>%
  mutate( Art_sub_type = trimws(Art_sub_type) ) %>%
  select( -sub_cr, -sub_cd, -sub_ap, -sub_hdp, -sub_hdf)
#  select( -sub_cr, -sub_cd, -sub_af, -sub_ap, -sub_hdp, -sub_hdf)

FFlower <- left_join(Flower, avg_sub_stat1, "uchwyt") %>% 
  select( -ile_razem)

PPlume <- left_join(Plume, avg_sub_stat1, "uchwyt") %>% 
  select( -ile_razem)

ile_subart <- function( wejscie ){
  a_ilosc = str_count( wejscie, "a")
  A_ilosc = str_count( wejscie, "A")
  C_ilosc = str_count( wejscie, "C")
  X_ilosc = str_count( wejscie, "X")
  P_ilosc = str_count( wejscie, "P")
  f_ilosc = str_count( wejscie, "f")
  a_war = if_else( a_ilosc == 0, "", if_else( a_ilosc == 1, "a ", paste( a_ilosc, "a ", sep = "" ) )  )
  A_war = if_else( A_ilosc == 0, "", if_else( A_ilosc == 1, "A ", paste( A_ilosc, "A ", sep = "" ) )  )
  C_war = if_else( C_ilosc == 0, "", if_else( C_ilosc == 1, "C ", paste( C_ilosc, "C ", sep = "" ) )  )
  X_war = if_else( X_ilosc == 0, "", if_else( X_ilosc == 1, "X ", paste( X_ilosc, "X ", sep = "" ) )  )
  P_war = if_else( P_ilosc == 0, "", if_else( P_ilosc == 1, "P ", paste( P_ilosc, "P ", sep = "" ) )  )
  f_war = if_else( f_ilosc == 0, "", if_else( f_ilosc == 1, "f ", paste( f_ilosc, "f ", sep = "" ) )  )
  wyjscie = paste( a_war, A_war, C_war, X_war, P_war, f_war, sep = "")
}

Art_FP <- left_join(FFlower, PPlume, "uchwyt") %>%
  mutate( Art_sub_fp = paste(Art_sub_type.x, Art_sub_type.y, sep = "") ) %>%
  select( -Art_sub_type.x, -Art_sub_type.y) %>%
  mutate( ileap = ileap.x + ileap.y, #ileaf = ileaf.x + ileaf.y,
          ilecr = ilecr.x + ilecr.y,
          ilecd = ilecd.x + ilecd.y, ilehdp = ilehdp.x + ilehdp.y, ilehdf = ilehdf.x + ilehdf.y) %>%
  select( -ileap.x, #-ileaf.x,
          -ilecr.x, -ilecd.x, -ileap.y, #-ileaf.y,
          -ilecr.y, -ilecd.y,
          -ilehdp.x, -ilehdp.y, -ilehdf.x, -ilehdf.y) %>%
  distinct( Art_sub_fp, ileap, #ileaf,
            ilecr, ilecd, ilehdp, ilehdf, .keep_all = TRUE  )

SSands <- left_join(Sands, avg_sub_stat1, "uchwyt") %>%
  filter( (Waga_S == 1 & ileap == 0) | (Waga_S == 0 & ilehdp == 0)) %>%
  select( -ile_razem)

Art_SFP <- left_join( Art_FP, SSands, "uchwyt") %>%
  mutate( Art_sub_sfp = paste(Art_sub_fp, Art_sub_type, sep = "") ) %>%
  select( -Art_sub_fp, -Art_sub_type) %>%
  mutate( ileap = ileap.x + ileap.y, #ileaf = ileaf.x + ileaf.y,
          ilecr = ilecr.x + ilecr.y,
          ilecd = ilecd.x + ilecd.y, ilehdp = ilehdp.x + ilehdp.y, ilehdf = ilehdf.x + ilehdf.y) %>%
  select( -ileap.x, #-ileaf.x,
          -ilecr.x, -ilecd.x, -ileap.y, #-ileaf.y,
          -ilecr.y, -ilecd.y,
          -ilehdp.x, -ilehdp.y, -ilehdf.x, -ilehdf.y) %>%
  distinct( Art_sub_sfp, ileap, #ileaf,
            ilecr, ilecd, ilehdp, ilehdf, .keep_all = TRUE  )

GGoblet <- left_join(Goblet, avg_sub_stat1, "uchwyt") %>%
  filter( (Waga_G == 1 & ileap == 0) | (Waga_G == 0 &  ilehdp == 0) | Waga_G == 7 | Waga_G == 4 ) %>%
  select( -ile_razem)

Art_GSFP <- left_join( Art_SFP, GGoblet, "uchwyt") %>%
  mutate( Art_sub_gsfp = paste(Art_sub_sfp, Art_sub_type, sep = "") ) %>%
  select( -Art_sub_sfp, -Art_sub_type) %>%
  mutate( ileap = ileap.x + ileap.y, #ileaf = ileaf.x + ileaf.y,
          ilecr = ilecr.x + ilecr.y, ilecd = ilecd.x + ilecd.y,
          ilehdp = ilehdp.x + ilehdp.y, ilehdf = ilehdf.x + ilehdf.y) %>%
  select( -ileap.x, #-ileaf.x,
          -ilecr.x, -ilecd.x, -ileap.y, #-ileaf.y,
          -ilecr.y, -ilecd.y,
          -ilehdp.x, -ilehdp.y, -ilehdf.x, -ilehdf.y) %>%
  distinct( Art_sub_gsfp, S_typ, G_typ, ileap, #ileaf,
            ilecr, ilecd, ilehdp, ilehdf, .keep_all = TRUE  )

CCirclet <- left_join(Circlet, avg_sub_stat1, "uchwyt") %>%
  filter( (Waga_C == 1 & ileap == 0) | (Waga_C == 10 & ilecr == 0) | 
            (Waga_C == 19 & ilecd == 0) | (Waga_C == 0 & ilehdp == 0) | Waga_C == 28 ) %>%
  select( -ile_razem)

rm(Art_FP, Art_SFP)

Artifact_X <- left_join( Art_GSFP, CCirclet, "uchwyt")

Artifact <- Artifact_X %>%
  mutate( Art_sub_x = paste(Art_sub_gsfp, Art_sub_type, sep = "") ) %>%
  select( -Art_sub_gsfp, -Art_sub_type) %>%
  mutate( ileap = ileap.x + ileap.y, #ileaf = ileaf.x + ileaf.y,
          ilecr = ilecr.x + ilecr.y, ilecd = ilecd.x + ilecd.y,
          ilehdp = ilehdp.x + ilehdp.y, ilehdf = ilehdf.x + ilehdf.y) %>%
  select( -ileap.x, #-ileaf.x,
          -ilecr.x, -ilecd.x, -ileap.y, #-ileaf.y,
          -ilecr.y, -ilecd.y, -ilehdp.x, -ilehdp.y, -ilehdf.x, -ilehdf.y) %>%
  mutate( Art_type = paste(S_typ, G_typ, C_typ, sep = " ") ) %>%
  distinct( Art_sub_x, Art_type, ileap, #ileaf,
            ilecr, ilecd, ilehdp, ilehdf, .keep_all = TRUE  ) %>%
  select( -S_typ, -G_typ, -C_typ) %>%
  mutate( ileap = ileap * 4.955,
          ileaf = 0 * 16.535,
          ilecr = ilecr * 3.305,
          ilecd = ilecd * 6.605,
          iledp = ilehdp * 6.195,
          iledf = ilehdf * 19.675,
          ilehp = ilehdp * 4.955,
          ilehf = ilehdf * 253.94,
          ileem = 0 * 19.815,
          ileer = 0 * 5.505
  ) %>%
  mutate( Art_AP = Atk_proc_S + Atk_proc_G + Atk_proc_C + ileap) %>%
  select( -Atk_proc_S, -Atk_proc_G, -Atk_proc_C, -ileap) %>%
  mutate( Art_AF = Atk_flat_P + ileaf) %>%
  select( -Atk_flat_P, -ileaf) %>%
  mutate( Art_CR = CR_C + ilecr, Art_CD = CD_C + ilecd) %>%
  select( -CR_C, -ilecr, -CD_C, -ilecd) %>%
  mutate( Art_HP = (P_C + P_G + P_S)*46.6 + ilehp,
          Art_HF = HP_flat_F + ilehf,
          Art_DP = (P_C + P_G + P_S)*58.3 + iledp,
          Art_DF = iledf, Art_ER = 0 ) %>%
  select( -P_S, -P_G, -P_C, -ilehp, -ilehf, -iledp, -iledf, -ileem, -ileer ) %>%
  mutate( Art_sub_x = trimws(ile_subart(Art_sub_x)) ) %>%
  distinct(Art_type, Art_sub_x, Art_AP, Art_AF, Art_CR, Art_CD,
           Art_HP, Art_HF, Art_DP, Art_DF, .keep_all = TRUE)

Artifact_H <- Artifact %>% 
  filter( !( Waga_S == 0 & Waga_G == 1) & !( Waga_S == 0 & Waga_C == 1) & !( Waga_G == 0 & Waga_C == 1 ) ) %>%
  select( -Art_DP, -Art_DF, -Waga_S, -Waga_G, -Waga_C)

Artifact_D <- Artifact %>% 
  filter( !( Waga_S == 0 & Waga_G == 1) & !( Waga_S == 0 & Waga_C == 1) & !( Waga_G == 0 & Waga_C == 1 ) ) %>%
  select( -Art_HP, -Art_HF, -Waga_S, -Waga_G, -Waga_C)
