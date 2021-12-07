library(tidyverse)
library(ggplot2)
library(dplyr)
source("std_artifact.R")

Jean <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~CritR, ~CritD, ~Wpn, ~ASet,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 0, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 0, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 0, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 0, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 0, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 0, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 0, 0, 5, 50, 0, 0
                )


Weapon <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~CritR, ~CritD, ~Wpn, ~ASet,
                  542, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0, 44.1, 0, 0, 0,
                  674, 20, 0, 0, 0, 0, 0, 41.3, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                  608, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0,
                  510, 41.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                  565, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36.8, 4, 0,
                  565, 16, 0, 0, 0, 0, 0, 34.5, 0, 0, 0, 0, 0, 0, 0, 5, 0,
                  454, 55.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0
                  )

Weapon_name <- tribble( ~Wpn, ~WName,
                        0, "Primordial_Jade_Cutter",  # + buff 
                        1, "Aquila_Favonia",
                        2, "Skyward_Blade^on_buff", # + buff
                        3, "The_Flute", 
                        4, "Blackcliff_Longsword^3stack",
                        5, "Prototype_Rancour^4stack",
                        6, "Amenoma_Kageuchi")                

Sett <- Weapon + Jean
Sett <- Sett %>% mutate( uchwyt = c("A_r") )

Set_name <- tribble(~ASet, ~Name_Set,
                    0, "Bez_setu",
                    1, "2 Bloodstained + 2 Pale Flame",
                    2, "4 Gladiator", 
                    3, "2 Gladiator + 2 Shimenawa", 
                    4, "4 Viridescent",
                    5, "")

ArtSet <- tribble(~Atk_AS, ~HP_AS, ~ELbuffAS, ~HealAS, ~PhysAS,
                  ~N_AS, ~CH_AS, ~E_AS, ~Q_AS, ~CrAS, ~CdAS, ~ASetAS, ~uchwyt,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "A_r",
                  0, 0, 0, 0, 50, 0, 0, 0, 0, 0, 0, 1, "A_r",
                  18, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 2, "A_r",
                  36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, "A_r",
                  0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 4, "A_r",
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, "A_r",)

Sett2 <- left_join( Sett, ArtSet, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Atk_AS, HP_proc = HP_proc + HP_AS,
          ELbuff = ELbuff + ELbuffAS, Heal = Heal + HealAS, Phys = Phys + PhysAS,
          N_bonus = N_bonus + N_AS, CH_bonus = CH_bonus + CH_AS, 
          E_bonus = E_bonus + E_AS, Q_bonus = Q_bonus + Q_AS,
          CritR = CritR + CrAS, CritD = CritD + CdAS, ASet = ASet + ASetAS) %>%
  select(1:18,)

Staty <- left_join( Sett2, Artifact, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Art_AP,
         ELbuff = ELbuff + El_dmg_G, Heal = Heal + HealAS, Phys = Phys + PhysAS,
         N_bonus = N_bonus + N_AS, CH_bonus = CH_bonus + CH_AS, 
         E_bonus = E_bonus + E_AS, Q_bonus = Q_bonus + Q_AS,
         CritR = CritR + CrAS, CritD = CritD + CdAS, ASet = ASet + ASetAS)
