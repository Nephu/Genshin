library(tidyverse)
library(ggplot2)
library(dplyr)
source("std_artifact.R")

Jean <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~CritR, ~CritD, ~Wpn, ~ASet,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 40, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 40, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 40, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 40, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 40, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 40, 0, 5, 50, 0, 0,
                  239, 0, 0, 14695, 0, 0, 0, 0, 22.2, 0, 0, 40, 0, 5, 50, 0, 0
                )


Weapon <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~CritR, ~CritD, ~Wpn, ~ASet,
                  542, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0, 44.1, 0, 0, 0,
                  674, 20, 0, 0, 0, 0, 0, 41.3, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                  608, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 2, 0,
                  510, 41.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                  565, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36.8, 4, 0,
                  565, 16, 0, 0, 0, 0, 0, 34.5, 0, 0, 0, 0, 0, 0, 0, 5, 0,
                  454, 55.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0
                  )

Weapon_name <- tribble( ~Wpn, ~WName,
                        0, "Primordial_Jade_Cutter", 
                        1, "Aquila_Favonia",
                        2, "Skyward_Blade^on_buff",
                        3, "The_Flute", 
                        4, "Blackcliff_Longsword^3stack",
                        5, "Prototype_Rancour^4stack",
                        6, "Amenoma_Kageuchi")                

Sett <- Weapon + Jean
Sett <- Sett %>% mutate( uchwyt = c("A_r") )

Set_name <- tribble(~ASet, ~Name_Set,
                    #0, "Bez_setu",
                    1, "2 Blood. + 2 Pale Flame",
                    2, "4 Gladiator", 
                    3, "2 Glad. + 2 Shime.", 
                    4, "4 Viridescent",
                    5, "4 Ocean-Hued Clam",
                    6, "2 Glad. + 2 Virid.")

ArtSet <- tribble(~Atk_AS, ~HP_AS, ~ELbuffAS, ~HealAS, ~PhysAS,
                  ~N_AS, ~CH_AS, ~E_AS, ~Q_AS, ~CrAS, ~CdAS, ~ASetAS, ~uchwyt,
                  #0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "A_r",
                  0, 0, 0, 0, 50, 0, 0, 0, 0, 0, 0, 1, "A_r",
                  18, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 2, "A_r",
                  36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, "A_r",
                  0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 4, "A_r",
                  0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 5, "A_r",
                  18, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 6, "A_r")

Sett2 <- left_join( Sett, ArtSet, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Atk_AS, HP_proc = HP_proc + HP_AS,
          ELbuff = ELbuff + ELbuffAS, Heal = Heal + HealAS, Phys = Phys + PhysAS,
          N_bonus = N_bonus + N_AS, CH_bonus = CH_bonus + CH_AS, 
          E_bonus = E_bonus + E_AS, Q_bonus = Q_bonus + Q_AS,
          CritR = CritR + CrAS, CritD = CritD + CdAS, ASet = ASet + ASetAS) %>%
  select(1:18,)

Statystyki <- left_join( Sett2, Artifact, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Art_AP, Atk_flat = Atk_flat + Art_AF, HP_flat = HP_flat + HP_flat_F,
         ELbuff = ELbuff + El_dmg_G, Heal = Heal + Heal_C, Phys = Phys + Phys_G,
         CritR = CritR + Art_CR, CritD = CritD + Art_CD) %>%
  select( -HP_flat_F, -Phys_G, -El_dmg_G, -Heal_C, -Art_AP, -Art_AF, -Art_CR, -Art_CD)

Statystyki2 <- left_join( Statystyki, Weapon_name) %>% left_join( Set_name) %>%
  mutate( HP = HP_base*(1+HP_proc/100) + HP_flat,
          Atk_flat = if_else( Wpn != 0, Atk_flat, Atk_flat + HP*1.2/100 ),
          ATK = Atk_base*(1+Atk_proc/100) + Atk_flat ) %>%
  mutate( NATK = ( ATK*if_else( Wpn != 2, 118.32/100, 138.32/100) )*(1 + (N_bonus + Phys)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          CATK = ( ATK*if_else( Wpn != 2, 320.28/100, 340.28/100) )*(1 + (CH_bonus + Phys)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          EATK = ( ATK*525.6/100)*(1 + (E_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          QATK = ( ATK*764.64/100)*(1 + (Q_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          Healing = ( ATK*452.16/100 + 3389)*(1 + Heal/100 ),
          Regen = ( ATK*45.22/100 + 339)*(1 + Heal/100 )
           )

avgStaty <- Statystyki2 %>% group_by( WName, Wpn, Name_Set,
                                      ASet, Art_type ) %>% summarise( Normal = mean(NATK),
                                                                                 Charged = mean(CATK),
                                                                                 Elemental = mean(EATK),
                                                                                 Burst = mean(QATK), 
                                                                                 Healing = mean(Healing), 
                                                                                 Regen = mean(Regen) ) %>% ungroup()
avgStaty2 <- avgStaty %>% gather("Rodzaj_ataku", "DMG", 6:11)

staty_pod_N <- Statystyki2 %>% arrange( desc(NATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_N)[25:28] = c("Normal", "Charged", "Elemental", "Burst")
staty_pod_N <- staty_pod_N %>% gather("Rodzaj_ataku", "DMG", 25:30)
  
staty_pod_C <- Statystyki2 %>% arrange( desc(CATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_C)[25:28] = c("Normal", "Charged", "Elemental", "Burst")
staty_pod_C <- staty_pod_C %>% gather("Rodzaj_ataku", "DMG", 25:30)

staty_pod_E <- Statystyki2 %>% arrange( desc(EATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_E)[25:28] = c("Normal", "Charged", "Elemental", "Burst")
staty_pod_E <- staty_pod_E %>% gather("Rodzaj_ataku", "DMG", 25:30)

staty_pod_Q <- Statystyki2 %>% arrange( desc(QATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_Q)[25:28] = c("Normal", "Charged", "Elemental", "Burst")
staty_pod_Q <- staty_pod_Q %>% gather("Rodzaj_ataku", "DMG", 25:30)

staty_pod_H <- Statystyki2 %>% arrange( desc(Healing) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_H)[25:28] = c("Normal", "Charged", "Elemental", "Burst")
staty_pod_H <- staty_pod_H %>% gather("Rodzaj_ataku", "DMG", 25:30)




flat_at = tibble( roll_AF = seq(0,30,1), roll_AP = seq(0,0,length.out=31), roll_CR = seq(0,0,length.out=31), roll_CD = seq(0,0,length.out=31),   )
proc_at = tibble( roll_AP = seq(0,30,1), roll_AF = seq(0,0,length.out=31), roll_CR = seq(0,0,length.out=31), roll_CD = seq(0,0,length.out=31),   )
proc_cr = tibble( roll_CR = seq(0,30,1), roll_AP = seq(0,0,length.out=31), roll_AF = seq(0,0,length.out=31), roll_CD = seq(0,0,length.out=31),   )
proc_cd = tibble( roll_CD = seq(0,30,1), roll_AP = seq(0,0,length.out=31), roll_CR = seq(0,0,length.out=31), roll_AF = seq(0,0,length.out=31),   )

prep_substat = bind_rows(flat_at, proc_at, proc_cr, proc_cd)
prep_substat <- prep_substat %>% mutate( HPF = 4780, AFP = 311) %>%
  mutate( Art_AP = roll_AP*4.955, Art_AF = roll_AF*16.535, Art_CR = roll_CR*3.305, Art_CD = roll_CD*6.605 ) %>%
  mutate( uchwyt = c("A_r") )

prep_Statystyki <- left_join( Sett, prep_substat, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Art_AP, Atk_flat = Atk_flat + Art_AF + AFP, HP_flat = HP_flat + HPF,
          CritR = CritR + Art_CR, CritD = CritD + Art_CD) %>%
  select( -HPF, -AFP, -Art_AP, -Art_AF, -Art_CR, -Art_CD)

prep_Statystyki2 <- left_join( prep_Statystyki, Weapon_name) %>% 
  mutate( HP = HP_base*(1+HP_proc/100) + HP_flat,
          Atk_flat = if_else( Wpn != 0, Atk_flat, Atk_flat + HP*1.2/100 ),
          ATK = Atk_base*(1+Atk_proc/100) + Atk_flat ) %>%
  mutate( NATK = ( ATK*if_else( Wpn != 2, 118.32/100, 138.32/100) )*(1 + (N_bonus + Phys)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          CATK = ( ATK*if_else( Wpn != 2, 320.28/100, 340.28/100) )*(1 + (CH_bonus + Phys)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          EATK = ( ATK*525.6/100)*(1 + (E_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          QATK = ( ATK*764.64/100)*(1 + (Q_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          Healing = ( ATK*452.16/100 + 3389)*(1 + Heal/100 ),
          Regen = ( ATK*45.22/100 + 339)*(1 + Heal/100 )
  )

prep_stat <- prep_Statystyki2 %>% select( roll_AF, roll_AP, roll_CR, roll_CD, WName, Wpn, NATK, CATK, EATK, QATK ) %>%
  arrange( NATK)
colnames(prep_stat)[1:4] = c( "Attack_flat", "Attack_proc", "Crit_Rate", "Crit_DMG")
colnames(prep_stat)[7:10] = c("Normal","Charged","Elemental","Burst")
prep_stat <- prep_stat %>% gather("Rodzaj_ataku", "DMG", 7:10) %>%
  gather( "Zwięszkana_statystyka", "Ilość_rolli", 1:4)

plot2 <- ggplot( ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Burst"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Charged"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_point( data = filter(avgStaty2, Rodzaj_ataku == "Healing"), aes( Art_type, DMG, color = Rodzaj_ataku) ) +
  geom_point( data = filter(avgStaty2, Rodzaj_ataku == "Regen"), aes( Art_type, DMG, color = Rodzaj_ataku) ) +
  ylab("Przeciętne obrażenia i leczenie") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  scale_fill_viridis_d( option = "C") +
  scale_color_brewer() +
  theme_dark() +
  guides(color = guide_legend(title = "Leczenie"))+
  coord_flip() + facet_grid(Name_Set~WName)


plot1 <-ggplot( ) +
  geom_col( data = filter(staty_pod_C, ASet == 1 & Rodzaj_ataku == "Burst"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_C, ASet == 1 & Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_C, ASet == 1 & Rodzaj_ataku == "Charged"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_C, ASet == 1 & Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_point( data = filter(staty_pod_C, ASet == 1 & Rodzaj_ataku == "Healing"), aes( Art_type, DMG, color = Rodzaj_ataku) ) +
  geom_point( data = filter(staty_pod_C, ASet == 1 & Rodzaj_ataku == "Regen"), aes( Art_type, DMG, color = Rodzaj_ataku) ) +
  geom_text( data = filter(staty_pod_C, ASet == 1 & Rodzaj_ataku == "Charged"), aes( Art_type, 75000, label = Art_sub_x), size = 3 ) +
  ylab("Przeciętne obrażenia i leczenie") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  scale_fill_brewer( palette = "Set1") +
  scale_color_brewer( palette = "Set3") +
  guides(color = guide_legend(title = "Leczenie"))+
  ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  coord_flip() + facet_wrap(~WName)

optstat <- Statystyki2  %>% filter( Wpn==1 ) %>% mutate( pmNATK = NATK/max(NATK),
                                   pmCATK = CATK/max(CATK),
                                   pmEATK = EATK/max(EATK),
                                   pmQATK = QATK/max(QATK),
                                  #pmAVG = ( pmNATK + pmQATK)/2  ) %>%
                                  pmAVG = ( pmNATK + pmCATK + pmEATK + pmQATK)/4  ) %>%
  arrange( desc(pmAVG) ) %>%
  distinct( Art_type, Wpn, ASet, .keep_all = TRUE)

ggplot( ) +
  geom_col( data = optstat, aes( Art_type, pmAVG, fill = pmAVG) ) +
  #ylab("Przeciętne obrażenia i leczenie") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  #scale_fill_viridis_d( option = "C") +
  #scale_color_brewer() +
  #theme_bw() +
  coord_flip() +
  facet_grid(Name_Set~WName)                                                                     
