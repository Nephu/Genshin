library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
source("emr_artifact.R")
source("Lostat.R")

Mona <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~ER, ~CritR, ~CritD, ~Wpn, ~ASet,
                  287, 0, 0, 10409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 5, 50, 0, 0,
                287, 0, 0, 10409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 5, 50, 0, 0,
                287, 0, 0, 10409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 5, 50, 0, 0,
                287, 0, 0, 10409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 5, 50, 0, 0,
                287, 0, 0, 10409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 5, 50, 0, 0,
                287, 0, 0, 10409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 5, 50, 0, 0,
                287, 0, 0, 10409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 5, 50, 0, 0,
                287, 0, 0, 10409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 5, 50, 0, 0,
                287, 0, 0, 10409, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 5, 50, 0, 0
)

Weapon <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~ER, ~CritR, ~CritD, ~Wpn, ~ASet,
                  674, 33.1, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  608, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 66.2, 1, 0,
                  608, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 33.1, 0, 2, 0,
                  510, 15, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 60, 0, 55.1, 3, 0,
                  #510, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 55.1, 3, 0,
                  #510, 0, 0, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 0, 55.1, 3, 0,
                  #510, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 0, 55.1, 3, 0,
                  510, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 20, 20, 0, 27.6, 0, 4, 0,
                  454, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 221, 0, 0, 5, 0,
                  565, 27.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0,
                  565, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 110, 0, 0, 7, 0,
                  510, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 55.1, 8, 0
)

Weapon_name <- tribble( ~Wpn, ~WName,
                        0, "Skyward_Atlas", 
                        1, "Kagura's_Verity^1stack",
                        2, "Lost_Prayer^1stack",
                        3, "The_Widsith_3x1/4", 
                        4, "Solar_Pearl",
                        5, "Sacrificial_F",
                        6, "Oathsworn_Eye",
                        7, "Mappa_Mare^2stacks",
                        8, "Blackcliff_Agate^3stacks")                

Sett <- Weapon + Mona
Sett <- Sett %>% mutate( uchwyt = c("A_r") )

####################

Mona <- tribble(~WName, ~ID, ~Refine, ~Atk_base, ~Atk_proc, ~HP_proc, ~ER,        
                  ~EM, ~CritR, ~CritD, ~ELbuff, ~Heal, ~N_bonus, ~CH_bonus, ~E_bonus,
                  ~Q_bonus, ~AP_plus, ~CR_plus, ~EM_plus, ~ER_plus, ~Eldmg_plus, ~Max_Stack,
                  ~HP_base, ~Phys, ~Def_base
)

single_Mona <- tribble(~WName, ~ID, ~Refine, ~Atk_base, ~Atk_proc, ~HP_proc, ~ER,        
                         ~EM, ~CritR, ~CritD, ~ELbuff, ~Heal, ~N_bonus, ~CH_bonus, ~E_bonus,
                         ~Q_bonus, ~AP_plus, ~CR_plus, ~EM_plus, ~ER_plus, ~Eldmg_plus, ~Max_Stack,
                         ~HP_base, ~Phys, ~Def_base,
                         "", 0, 0, 287, 0, 0, 0,
                         0, 5, 50, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 32, 0, 0,
                         10409, 0, 653
)

Catalyst <- read_csv2("./Weapon/Catalyst.csv") %>% 
  mutate(HP_base = 0, Phys = 0, Def_base = 0)
ile_razy <- count(Catalyst)


for ( i in 1:ile_razy$n-1) {
  Mona <- bind_rows(Mona, single_Mona)
}

Sett <- bind_cols( Catalyst[,1] , Catalyst[,-1] + Mona[,-1] )
Sett <- Sett %>% mutate( uchwyt = c("A_r") )

###################



Set_name <- tribble(~ASet, ~Name_Set,
                    #0, "Bez_setu",
                    1, "4 Emblem of SF",
                    2, "Heart of Depth + 2 Gladiator",
                    3, "Heart of Depth + 2 Noblesse")

ArtSet <- tribble(~Atk_AS, ~HP_AS, ~ELbuffAS, ~HealAS, ~PhysAS,
                  ~N_AS, ~CH_AS, ~E_AS, ~Q_AS, ~ERAS, ~CrAS, ~CdAS, ~ASetAS, ~uchwyt,
                  #0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "A_r",
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 1, "A_r",
                  18, 0, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, "A_r",
                  0, 0, 15, 0, 0, 0, 0, 0, 20, 0, 0, 0, 3, "A_r")

Sett2 <- left_join( Sett, ArtSet, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Atk_AS + AP_plus*Max_Stack, HP_proc = HP_proc + HP_AS,
          ELbuff = ELbuff + ELbuffAS + Eldmg_plus*Max_Stack, Heal = Heal + HealAS, Phys = Phys + PhysAS,
          N_bonus = N_bonus + N_AS, CH_bonus = CH_bonus + CH_AS, 
          E_bonus = E_bonus + E_AS, Q_bonus = Q_bonus + Q_AS, ER = ER + ERAS + ER_plus*Max_Stack,
          CritR = CritR + CrAS + CR_plus*Max_Stack, CritD = CritD + CdAS ) %>%
  select( -Atk_AS, -HP_AS, -ELbuffAS, -HealAS, -PhysAS, -N_AS, -CH_AS, -E_AS,
          -Q_AS, -ERAS, -CrAS, -CdAS, -AP_plus, -CR_plus, -EM_plus, -ER_plus, -Eldmg_plus, -Max_Stack )

Statystyki <- left_join( Sett2, Artifact_ER, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Art_AP, Atk_flat = Art_AF, HP_flat = HP_flat_F,
          ELbuff = ELbuff + El_dmg_G, Heal = Heal + Heal_C, Phys = Phys + Phys_G,
          CritR = CritR + Art_CR, CritD = CritD + Art_CD, ER = ER + Art_ER ) %>%
  select( -HP_flat_F, -Phys_G, -El_dmg_G, -Heal_C, -Art_AP, -Art_AF, -Art_CR, -Art_CD, -Art_ER) %>%
  mutate( ELbuff = ELbuff + ER*0.2 )

Statystyki2 <- left_join( Statystyki, Weapon_name) %>% left_join( Set_name) %>%
  mutate( HP = HP_base*(1+HP_proc/100) + HP_flat,
          ATK = Atk_base*(1+Atk_proc/100) + Atk_flat ) %>%
  mutate( NATK = ( ATK*78.5525/100 )*(1 + (N_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          CATK = ( ATK*269.5/100 )*(1 + (CH_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          EATK = ( ATK*239.04/100)*(1 + (E_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          QATK = ( ATK*796.32/100)*
            (1 + (Q_bonus + ELbuff + if_else(ASet == 1, pmin( ER/4,75), 0) )/100 )*
            (1 + ( pmin(CritR,100) * CritD)/10000 ),
  ) %>% filter( Phys == 0 & Heal == 0 )

avgStaty <- Statystyki2 %>% group_by( WName, Wpn, Name_Set,
                                      ASet, Art_type ) %>% summarise( Normal = mean(NATK),
                                                                      Charged = mean(CATK),
                                                                      Elemental = mean(EATK),
                                                                      Burst = mean(QATK) ) %>% ungroup()
avgStaty2 <- avgStaty %>% gather("Rodzaj_ataku", "DMG", 6:9)

staty_pod_N <- Statystyki2 %>% arrange( desc(NATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_N)[26:29] = c("Normal", "Charged", "Elemental", "Burst")
staty_pod_N <- staty_pod_N %>% gather("Rodzaj_ataku", "DMG", 26:29)

staty_pod_C <- Statystyki2 %>% arrange( desc(CATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_C)[26:29] = c("Normal", "Charged", "Elemental", "Burst")
staty_pod_C <- staty_pod_C %>% gather("Rodzaj_ataku", "DMG", 26:29)

staty_pod_E <- Statystyki2 %>% arrange( desc(EATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_E)[26:29] = c("Normal", "Charged", "Elemental", "Burst")
staty_pod_E <- staty_pod_E %>% gather("Rodzaj_ataku", "DMG", 26:29)

staty_pod_Q <- Statystyki2 %>% arrange( desc(QATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_Q)[26:29] = c("Normal", "Charged", "Elemental", "Burst")
staty_pod_Q <- staty_pod_Q %>% gather("Rodzaj_ataku", "DMG", 26:29)


plot2 <- ggplot( ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Burst"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Charged"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  scale_fill_viridis_d( option = "C") +
  scale_color_brewer() +
  #theme_dark() +
  coord_flip() + facet_grid(Name_Set~WName)


plot1 <-ggplot( ) +
  geom_col( data = filter(staty_pod_N, ASet == 1 & Rodzaj_ataku == "Burst"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_N, ASet == 1 & Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_N, ASet == 1 & Rodzaj_ataku == "Charged"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_N, ASet == 1 & Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_text( data = filter(staty_pod_N, ASet == 1 & Rodzaj_ataku == "Normal"), aes( Art_type, 275000, label = Art_sub_x), size = 3 ) +
  ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  ylim(0, 350000)+
  scale_fill_brewer( palette = "Set1") +
  scale_color_brewer( palette = "Set3") +
  coord_flip() + facet_wrap(~WName)


topmax <- Statystyki2 %>% select( WName, Name_Set, NATK, CATK, EATK, QATK)
colnames(topmax) <- c( "Weapon", "Artifact_Set", "Normal", "Charged", "Elemental", "Burst")

topmaxN <- topmax %>% arrange( desc(Normal)) %>%
  distinct(Weapon, Artifact_Set, .keep_all =  TRUE)

topmaxC <- topmax %>% arrange( desc(Charged)) %>%
  distinct(Weapon, Artifact_Set, .keep_all =  TRUE)

topmaxE <- topmax %>% arrange( desc(Elemental)) %>%
  distinct(Weapon, Artifact_Set, .keep_all =  TRUE)

topmaxQ <- topmax %>% arrange( desc(Burst)) %>%
  distinct(Weapon, Artifact_Set, .keep_all =  TRUE)


STT <- Statystyki2 %>% select( -1, -(4:13), -20, -(25:26) )
STT2 <- STT %>% 
  group_by( WName, Name_Set) %>%
  summarize( corAP = cor( Atk_proc, QATK ),
             corAF = cor( Atk_flat, QATK ), 
             corCR = cor( CritR, QATK ), 
             corCD = cor( CritD, QATK ), 
             corER = cor( ER, QATK ) ) %>%
  ungroup() %>%
  gather( "Typ", "Korelacja", 3:7 )

STT2

ggplot(STT2, aes(Typ, Korelacja)) +
  geom_point( )