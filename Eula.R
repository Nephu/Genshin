library(tidyverse)
library(ggplot2)
library(dplyr)
source("std_artifact.R")

Eula <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~CritR, ~CritD, ~Wpn, ~ASet,
                  342, 0, 0, 13226, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 88.4, 0, 0,
                  342, 0, 0, 13226, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 88.4, 0, 0,
                  342, 0, 0, 13226, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 88.4, 0, 0,
                  342, 0, 0, 13226, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 88.4, 0, 0,
                  342, 0, 0, 13226, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 88.4, 0, 0,
                  342, 0, 0, 13226, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 88.4, 0, 0
)


Weapon <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~CritR, ~CritD, ~Wpn, ~ASet,
                  608, 69.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  674, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 0, 0, 1, 0,
                  565, 0, 0, 0, 0, 0, 0, 34.5, 0, 0, 0, 0, 0, 0, 0, 2, 0,
                  510, 0, 0, 0, 0, 0, 0, 0, 0, 18, 18, 18, 18, 27.6, 0, 3, 0,
                  741, 36, 0, 0, 0, 0, 0, 20.7, 0, 0, 0, 0, 0, 0, 0, 4, 0,
                  510, 41.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 5, 0
)

Weapon_name <- tribble( ~Wpn, ~WName,
                        0, "Wolf's Gravestone offbuff ", 
                        1, "Skyward Pride",
                        2, "Snow-Tombed Starsilver",
                        3, "Serpent Spine^3 stacks",
                        4, "Song of Broken Pines active",
                        5, "Akuoumaru^1/2max"
                        )                

Sett <- Weapon + Eula
Sett <- Sett %>% mutate( uchwyt = c("A_r") )

Set_name <- tribble(~ASet, ~Name_Set,
                    #0, "Bez_setu",
                    1, "4 Pale Flame",
                    2, "2 Pale. + 2 Blood.")

ArtSet <- tribble(~Atk_AS, ~HP_AS, ~ELbuffAS, ~HealAS, ~PhysAS,
                  ~N_AS, ~CH_AS, ~E_AS, ~Q_AS, ~CrAS, ~CdAS, ~ASetAS, ~uchwyt,
                  #0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "A_r",
                  18, 0, 0, 0, 50, 0, 0, 0, 0, 0, 0, 1, "A_r",
                  0, 0, 0, 0, 50, 0, 0, 0, 0, 0, 0, 2, "A_r")

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
          ATK = Atk_base*(1+Atk_proc/100) + Atk_flat ) %>%
  mutate( NATK = ( ATK*183.232/100 )*(1 + (N_bonus + Phys)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          CATK = ( ATK*148.24/100 )*(1 + (Q_bonus + Phys)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ), #stacki burst
          EATK = ( ATK*442.08/100)*(1 + (E_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          QATK = ( ATK*725.56/100)*(1 + (Q_bonus + Phys)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 )
  )

avgStaty <- Statystyki2 %>% group_by( WName, Wpn, Name_Set,
                                      ASet, Art_type ) %>% summarise( Normal = mean(NATK),
                                                                      Q_Stack = mean(CATK),
                                                                      Elemental = mean(EATK),
                                                                      Lightfall = mean(QATK) ) %>% ungroup()
avgStaty2 <- avgStaty %>% gather("Rodzaj_ataku", "DMG", 6:9)

staty_pod_N <- Statystyki2 %>% arrange( desc(NATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_N)[25:28] = c("Normal", "Q_Stack", "Elemental", "Lightfall")
staty_pod_N <- staty_pod_N %>% gather("Rodzaj_ataku", "DMG", 25:28)

staty_pod_C <- Statystyki2 %>% arrange( desc(CATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_C)[25:28] = c("Normal", "Q_Stack", "Elemental", "Lightfall")
staty_pod_C <- staty_pod_C %>% gather("Rodzaj_ataku", "DMG", 25:28)

staty_pod_E <- Statystyki2 %>% arrange( desc(EATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_E)[25:28] = c("Normal", "Q_Stack", "Elemental", "Lightfall")
staty_pod_E <- staty_pod_E %>% gather("Rodzaj_ataku", "DMG", 25:28)

staty_pod_Q <- Statystyki2 %>% arrange( desc(QATK) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
colnames(staty_pod_Q)[25:28] = c("Normal", "Q_Stack", "Elemental", "Lightfall")
staty_pod_Q <- staty_pod_Q %>% gather("Rodzaj_ataku", "DMG", 25:28)


plot2 <- ggplot( ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Lightfall"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Q_Stack"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  scale_fill_viridis_d( option = "C") +
  scale_color_brewer() +
  theme_dark() +
  coord_flip() + facet_grid(Name_Set~WName)


plot1 <-ggplot( ) +
  geom_col( data = filter(staty_pod_Q, ASet == 1 & Rodzaj_ataku == "Lightfall"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_Q, ASet == 1 & Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_Q, ASet == 1 & Rodzaj_ataku == "Q_Stack"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_Q, ASet == 1 & Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_text( data = filter(staty_pod_Q, ASet == 1 & Rodzaj_ataku == "Lightfall"), aes( Art_type, 150000, label = Art_sub_x), size = 3 ) +
  ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  scale_fill_brewer( palette = "Set1") +
  scale_color_brewer( palette = "Set3") +
  coord_flip() + facet_wrap(~WName)

optstatfull <- Statystyki2 %>% mutate( pmNATK = NATK/max(NATK),
                                       pmCATK = CATK/max(CATK),
                                       pmEATK = EATK/max(EATK),
                                       pmQATK = QATK/max(QATK),
                                       #pmAVG = ( pmNATK + pmQATK)/2  ) %>%
                                       pmAVG = ( pmNATK + pmCATK + pmEATK + pmQATK)*25  ) %>%
  arrange( desc(pmAVG) ) %>%
  distinct( Wpn, ASet, .keep_all = TRUE)

plotopf<-ggplot( ) +
  geom_col( data = optstatfull, aes( Art_type, pmAVG, fill = pmAVG) ) +
  ylab("Średni stosunek obrażeń ") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  guides( fill = guide_legend( title = "", nrow = 1 ))+
  theme(legend.position = "bottom")+
  coord_flip() +
  facet_grid(Name_Set~WName)                                                                     

topmax <- Statystyki2 %>% select( WName, Name_Set, NATK, CATK, EATK, QATK)
colnames(topmax)[3:6] <- c("Normal", "Q_Stack", "Elemental", "Lightfall")

topmaxN <- topmax %>% arrange( desc(Normal)) %>%
  distinct(WName, Name_Set, .keep_all =  TRUE)

topmaxC <- topmax %>% arrange( desc(Q_Stack)) %>%
  distinct(WName, Name_Set, .keep_all =  TRUE)

topmaxE <- topmax %>% arrange( desc(Elemental)) %>%
  distinct(WName, Name_Set, .keep_all =  TRUE)

topmaxQ <- topmax %>% arrange( desc(Lightfall)) %>%
  distinct(WName, Name_Set, .keep_all =  TRUE)


optmaxstat <- Statystyki2 %>% group_by(Wpn) %>% summarise( maxNATK = max(NATK),
                                                           maxCATK = max(CATK),
                                                           maxEATK = max(EATK),
                                                           maxQATK = max(QATK) ) %>% ungroup()


optstat <- left_join( Statystyki2, optmaxstat ) %>%
  select( WName, Name_Set, Art_type, NATK, CATK, EATK, QATK, maxNATK, maxCATK, maxEATK, maxQATK) %>%
  mutate( pmNATK = NATK/maxNATK,
          pmCATK = CATK/maxCATK,
          pmEATK = EATK/maxEATK,
          pmQATK = QATK/maxQATK,
          pmAVG = ( pmNATK + pmCATK + pmEATK + pmQATK)/4) %>%
  arrange( desc(pmAVG) ) %>%
  select( -(8:15) ) %>%
  distinct( WName, Name_Set, .keep_all = TRUE)

plot_opp <- ggplot( ) +
  geom_col( data = optstat, aes( Art_type, pmAVG, fill = pmAVG) ) +
  coord_flip() +
  facet_grid(Name_Set~WName) 

