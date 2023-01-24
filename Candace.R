library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
source("ehd_artifact.R")
#source("emr_artifact.R")


Candace <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~CritR, ~CritD, ~ER, ~Wpn, ~ASet,
                212, 0, 0, 10875, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 50, 100, 0, 0,
                212, 0, 0, 10875, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 50, 100, 0, 0,
                212, 0, 0, 10875, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 50, 100, 0, 0,
                212, 0, 0, 10875, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 50, 100, 0, 0,
                212, 0, 0, 10875, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 50, 100, 0, 0,
                212, 0, 0, 10875, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 50, 100, 0, 0
)


Weapon <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~ELbuff, ~Phys, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~CritR, ~CritD, ~ER, ~Wpn, ~ASet,
                  354, 0, 0, 0, 46.9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  509, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 55.1, 0, 1, 0,
                  454, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36.7, 0, 0, 2, 0,
                  564, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30.6, 3, 0,
                  509, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 0, 0, 45.9, 4, 0,
                  608, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 66.1, 0, 5, 0
)

Weapon_name <- tribble( ~Wpn, ~WName,
                        0, "Black_Tassel", 
                        1, "Blackcliff_Pole",
                        2, "Deathmatch",
                        3, "Favonius_Lance",
                        4, "The_Catch_R5",
                        5, "Staff_of_Homa_R1_on"
)                

Sett <- Weapon + Candace
Sett <- Sett %>% mutate( uchwyt = c("A_r") )


Set_name <- tribble(~ASet, ~Name_Set,
                    0, "4 Noblesse Oblige",
                    1, "4 Severed Fate",
                    2, "2 Tenacity + 2 HoD")

ArtSet <- tribble(~Atk_AS, ~HP_AS, ~ELbuffAS, ~HealAS, ~PhysAS,
                  ~N_AS, ~CH_AS, ~E_AS, ~Q_AS, ~CrAS, ~CdAS, ~ERAS, ~ASetAS, ~uchwyt,
                  0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, "A_r",
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 1, "A_r",
                  0, 20, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, "A_r")

Sett2 <- left_join( Sett, ArtSet, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Atk_AS, HP_proc = HP_proc + HP_AS,
          ELbuff = ELbuff + ELbuffAS, Heal = Heal + HealAS, Phys = Phys + PhysAS,
          N_bonus = N_bonus + N_AS, CH_bonus = CH_bonus + CH_AS, 
          E_bonus = E_bonus + E_AS, Q_bonus = Q_bonus + Q_AS,
          CritR = CritR + CrAS, CritD = CritD + CdAS,
          ER = ER + ERAS, ASet = ASet + ASetAS) %>%
  select(1:19,)

Statystyki <- left_join( Sett2, Artifact_H, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Art_AP, Atk_flat = Atk_flat + Art_AF,
          HP_flat = HP_flat + Art_HF, HP_proc = HP_proc + Art_HP, ER = ER + Art_ER,
          ELbuff = ELbuff + El_dmg_G, Heal = Heal + Heal_C, Phys = Phys + Phys_G,
          CritR = CritR + Art_CR, CritD = CritD + Art_CD) %>%
  select( -HP_flat_F, -Phys_G, -El_dmg_G, -Heal_C, -Art_AP, -Art_AF, -Art_CR, -Art_CD,
          -Art_HP, -Art_HF, -Art_ER) %>% filter( Phys == 0 , Heal == 0 )

Statystyki2 <- left_join( Statystyki, Weapon_name) %>% left_join( Set_name) %>%
  mutate( HP = HP_base*(1+HP_proc/100) + HP_flat,
          Atk_flat = if_else( Wpn == 5, Atk_flat + 1.8*HP/100, Atk_flat ),
          ATK = Atk_base*(1+Atk_proc/100) + Atk_flat,
          ATKNO = Atk_base*(1+(Atk_proc+20)/100) + Atk_flat, 
          EoSF = pmin(75, 0.25*ER )  ) %>%
  mutate( NATK = ( if_else( ASet == 0, ATKNO, ATK)*146.1575/100 )*(1 + (N_bonus + Phys)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          CATK = ( if_else( ASet == 0, ATKNO, ATK)*228.15/100 )*(1 + (CH_bonus + Phys)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ), 
          EATK = ( HP*34.27/100)*(1 + (E_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          QATK = ( HP*11.9*4/100)*
            (1 + (Q_bonus + ELbuff + if_else( ASet == 1, EoSF,0))/100 )*
            (1 + ( pmin( if_else( Wpn == 4, CritR+12, CritR),100) * CritD)/10000 )
  ) %>% select( -ATKNO, -EoSF )

avgStaty <- Statystyki2 %>% group_by( WName, Wpn, Name_Set,
                                      ASet, Art_type ) %>% summarise( Normal = mean(NATK),
                                                                      Charged = mean(CATK),
                                                                      Elemental = mean(EATK),
                                                                      Burst = mean(QATK) ) %>% ungroup()
avgStaty2 <- avgStaty %>% gather("Rodzaj_ataku", "DMG", 6:9)

podstaty <- Statystyki2 %>% rename( "Normal" = "NATK" , "Charged" = "CATK",
                                    "Elemental" = "EATK", "Burst" = "QATK"  )

staty_pod_N <- podstaty %>% arrange( desc(Normal) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
staty_pod_N <- staty_pod_N %>% gather("Rodzaj_ataku", "DMG", Normal, Charged, Elemental, Burst)

staty_pod_C <- podstaty %>% arrange( desc(Charged) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
staty_pod_C <- staty_pod_C %>% gather("Rodzaj_ataku", "DMG", Normal, Charged, Elemental, Burst)

staty_pod_E <- podstaty %>% arrange( desc(Elemental) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
staty_pod_E <- staty_pod_E %>% gather("Rodzaj_ataku", "DMG", Normal, Charged, Elemental, Burst)

staty_pod_Q <- podstaty %>% arrange( desc(Burst) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
staty_pod_Q <- staty_pod_Q %>% gather("Rodzaj_ataku", "DMG", Normal, Charged, Elemental, Burst)

staty_pod_HE <- podstaty %>% arrange( desc(HP), desc(Elemental) ) %>% distinct( Art_type, Wpn, ASet, .keep_all = TRUE )
staty_pod_HE <- staty_pod_HE %>% gather("Rodzaj_ataku", "DMG", Normal, Charged, Elemental, Burst)

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


boxStaty <- Statystyki2 %>%
  gather("Rodzaj_ataku", "DMG", c(NATK,CATK,EATK,QATK))

plotbox <- ggplot(boxStaty, aes( Name_Set, DMG, fill = WName)) +
  geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~Rodzaj_ataku)+
  coord_flip()


avgStaty3 <- Statystyki2 %>% 
  group_by( WName, Name_Set, Art_type ) %>%
  summarise( Normal = mean(NATK), Charged = mean(CATK),
             Elemental = mean(EATK),
             Burst = mean(QATK) ) %>% ungroup() %>% arrange(desc(Burst))
#avgStaty4 <- avgStaty %>% gather("Rodzaj_ataku", "DMG", 4:7)

avgWeapon <- Statystyki2 %>% 
  group_by( WName ) %>%
  summarise( Normal = mean(NATK), Charged = mean(CATK),
             Elemental = mean(EATK), 
             Burst = mean(QATK) ) %>% ungroup() %>%
  arrange(desc(Burst))

avgSet <- Statystyki2 %>% 
  group_by( Name_Set ) %>%
  summarise( Normal = mean(NATK), Charged = mean(CATK),
             Elemental = mean(EATK), 
             Burst = mean(QATK) ) %>% ungroup() %>%
  arrange(desc(Burst))

avgArt <- Statystyki2 %>% 
  group_by( Art_type ) %>%
  summarise( Normal = mean(NATK), Charged = mean(CATK),
             Elemental = mean(EATK), 
             Burst = mean(QATK) ) %>% ungroup() %>%
  arrange(desc(Burst))


plot1 <-ggplot( ) +
  geom_col( data = filter(staty_pod_HE, ASet == 2 & Rodzaj_ataku == "Burst"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_HE, ASet == 2 & Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_HE, ASet == 2 & Rodzaj_ataku == "Charged"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_HE, ASet == 2 & Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_text( data = filter(staty_pod_HE, ASet == 2 & Rodzaj_ataku == "Elemental"), aes( Art_type, 60000, label = Art_sub_x), size = 3 ) +
  ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  ylim(0, 80000)+
  scale_fill_brewer( palette = "Set1") +
  #scale_color_brewer( palette = "Set3") +
  geom_point( data = filter(staty_pod_HE, ASet == 2 & Rodzaj_ataku == "Elemental"), aes( Art_type, HP), size = 3 ) +
  coord_flip()+
  facet_wrap(~WName)

ggplotly(plot1)

STT <- Statystyki2 %>% select( WName, Name_Set, Atk_proc, Atk_flat,
                               HP_proc, HP_flat, CritR, CritD, ER, QATK )

STT2 <- STT %>% 
  group_by( WName, Name_Set) %>%
  summarize( corAP = cor( Atk_proc, QATK ),
             corAF = cor( Atk_flat, QATK ),
             corHP = cor( HP_proc, QATK ),
             corHF = cor( HP_flat, QATK ),
             corCR = cor( CritR, QATK ), 
             corCD = cor( CritD, QATK ), 
             corER = cor( ER, QATK ) ) %>%
  ungroup() %>%
  gather( "Typ", "Korelacja", 3:9 )

STT2

ggplot(STT2, aes(Typ, Korelacja)) +
  geom_point( ) +
  facet_wrap( ~WName )

topmax <- Statystyki2 %>% select( WName, Name_Set, HP, NATK, CATK, EATK, QATK)
colnames(topmax)[4:7] <- c("Normal", "Charged", "Elemental", "Burst")

topmaxN <- topmax %>% arrange( desc(Normal)) %>%
  distinct(WName, Name_Set, .keep_all =  TRUE)

topmaxC <- topmax %>% arrange( desc(Charged)) %>%
  distinct(WName, Name_Set, .keep_all =  TRUE)

topmaxE <- topmax %>% arrange( desc(Elemental)) %>%
  distinct(WName, Name_Set, .keep_all =  TRUE)

topmaxQ <- topmax %>% arrange( desc(Burst)) %>%
  distinct(WName, Name_Set, .keep_all =  TRUE)

topmaxHP <- topmax %>% arrange( desc(HP)) %>%
  distinct(WName, Name_Set, .keep_all =  TRUE)

Candace_avg <- avgStaty2
Candace_E <- staty_pod_E
Candace_H <- staty_pod_HE

