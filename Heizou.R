library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
source("emr_artifact.R")

Heizou <- tribble(~WName, ~ID, ~Refine, ~Atk_base, ~Atk_proc, ~HP_proc, ~ER,        
                  ~EM, ~CritR, ~CritD, ~ELbuff, ~Heal, ~N_bonus, ~CH_bonus, ~E_bonus,
                  ~Q_bonus, ~AP_plus, ~CR_plus, ~EM_plus, ~ER_plus, ~Eldmg_plus, ~Max_Stack,
                  ~HP_base, ~Phys, ~Def_base
)

single_Heizou <- tribble(~WName, ~ID, ~Refine, ~Atk_base, ~Atk_proc, ~HP_proc, ~ER,        
                         ~EM, ~CritR, ~CritD, ~ELbuff, ~Heal, ~N_bonus, ~CH_bonus, ~E_bonus,
                         ~Q_bonus, ~AP_plus, ~CR_plus, ~EM_plus, ~ER_plus, ~Eldmg_plus, ~Max_Stack,
                         ~HP_base, ~Phys, ~Def_base,
                         "", 0, 0, 225, 0, 0, 0,
                         0, 5, 50, 24, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0,
                         10657, 0, 684
)

Catalyst <- read_csv2("./Weapon/Catalyst.csv") %>% 
  mutate(HP_base = 0, Phys = 0, Def_base = 0)
ile_razy <- count(Catalyst)


for ( i in 1:ile_razy$n-1) {
  Heizou <-bind_rows(Heizou, single_Heizou)
}
              
Sett <- bind_cols( Catalyst[,1] , Catalyst[,-1] + Heizou[,-1] )
Sett <- Sett %>% mutate( uchwyt = c("A_r") )

ArtSet24 <- read_csv2("./Weapon/ArtSet.csv") %>% mutate( uchwyt = c("A_r") )

ArtSet24 <- ArtSet24 %>% filter( ! ID %in% c(2,7,8,9,10,13:29,32:34,36:41,44)  )
ArtSet2 <- ArtSet24 %>% filter( Piece == 2 )
ArtSet2 <- inner_join( ArtSet2, ArtSet2, by = "uchwyt", suffix = c("", "_R")) %>%
  filter( !(ID == ID_R) ) %>%
  mutate( Atk_AS = Atk_AS + Atk_AS_R, HP_AS = HP_AS + HP_AS_R, Def_AS = Def_AS + Def_AS_R,
          ERAS = ERAS + ERAS_R, EMAS = EMAS + EMAS_R, CrAS = CrAS + CrAS, 
          PhysAS = PhysAS + PhysAS_R, HealAS = HealAS + HealAS_R,
          N_AS = N_AS + N_AS_R, CH_AS = CH_AS + CH_AS_R, Q_AS = Q_AS + Q_AS_R,
          Nobles = Nobles + Nobles_R, Blood = Blood + Blood_R,
          Crimson = Crimson + Crimson_R, Tenacity = Tenacity + Tenacity_R,
          PaleFlame = PaleFlame + PaleFlame_R, PFLame = PFLame + PFLame_R,
          Shime = Shime + Shime_R, Husk = Husk + Husk_R,
          Vermillion = Vermillion + Vermillion_R, HereAfter = HereAfter + HereAfter_R,
          Echoes = Echoes + Echoes_R, Max_Stack = Max_Stack + Max_Stack_R,
          Electro = Electro + Electro_R, Cryo = Cryo + Cryo_R,
          Hydro = Hydro + Hydro_R, Pyro = Pyro + Pyro_R,
          Anemo = Anemo + Anemo_R, Geo = Geo + Geo_R, Dendro = Dendro + Dendro_R)

ArtSet2 <- ArtSet2 %>% select( -(38:69) ) %>% 
  mutate( Name_Set = paste(Name_Set, Name_Set_R, sep = " + ") ) %>% select(-Name_Set_R) %>%
  mutate( SID = ifelse(ID < ID_R, paste(ID, ID_R, sep = ""), paste(ID_R, ID, sep = "") ) ) %>%
  distinct( SID, .keep_all = TRUE) %>%
  mutate( ID = as.numeric(SID) + 100) %>%
  select( -ID_R, -SID)

ArtSet <- ArtSet24 %>% filter( Piece == 4) %>% bind_rows(ArtSet2)

ArtSet <- ArtSet %>% select( -Electro, -Cryo, -Hydro, -Pyro, -Geo, -Dendro) %>%
  mutate( Max_Stack = if_else( ID == 43, 0, Max_Stack ) ) %>%
  filter( ID %in% c(6,12,43,211,411) )

SettF <- Sett %>% filter( Refine == 1, !ID %in% (c(0,5,10,20,25,30,40,45,50,60,65,70,80,85,105,110,120)+0 )) %>%
  mutate( Max_Stack = if_else( ID == 90, 2, Max_Stack ) )

Sett2 <- left_join( SettF, ArtSet, "uchwyt", suffix = c("", "AS")) %>%
  mutate( Atk_proc = Atk_proc + Atk_AS, HP_proc = HP_proc + HP_AS, Def = Def_AS,
          ELbuff = ELbuff + Anemo, Heal = Heal + HealAS, Phys = Phys + PhysAS,
          N_bonus = N_bonus + N_AS, CH_bonus = CH_bonus + CH_AS, 
          Q_bonus = Q_bonus + Q_AS, EM = EM + EMAS, ER = ER + ERAS,
          CritR = CritR + CrAS) %>%
  select(-(30:41))
  

rm(Artifact_ER)

Artifact_EM2 <- Artifact_EM %>% filter( Phys_G == 0 & Heal_C == 0 ) %>%
  filter( !(Art_type %in% c("Atk Atk E", "Atk E CD", "Atk E CR", "Atk E E",
                            "Atk EL E", "E E CD", "E E CR", "E EL CD", "E EL CR", "E EL E") ) )

#Artifact_EM2 %>% count( Art_type, sort = TRUE )
  

Statystyki <- left_join( Sett2, Artifact_EM2, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Art_AP + AP_plus*Max_Stack, Atk_flat = Art_AF, HP_flat = HP_flat_F,
          ELbuff = ELbuff + El_dmg_G  + Eldmg_plus*Max_Stack,
          Heal = Heal + Heal_C, Phys = Phys + Phys_G,
          CritR = CritR + Art_CR + CR_plus*Max_Stack, CritD = CritD + Art_CD,
          EM = EM + Art_EM + EM_plus*Max_Stack, ER = ER + ER_plus*Max_Stack ) %>%
  select( -HP_flat_F, -Phys_G, -El_dmg_G, -Heal_C, -Art_AP, -Art_AF, -Art_CR, -Art_CD, -Art_EM)

CRC6 <- 16
CDC6 <- 32
CRC0 <- 0
CDC0 <- 0

Statystyki2 <- Statystyki %>%
  mutate( HP = HP_base*(1+HP_proc/100) + HP_flat,
          ATK = Atk_base*(1+(Atk_proc + Nobles + Tenacity + Vermillion + HereAfter*Max_StackAS)/100) + Atk_flat ) %>%
  mutate( NATK = ( ATK*85.354/100 + ATK*0.7*Echoes/100 )*(1 + (N_bonus + ELbuff + Shime)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          CATK = ( ATK*131.4/100 )*(1 + (CH_bonus + ELbuff + Shime)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          EATK = ( (Atk_base*(1+(Atk_proc + Nobles + Vermillion + HereAfter*Max_Stack)/100) + Atk_flat)*409.54/100)*(1 + (E_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
          HSC = ( (Atk_base*(1+(Atk_proc + Nobles + Vermillion + HereAfter*Max_Stack)/100) + Atk_flat)*1023.83/100 )*(1 + (E_bonus + ELbuff)/100 )*(1 + ( pmin(CritR+CRC0,100) * (CritD+CDC0) )/10000 ),
          QATK = ( (Atk_base*(1+(Atk_proc + Tenacity)/100) + Atk_flat)*566.44/100)*(1 + (Q_bonus + ELbuff)/100 )*(1 + ( pmin(CritR,100) * CritD)/10000 ),
  )

boxStaty <- Statystyki2 %>%
  gather("Rodzaj_ataku", "DMG", c(NATK,HSC,QATK))

plotbox <- ggplot(boxStaty, aes( Name_Set, DMG, fill = WName)) +
  geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~Rodzaj_ataku)+
  coord_flip()


avgStaty <- Statystyki2 %>% 
  group_by( WName, Name_Set, Art_type ) %>%
  summarise( Normal = mean(NATK), Charged = mean(CATK),
             Elemental = mean(EATK), Conviction_Strike = mean(HSC),
             Burst = mean(QATK) ) %>% ungroup()
avgStaty2 <- avgStaty %>% gather("Rodzaj_ataku", "DMG", 4:8)

avgWeapon <- Statystyki2 %>% 
  group_by( WName ) %>%
  summarise( Normal = mean(NATK), Charged = mean(CATK),
             Elemental = mean(EATK), Conviction_Strike = mean(HSC),
             Burst = mean(QATK) ) %>% ungroup() %>%
  arrange(desc(Conviction_Strike))

avgSet <- Statystyki2 %>% 
  group_by( Name_Set ) %>%
  summarise( Normal = mean(NATK), Charged = mean(CATK),
             Elemental = mean(EATK), Conviction_Strike = mean(HSC),
             Burst = mean(QATK) ) %>% ungroup() %>%
  arrange(desc(Conviction_Strike))

avgArt <- Statystyki2 %>% 
  group_by( Art_type ) %>%
  summarise( Normal = mean(NATK), Charged = mean(CATK),
             Elemental = mean(EATK), Conviction_Strike = mean(HSC),
             Burst = mean(QATK) ) %>% ungroup() %>%
  arrange(desc(Conviction_Strike))

staty_pod_HSC <- Statystyki2 %>% arrange( desc(HSC) ) %>% distinct( Art_type, ID, IDAS, .keep_all = TRUE )
colnames(staty_pod_HSC)[50:54] = c("Normal", "Charged", "Elemental","Conviction_Strike", "Burst")
staty_pod_HSC <- staty_pod_HSC %>% gather("Rodzaj_ataku", "DMG", 50:54)

plot2 <- ggplot( ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Conviction_Strike"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Burst"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Charged"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(avgStaty2, Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  scale_fill_viridis_d( option = "C") +
  scale_color_brewer() +
  #theme_dark() +
  coord_flip() + facet_grid(WName~~Name_Set)


plot1 <-ggplot( ) +
  geom_col( data = filter(staty_pod_HSC, IDAS == 43 & Rodzaj_ataku == "Conviction_Strike"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_HSC, IDAS == 43 & Rodzaj_ataku == "Burst"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_HSC, IDAS == 43 & Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_HSC, IDAS == 43 & Rodzaj_ataku == "Charged"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_col( data = filter(staty_pod_HSC, IDAS == 43 & Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
  geom_text( data = filter(staty_pod_HSC, IDAS == 43 & Rodzaj_ataku == "Normal"), aes( Art_type, 80000, label = Art_sub_x), size = 3 ) +
  ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  ylim(0, 130000)+
  scale_fill_brewer( palette = "Set1") +
  scale_color_brewer( palette = "Set3") +
  coord_flip()+
  facet_wrap(~WName)

ggplotly(plot1)


topmax <- Statystyki2 %>% select( WName, Name_Set, NATK, CATK, EATK, HSC, QATK)
colnames(topmax) <- c( "Weapon", "Artifact_Set", "Normal", "Charged",
                       "Elemental", "Conviction_Strike", "Burst")

topmaxHSC <- topmax %>% arrange( desc(Conviction_Strike)) %>%
  distinct(Weapon, Artifact_Set, .keep_all =  TRUE)

opttopHSC <- topmaxHSC %>% mutate( Normal = round( Normal/max(Normal), digits = 3) ,
                               Charged = round( Charged/max(Charged), digits = 3),
                               Elemental = round( Elemental/max(Elemental), digits = 3),
                               Conviction_Strike = round( Conviction_Strike/max(Conviction_Strike), digits = 3),
                               Burst =  round( Burst/max(Burst), digits = 3),
                               Suma = (Normal + Charged)/2 + (Elemental + Conviction_Strike)/2 + Burst) %>%
  arrange( desc(Suma) )

meantophsc <- opttopHSC %>% group_by(Weapon) %>%
  summarise( przec_punkty = mean(Suma) ) %>%
  ungroup() %>%
  arrange( desc(przec_punkty))

meantophsc2 <- opttopHSC %>% group_by(Artifact_Set) %>%
  summarise( przec_punkty = mean(Suma) ) %>%
  ungroup() %>%
  arrange( desc(przec_punkty))


optmaxstat <- Statystyki2 %>% group_by(WName) %>% summarise( maxNATK = max(NATK),
                                                           maxCATK = max(CATK),
                                                           maxEATK = max(EATK),
                                                           maxHSC = max(HSC),
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

Heizou_avg <- avgStaty2
Heizou_HSC <- staty_pod_HSC

