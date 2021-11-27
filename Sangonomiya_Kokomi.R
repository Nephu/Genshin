library(tidyverse)
library(ggplot2)
library(dplyr)

Kokomi <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~HDR, ~Heal,
                  ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~Wpn, ~ASet,
                  234, 0, 0, 13471, 0, 0, 28.8, 25, 0, 0, 0, 0, 0, 0,
                  234, 0, 0, 13471, 0, 0, 28.8, 25, 0, 0, 0, 0, 0, 0,
                  234, 0, 0, 13471, 0, 0, 28.8, 25, 0, 0, 0, 0, 0, 0,
                  234, 0, 0, 13471, 0, 0, 28.8, 25, 0, 0, 0, 0, 0, 0,
                  234, 0, 0, 13471, 0, 0, 28.8, 25, 0, 0, 0, 0, 0, 0,
                  234, 0, 0, 13471, 0, 0, 28.8, 25, 0, 0, 0, 0, 0, 0,
                  234, 0, 0, 13471, 0, 0, 28.8, 25, 0, 0, 0, 0, 0, 0)


Weapon <- tribble(~Atk_base, ~Atk_proc, ~Atk_flat, ~HP_base, ~HP_proc, ~HP_flat, ~HDR, ~Heal,
                ~N_bonus, ~CH_bonus, ~E_bonus, ~Q_bonus, ~Wpn, ~ASet,
                401, 0, 0, 0, 36.2, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                510, 0, 0, 0, 41.3, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                565, 27.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0,
                454, 56.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0,
                608, 0, 0, 0, 49.6, 0, 0, 10, 0, 0, 0, 0, 4, 0,
                674, 33.1, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 5, 0,
                454, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0)

Weapon_name <- tribble( ~Wpn, ~WName,
                        0, "TTDS",
                        1, "Prot_Amber",
                        2, "Royal_Grimoire",
                        3, "Eye_of_Perception",
                        4, "Everlasting_Moonglow",
                        5, "Skyward_Atlas",
                        6, "Sacrificial")                

Sett <- Weapon + Kokomi

Flower <- tribble(~HP_flat_F, ~uchwyt, 4780, "A_r")

Plume <- tribble(~Atk_flat_P, ~uchwyt, 311, "A_r")

Sands <- tribble(~Atk_proc_S, ~HP_proc_S, ~S_typ, ~Waga_S, ~uchwyt,
                 46.6, 0, "Atk", 1, "A_r",
                 0, 46.6, "HP" , 0, "A_r")

Goblet <- tribble(~Atk_proc_G, ~HP_proc_G, ~El_dmg_G, ~G_typ, ~Waga_G, ~uchwyt,
                  46.6, 0, 0, "Atk", 1, "A_r",
                  0, 46.6, 0, "HP", 0, "A_r",
                  0, 0, 46.6, "EL", 4, "A_r")


Circlet <- tribble(~Atk_proc_G, ~HP_proc_G, ~Heal_G, ~C_typ, ~Waga_C, ~uchwyt,
                   46.6, 0, 0, "Atk", 1, "A_r",
                   0, 46.6, 0, "HP", 0, "A_r",
                   0, 0, 35.9, "Heal", 13, "A_r" )

avghpf <- tribble(~sub_pf, ~wpf, ~ile, ~uchwyt,
                  "hpf", 0, 0, "A_r",
                  "hpf", 254, 1, "A_r",
                  "hpf", 1524, 6, "A_r")

avghpp <- tribble(~sub_pp, ~wpp, ~ile, ~uchwyt,
                  "hpp", 0, 0, "A_r",
                  "hpp", 4.925, 1, "A_r",
                  "hpp", 29.55, 6, "A_r")

avgatkf <- tribble(~sub_af, ~waf, ~ile, ~uchwyt,
                   "af", 0, 0, "A_r",
                   "af", 16.75, 1, "A_r",
                   "af", 100.5, 6, "A_r")

avgatkp <- tribble(~sub_ap, ~wap, ~ile, ~uchwyt,
                   "ap", 0, 0, "A_r",
                   "ap", 4.925, 1, "A_r",
                   "ap", 29.55, 6, "A_r")

avg_sub_stat <- left_join( avghpp, avghpf, "uchwyt") %>%
  left_join( avgatkf, "uchwyt") %>%
  left_join( avgatkp, "uchwyt") %>%
  mutate( ile_razem = ile.x + ile.y + ile.x.x + ile.y.y) %>%
  filter( ile_razem <= 9) %>%
  mutate( Art_sub_type = paste( if_else(ile.x <= 1, "", sub_pp ),
                                if_else(ile.y <= 1, "", sub_pf ),
                                if_else(ile.x.x <= 1, "", sub_af ),
                                if_else(ile.y.y <= 1, "", sub_ap ) ))

avg_sub_stat1 <- avg_sub_stat %>%
  select( -ile.x, -ile.y, -ile.x.x, -ile.y.y, -ile_razem) %>%
  mutate( Art_sub_type = trimws(Art_sub_type) )

FFlower <- left_join(Flower, avg_sub_stat1, "uchwyt") %>% 
  filter( !wpp == 0) %>% filter( !wpf == 0) %>% filter( !waf == 0) %>% filter( !wap == 0 ) %>%
  filter( !Art_sub_type == "") %>%
  select( -sub_pp, -sub_pf, -sub_af, -sub_ap)

PPlume <- left_join(Plume, avg_sub_stat1, "uchwyt") %>% 
  filter( !wpp == 0) %>% filter( !wpf == 0) %>% filter( !waf == 0) %>% filter( !wap == 0 ) %>%
  filter( !Art_sub_type == "") %>%
  select( -sub_pp, -sub_pf, -sub_af, -sub_ap)

Art_FP <- left_join(FFlower, PPlume, "uchwyt")

SSands <- left_join(Sands, avg_sub_stat1, "uchwyt") %>%
  mutate( wgpp = if_else( Waga_S == 0 & wpp == 0, "ok", "x" ) ) %>%
  mutate( wgap = if_else( Waga_S == 1 & wap == 0, "ok", "x" ) ) %>%
  mutate( wgsum = paste( wgpp, wgap, sep = "")) %>%
  filter( !( wgsum == "xx")) %>%
  select( -wgpp, -wgap, -wgsum) %>%
  filter( !wpf == 0) %>% filter( !waf == 0) %>%
  filter( !Art_sub_type == "") %>%
  select( -sub_pp, -sub_pf, -sub_af, -sub_ap)

ARt_SFP <- left_join( Art_FP, SSands, "uchwyt")

GGoblet <- left_join(Goblet, avg_sub_stat1, "uchwyt") %>%
  mutate( wgpp = if_else( Waga_G == 0 & wpp == 0, "ok", "x" ) ) %>%
  mutate( wgap = if_else( Waga_G == 1 & wap == 0, "ok", "x" ) ) %>%
  mutate( wged = if_else( Waga_G == 4 , "ok", "x" ) ) %>%
  mutate( wgsum = paste( wgpp, wgap, wged, sep = "")) %>%
  filter( !( wgsum == "xxx")) %>%
  select( -wgpp, -wgap, -wged, -wgsum) %>%
  filter( !wpf == 0) %>% filter( !waf == 0) %>%
  filter( !Art_sub_type == "") %>%
  select( -sub_pp, -sub_pf, -sub_af, -sub_ap)  

ARt_GSFP <- left_join( ARt_SFP, GGoblet, "uchwyt")

CCirclet <- left_join(Circlet, avg_sub_stat1, "uchwyt") %>%
  mutate( wgpp = if_else( Waga_C == 0 & wpp == 0, "ok", "x" ) ) %>%
  mutate( wgap = if_else( Waga_C == 1 & wap == 0, "ok", "x" ) ) %>%
  mutate( wghe = if_else( Waga_C == 13 , "ok", "x" ) ) %>%
  mutate( wgsum = paste( wgpp, wgap, wghe, sep = "")) %>%
  filter( !( wgsum == "xxx")) %>%
  select( -wgpp, -wgap, -wghe, -wgsum) %>%
  filter( !wpf == 0) %>% filter( !waf == 0) %>%
  filter( !Art_sub_type == "") %>%
  select( -sub_pp, -sub_pf, -sub_af, -sub_ap)  

Artifact_X <- left_join( ARt_GSFP, CCirclet, "uchwyt")

Artifact <- Artifact_X %>%
  mutate( Waga = Waga_S + Waga_G + Waga_C) %>%
  mutate( Art_type = paste(S_typ, G_typ, C_typ, sep = " ") ) %>%
  mutate( Sub_art = paste(Art_sub_type.x, Art_sub_type.y,
                          Art_sub_type.x.x, Art_sub_type.y.y,
                          Art_sub_type, sep = " ") ) %>%
  select( -Waga_S, -Waga_G, -Waga_C, -S_typ, -G_typ, -C_typ ) %>%
  select( -Art_sub_type.x, -Art_sub_type.x.x,
          -Art_sub_type.y, -Art_sub_type.y.y, -Art_sub_type) %>%
  mutate( WagaXType = paste( Waga, Sub_art) ) %>%
  distinct( WagaXType, .keep_all = TRUE) %>%
  mutate( Art_Atk_proc = Atk_proc_S + Atk_proc_G.x + Atk_proc_G.y +
            wap.x + wap.x.x + wap.y + wap.y.y + wap) %>%
  mutate( Art_HP_proc = HP_proc_S + HP_proc_G.x + HP_proc_G.y +
            wpp.x + wpp.x.x + wpp.y + wpp.y.y + wpp) %>%
  mutate( Art_Atk_flat = Atk_flat_P +
            waf.x + waf.x.x + waf.y + waf.y.y + waf) %>%
  mutate( Art_HP_flat = HP_flat_F +
            wpf.x + wpf.x.x + wpf.y + wpf.y.y + wpf) %>%
  select( Art_HP_flat, Art_Atk_flat,
          Art_HP_proc, Art_Atk_proc,
          El_dmg_G, Heal_G, Art_type, Sub_art, uchwyt)

Sett <- Sett %>% mutate( uchwyt = c("A_r") )

Set_name <- tribble(~ASet, ~Name_Set,
                    0, "Bez_setu",
                    1, "4xHeart_of_Depth")

ArtSet <- tribble(~Atk_AS, ~HP_AS, ~HDRAS, ~HealAS,
                  ~N_AS, ~CH_AS, ~E_AS, ~Q_AS, ~ASetAS, ~uchwyt,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, "A_r",
                  0, 0, 15, 0, 30, 30, 0, 0, 1, "A_r")

Sett2 <- left_join( Sett, ArtSet, "uchwyt") %>%
  mutate( Atk_proc = Atk_proc + Atk_AS, HP_proc = HP_proc + HP_AS,
          HDR = HDR + HDRAS, Heal = Heal + HealAS,
          N_bonus = N_bonus + N_AS, CH_bonus = CH_bonus + CH_AS, 
          E_bonus = E_bonus + E_AS, Q_bonus = Q_bonus + Q_AS, ASet = ASet + ASetAS) %>%
  select(1:15,)

Staty_Kokommi <- left_join( Sett2, Artifact, "uchwyt") %>%
    mutate( Atk_proc = Atk_proc + Art_Atk_proc, Atk_flat = Atk_flat + Art_Atk_flat) %>%
    mutate( HP_proc = HP_proc + Art_HP_proc, HP_flat = HP_flat + Art_HP_flat) %>% 
    mutate( HDR = HDR + El_dmg_G) %>%
    mutate( ATK = Atk_base * ( 1 + Atk_proc/100) + Atk_flat, 
            HPMax = HP_base * ( 1 + HP_proc/100) + HP_flat ) %>%
    select( ATK, HPMax, HDR, Wpn, uchwyt, Heal_G, Art_type, Sub_art, Heal, ASet) %>%
    mutate( Heal_dmg = ( Heal_G + Heal)*0.15 ) %>%
    mutate( N_HP_proc = 8.71 + Heal_dmg,
            CH_HP_proc = 12.2 + Heal_dmg,
            ED_HP_proc = 12.77) %>%
    mutate( N_HP_bonus = N_HP_proc/100 * HPMax,
            CH_HP_bonus = CH_HP_proc/100 * HPMax,
            ED_HP_bonus = ED_HP_proc/100 * HPMax) %>%
    mutate( Out_dmg = ATK* ( 1 + HDR/100 ) )


FT10B <- function( zesto){ 
  zesto %>% mutate(N1 = Out_dmg*1.2308 + N_HP_bonus,
                   N2 = Out_dmg*1.1077 + N_HP_bonus,
                   N3 = Out_dmg*1.6975 + N_HP_bonus,
                   CH = Out_dmg*2.6698 + CH_HP_bonus,
                   ED = Out_dmg*1.9654 + ED_HP_bonus,
                   Reg_per_hit = (HPMax * 0.145 + 169)*( 1 + (Heal_G + Heal)/100 ) ) %>%
    mutate( N1 = if_else( Wpn == "Moonglow", N1 + HPMax*0.01, N1) ) %>%
    mutate( N2 = if_else( Wpn == "Moonglow", N2 + HPMax*0.01, N2) ) %>%
    mutate( N3 = if_else( Wpn == "Moonglow", N3 + HPMax*0.01, N3) ) %>%
    arrange( desc(N1) )
}

wyjscieT10 <- FT10B(Staty_Kokommi)[1:50, ]

WBT10 <- FT10B(Staty_Kokommi)

FT10N <- function( zesto){ 
  zesto %>% mutate(N1 = Out_dmg*1.2308,
                   N2 = Out_dmg*1.1077,
                   N3 = Out_dmg*1.6975,
                   CH = Out_dmg*2.6698,
                   ED = Out_dmg*1.9654) %>%
    mutate( N1 = if_else( Wpn == "Moonglow", N1 + HPMax*0.01, N1) ) %>%
    mutate( N2 = if_else( Wpn == "Moonglow", N2 + HPMax*0.01, N2) ) %>%
    mutate( N3 = if_else( Wpn == "Moonglow", N3 + HPMax*0.01, N3) ) %>%
    arrange( desc(N1) )
}

WBN10 <- FT10N(Staty_Kokommi)

FdmgoutB <- WBT10 %>% select( ATK, HPMax, Wpn, Art_type, Sub_art, N1, N2, N3, CH, ED, Reg_per_hit, ASet) %>%
  mutate( AVG_Normal = ( N1 + N2 + N3)/3)

FdmgoutN <- WBN10 %>% select( ATK, HPMax, Wpn, Art_type, Sub_art, N1, N2, N3, CH, ED, ASet) %>%
  mutate( AVG_Normal = ( N1 + N2 + N3)/3)

#FoutB <- FdmgoutB %>% gather("Zdolnosc", "Obrazenia", 6:10 )

avgdmgB <- FdmgoutB %>% group_by( Wpn, Art_type, ASet) %>% summarise( AVG_N = mean(AVG_Normal),
                                                                   AVG_Heal = mean(Reg_per_hit),
                                                                   AVG_HP = mean(HPMax),
                                                                   AVG_CH = mean(CH),
                                                                   AVG_ED = mean(ED)) %>%
  arrange( desc(AVG_N) ) %>% ungroup()

avgdmgN <- FdmgoutN %>% group_by( Wpn, Art_type, ASet) %>% summarise( AVG_N = mean(AVG_Normal),
                                                                   AVG_HP = mean(HPMax),
                                                                   AVG_CH = mean(CH),
                                                                   AVG_ED = mean(ED)) %>%
  arrange( desc(AVG_N) ) %>% ungroup()

dane_burst <- avgdmgB %>% select(1:5, 7:8) %>% mutate( uchwyt = paste(Wpn, Art_type ))
dane_normal <- avgdmgN %>% select(1:4, 6:7) %>% mutate( uchwyt = paste(Wpn, Art_type ))

dane_razem <- left_join( dane_burst, dane_normal, "uchwyt") %>%
  mutate( AVGN = ( AVG_N.x + AVG_N.y)/2 ) %>%
  mutate( AVGC = ( AVG_CH.x + AVG_CH.y)/2 ) %>%
  mutate( AVED = ( AVG_ED.x + AVG_ED.y)/2 ) %>%
  select(1:7, 12:17)
colnames(dane_razem) <- c("Weapon", "Artifact_type", "Set_type","Normal_Burst",
                          "AVG_Heal", "Charged_Burst", "Elemental_Burst",
                          "Normal", "Charged", "Elemental", 
                          "On/Off_Burst_Normal", "On/Off_Burst_Charged", "On/Off_Burst_Elemental" )

dane_razemv2 <- dane_razem %>% gather("Rodzaj_ataku", "DMG", 4:13) %>%
  filter(!(Rodzaj_ataku == "AVG_Heal")) %>%
  filter( !Rodzaj_ataku %in% c("Elemental","Elemental_Burst", "On/Off_Burst_Elemental")  )

dane_razemv2_1 <- left_join(dane_razemv2, Weapon_name, by = c("Weapon" = "Wpn") )

dane_razemv3 <- dane_razem %>% gather("Rodzaj_ataku", "DMG", 4:13) %>%
  filter( Rodzaj_ataku %in% c("Elemental","Elemental_Burst", "On/Off_Burst_Elemental")  )

ggplot( data = filter(dane_razemv2, Set_type == 1) ) + 
  geom_point( aes( Artifact_type, DMG, color = Rodzaj_ataku, shape = Rodzaj_ataku), size = 2 ) +
  ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
  theme(  legend.position = c( 0.65, 0.15) )+
  guides( color = guide_legend( nrow = 2 ) ) +
  scale_color_brewer( palette = "Dark2") +
  coord_flip() + facet_wrap(~Weapon)
