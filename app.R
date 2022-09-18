#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
load("./dane/Heizou.RData")

WpnCata <- read_csv2("./Weapon/Catalyst.csv")
Artefakty <- read_csv2("./Weapon/ArtSet.csv")


Hei_opis <- tabPanel("Założenia", fluidPage( h4("Postać: poziom 90, C0, talenty 3x10, Broń: poziom 90, R1, dodatkowe efekty nie wpływające na statystyki nie są uwzględnione."),
                                            h4("Artefakty: +20, 5*, substaty w ATK(a), ATK%(A), Crit rate(C), Crit Damage(X), Elemental Mastery(E) z wyjątkiem przypadków gdyby miały pokrywać się z główną statystyką. Cyfry na wykresach informują o ilości artefaktów z wymaksowanym danym substatem. Uśredniona wartość tierów."),
                                            h4("Normal - liczony jako średnia z uderzeń.")
                                            ) )
Hei_avg <- tabPanel("Porównanie setów", fluidPage( plotOutput("Heizou_avg_plot", width = "100%", height = "900px") ) )
Hei_set1 <- tabPanel("Vermillion Hereafter", fluidPage( plotOutput("Heizou_set1", width = "100%", height = "900px") ))
Hei_set2 <- tabPanel("Viridescent Venerer", fluidPage( plotOutput("Heizou_set2", width = "100%", height = "900px") ))
Hei_set3 <- tabPanel("Noblesse Oblige", fluidPage( plotOutput("Heizou_set3", width = "100%", height = "900px") ))
Hei_set4 <- tabPanel("Gladiator's Finale + Viridescent Venererr", fluidPage( plotOutput("Heizou_set4", width = "100%", height = "900px") ))
Hei_set5 <- tabPanel("Wanderer's Troupe + Viridescent Venerer", fluidPage( plotOutput("Heizou_set5", width = "100%", height = "900px") ))

Heizou <- tabPanel("Heizou", h3("Heizou"),
                   tabsetPanel( Hei_opis, Hei_avg, Hei_set1, Hei_set2,
                                Hei_set3, Hei_set4, Hei_set5) )
TEST1 <- tabPanel("TEST1")

Postaci <- navbarMenu( "Postaci",
                        Heizou, TEST1
                        )

Opis <- tabPanel( "Opis")

Flower1 <- tabPanel( "Flower",
                     column(3, numericInput( "AF1F", "Atk", 0, min = 0, max = 100) ),
                     column(3, numericInput( "AP1F", "Atk%", 0, min = 0, max = 100) ),
                     column(3, numericInput( "CR1F", "Crit Rate", 0, min = 0, max = 100) ),
                     column(3, numericInput( "CD1F", "Crit Dmg", 0, min = 0, max = 100) ),
                     column(3, numericInput( "ER1F", "Energy R.", 0, min = 0, max = 100) ),
                     column(3, numericInput( "EM1F", "E. Mastery", 0, min = 0, max = 100) ),
                     column(3, numericInput( "HPF1F", "HP", 0, min = 0, max = 100) ),
                     column(3, numericInput( "HPP1F", "HP%", 0, min = 0, max = 100) ),
                     column(3, numericInput( "DF1F", "Def", 0, min = 0, max = 100) ),
                     column(3, numericInput( "DP1F", "Def%", 0, min = 0, max = 100) ) )

Plume1 <- tabPanel( "Plume",
                    column(3, numericInput( "AF1P", "Atk", 0, min = 0, max = 100) ),
                    column(3, numericInput( "AP1P", "Atk%", 0, min = 0, max = 100) ),
                    column(3, numericInput( "CR1P", "Crit Rate", 0, min = 0, max = 100) ),
                    column(3, numericInput( "CD1P", "Crit Dmg", 0, min = 0, max = 100) ),
                    column(3, numericInput( "ER1P", "Energy R.", 0, min = 0, max = 100) ),
                    column(3, numericInput( "EM1P", "E. Mastery", 0, min = 0, max = 100) ),
                    column(3, numericInput( "HPF1P", "HP", 0, min = 0, max = 100) ),
                    column(3, numericInput( "HPP1P", "HP%", 0, min = 0, max = 100) ),
                    column(3, numericInput( "DF1P", "Def", 0, min = 0, max = 100) ),
                    column(3, numericInput( "DP1P", "Def%", 0, min = 0, max = 100) ))

Sands1 <- tabPanel( "Sands",
                    selectInput( "Sands1", "Główna statystyka", c("Atk%", "HP%", "Def%", "EM", "ER") ),
                    column(3, numericInput( "AF1S", "Atk", 0, min = 0, max = 100) ),
                    column(3, numericInput( "AP1S", "Atk%", 0, min = 0, max = 100) ),
                    column(3, numericInput( "CR1S", "Crit Rate", 0, min = 0, max = 100) ),
                    column(3, numericInput( "CD1S", "Crit Dmg", 0, min = 0, max = 100) ),
                    column(3, numericInput( "ER1S", "Energy R.", 0, min = 0, max = 100) ),
                    column(3, numericInput( "EM1S", "E. Mastery", 0, min = 0, max = 100) ),
                    column(3, numericInput( "HPF1S", "HP", 0, min = 0, max = 100) ),
                    column(3, numericInput( "HPP1S", "HP%", 0, min = 0, max = 100) ),
                    column(3, numericInput( "DF1S", "Def", 0, min = 0, max = 100) ),
                    column(3, numericInput( "DP1S", "Def%", 0, min = 0, max = 100) ) )

Goblet1 <- tabPanel( "Goblet",
                     selectInput( "Goblet1", "Główna statystyka", c("Atk%", "HP%", "Def%", "EM", "ER") ),
                     column(3, numericInput( "AF1G", "Atk", 0, min = 0, max = 100) ),
                     column(3, numericInput( "AP1G", "Atk%", 0, min = 0, max = 100) ),
                     column(3, numericInput( "CR1G", "Crit Rate", 0, min = 0, max = 100) ),
                     column(3, numericInput( "CD1G", "Crit Dmg", 0, min = 0, max = 100) ),
                     column(3, numericInput( "ER1G", "Energy R.", 0, min = 0, max = 100) ),
                     column(3, numericInput( "EM1G", "E. Mastery", 0, min = 0, max = 100) ),
                     column(3, numericInput( "HPF1G", "HP", 0, min = 0, max = 100) ),
                     column(3, numericInput( "HPP1G", "HP%", 0, min = 0, max = 100) ),
                     column(3, numericInput( "DF1G", "Def", 0, min = 0, max = 100) ),
                     column(3, numericInput( "DP1G", "Def%", 0, min = 0, max = 100) ) )

Circlet1 <- tabPanel( "Circlet", 
                      selectInput( "Circlet1", "Główna statystyka", c("Atk%", "HP%", "Def%", "EM", "ER") ),
                      column(3, numericInput( "AF1C", "Atk", 0, min = 0, max = 100) ),
                      column(3, numericInput( "AP1C", "Atk%", 0, min = 0, max = 100) ),
                      column(3, numericInput( "CR1C", "Crit Rate", 0, min = 0, max = 100) ),
                      column(3, numericInput( "CD1C", "Crit Dmg", 0, min = 0, max = 100) ),
                      column(3, numericInput( "ER1C", "Energy R.", 0, min = 0, max = 100) ),
                      column(3, numericInput( "EM1C", "E. Mastery", 0, min = 0, max = 100) ),
                      column(3, numericInput( "HPF1C", "HP", 0, min = 0, max = 100) ),
                      column(3, numericInput( "HPP1C", "HP%", 0, min = 0, max = 100) ),
                      column(3, numericInput( "DF1C", "Def", 0, min = 0, max = 100) ),
                      column(3, numericInput( "DP1C", "Def%", 0, min = 0, max = 100) ) )


TSET1 <- tabPanel( "Set1",
                   selectInput( "Char1", "Postać", c("Heizou", "Pos1") ), 
                   selectInput( "Weap1", "Broń", unique( WpnCata$WName ) ),
                   numericInput( "STACK1", "Ilość stack'ów", 0, 0, 4),
                   fluidRow( column(6, selectInput( "Art1A", "Artefakt", unique( Artefakty$Name_Set ) )),
                             column(6, selectInput( "Art1B", "Artefakt", unique( Artefakty$Name_Set ) )) ),
                   tabsetPanel( Flower1, Plume1, Sands1, Goblet1, Circlet1 )
                   )

TSET2 <- tabPanel( "Set2")

TPanel <- fluidPage( sidebarLayout( sidebarPanel( tabsetPanel( TSET1, TSET2 ), width = 3 ), mainPanel() ) )

Testy <- tabPanel("Testy", TPanel)

# Define UI for application that draws a histogram
ui <- navbarPage( "",
                  Postaci, Opis, Testy
)

Hei_avg_plot <- function(){
  
    ggplot( ) +
    geom_col( data = filter(Heizou_avg, Rodzaj_ataku == "Conviction_Strike"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    geom_col( data = filter(Heizou_avg, Rodzaj_ataku == "Burst"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    geom_col( data = filter(Heizou_avg, Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    geom_col( data = filter(Heizou_avg, Rodzaj_ataku == "Charged"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    geom_col( data = filter(Heizou_avg, Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
    scale_fill_viridis_d( option = "C") +
    scale_color_brewer() +
    coord_flip() + facet_grid(WName~Name_Set)
}

Hei_plot_set <- function(nrset){
  
    ggplot( ) +
    geom_col( data = filter(Heizou_HSC, IDAS == nrset & Rodzaj_ataku == "Conviction_Strike"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    geom_col( data = filter(Heizou_HSC,  IDAS == nrset & Rodzaj_ataku == "Burst"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    geom_col( data = filter(Heizou_HSC,  IDAS == nrset & Rodzaj_ataku == "Elemental"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    geom_col( data = filter(Heizou_HSC,  IDAS == nrset & Rodzaj_ataku == "Charged"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    geom_col( data = filter(Heizou_HSC,  IDAS == nrset & Rodzaj_ataku == "Normal"), aes( Art_type, DMG, fill = Rodzaj_ataku) ) +
    geom_text( data = filter(Heizou_HSC,  IDAS == nrset & Rodzaj_ataku == "Normal"), aes( Art_type, 125000, label = Art_sub_x), size = 3 ) +
    ylab("Przeciętne obrażenia") + xlab("Kombinacje artefaktów: Sands, Goblet, Circlet") +
    ylim(0, 150000)+
    scale_fill_brewer( palette = "Set1") +
    scale_color_brewer( palette = "Set3") +
    coord_flip() + facet_wrap(~WName)

}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Heizou_avg_plot <- renderPlot( { Hei_avg_plot() } )
  output$Heizou_set1 <- renderPlot( { Hei_plot_set(43) } )
  output$Heizou_set2 <- renderPlot( { Hei_plot_set(12) } )
  output$Heizou_set3 <- renderPlot( { Hei_plot_set(6) } )
  output$Heizou_set4 <- renderPlot( { Hei_plot_set(211) } )
  output$Heizou_set5 <- renderPlot( { Hei_plot_set(411) } )

}

# Run the application 
shinyApp(ui = ui, server = server)
