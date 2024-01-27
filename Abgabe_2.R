#### Aufgabe 3.1 ####


Personen <- 1:10       #Anzahl der Personen
Geschenke <- 1:10      #Anzahl Geschenke
iterations <- 100000   #Anzahl Wiederholungen
counter <- 0           #zaehlt positive Fälle

#Simulation das eigene Geschenk zu ziehen
for(i in 1:iterations) {
  (Personenziehung <- sample (Personen, size=10, replace=FALSE))&
  (Geschenkziehung <- sample (Geschenke, size=10, replace=FALSE))
  if(any(Personenziehung == Geschenkziehung)) {
    counter <- counter+1
  }  
}                      

counter/iterations     #berechnet die simulierte Wahrscheinlichkeit



#### Aufgabe 3.2 ####


#Erstellen der Funktion wichtel_unglueck mit den Eingaben:
 #n = Anzahl der Personen die beim Wichteln mitmachen
 #k = Anzahl der Personen die ihr eigenes Geschenk ziehen
 #iterationen = Anzahl der Wiederholungen, Standard liegt bei 1000
wichtel_unglueck <- function (n, k, iterationen=1000) {

  #"k" muss größer als 0 und kleiner als "n" sein, wenn nicht stoppt die Funktion
  if(k < 0 || k > n) {
    stop("Ungueltige Eingabe, k muss zwischen 0 und n liegen")
  }
  
#wenn "Iterationen" nicht positiv ist stoppt die Funktion    
  if(iterationen<1) {
    stop("Ungueltige Eingabe, Iterationen muss eine positive Zahl sein")
  }
  
#wenn "Iterationen" keine ganze Zahl ist stoppt die Funktion
  if(round(iterationen)!=iterationen) {
    stop("Ungueltige Eingabe, Iterationen muss eine ganze Zahl sein")
  }
  
#Zähler für die Fälle in denen eine Person sich selber zieht
  eigenes_geschenk <- 0
  
#Simulation dafür das aus n Personen k Personen das eigene Geschenk ziehen
  for (i in 1:iterationen) {
    Geschenkzuordnung <- sample(1:n, n, replace=FALSE)
    if (sum(1:n == Geschenkzuordnung) >= k) {
      eigenes_geschenk=eigenes_geschenk+1
   }
  }
  
#ausrechnen der Wahrscheinlichkeit
  wahrscheinlichkeit <- eigenes_geschenk / iterationen

#Ausgabe:
 #Wahrscheinlichkeit dafür, dass unter n Personen k Personen das eigene
 #Geschenk ziehen
  return(wahrscheinlichkeit)
}



#### Aufgabe 3.4 ####


#laden erforderlicher Pakete
library(testthat)   

#testen der Funktion mit 4 Testfällen:
 #1. n=1 und k=1 soll immer 1 als Ergebnis ausgeben
 #2. wenn k größer als n ist soll ein Fehler ausgegeben werden
 #3. wenn bei Iterationen eine keine ganze Zahl steht soll ein
 #   Fehler ausgegeben werden
 #4. wenn bei Iterationen keine Zahl sondern ein Wort steht soll ein
 #   Fehler ausgegebn werden

test_that(            
  "Test der Funktion",
   {expect_equal(wichtel_unglueck(n=1, k=1, 10000),1)
    
    expect_error(wichtel_unglueck(n=10, k=11, 10000))   
    
    expect_error(wichtel_unglueck(n=10, k=1, 10/3))
    
    expect_error(wichtel_unglueck(n=10, k=1, hundert))
    }
)



#### Aufgabe 3.5 ####


#einlesen der Daten
alle_daten <- read.csv(file="Capital_bikeshare_data_2022_with_NAs (2).csv",
                         sep=",",
                         dec=".",
                         header=TRUE)

#überprüfen ob der Datensatz als Data Frame eingelesen wurde
class(alle_daten)   

#Daten nach unserer Station filtern und neuen Data Frame erstellen
unsere_station <- data.frame(subset(alle_daten, station=="Eckington Pl & Q St NE"))

#überprüfen ob es NA´s gibt
anyNA(unsere_station)

#an welchen Stellen sind die NA´s?
which(is.na(unsere_station))

#wie viele NA´s gibt es?
sum(!complete.cases(unsere_station))

#Data Frame neu abspeichern ohne die NA´s
unsere_station <- unsere_station[complete.cases(unsere_station),]

#überprüfen ob die Daten plausibel sind
range(unsere_station$date)
range(unsere_station$station)
range(unsere_station$count)
range(unsere_station$wind_speed)
range(unsere_station$precipitation)
range(unsere_station$snowfall)
range(unsere_station$snow_depth)
range(unsere_station$mean_temperature)
range(unsere_station$max_temperature)
range(unsere_station$min_temperature)

#unplausible Werte abspeichern
unplausibler_wert <- which(unsere_station$wind_speed == -1.00)

#unplausible Werte entfernen
unsere_station <- unsere_station[-unplausibler_wert,]

#Variable date als Datumsvariable formatieren
unsere_station$date <- as.Date(unsere_station$date, format="%Y-%m-%d")

#überprüfen ob korrekt umgewandelt wurde
class(unsere_station$date)



#### Aufgabe 4.1 ####


#laden erforderlicher Pakete
library(ggplot2)
library(dplyr)
library(gridExtra)

#Grafik stellt den Zusammenhang zwischen ausgeliehenen Fahrrädern 
#und der Durchschnittstemperatur (in °F) dar
count_meantemp <- ggplot(data=unsere_station) + 
  geom_point(aes(x=mean_temperature,
                 y=count),col="blue") +
  xlab("Durchschnittstemperatur (in °F)") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag vs. Durchschnittstemperatur") +
  theme_minimal()


#Grafik stellt den Zusammenhang zwischen ausgeliehenen Fahrrädern
#und der Niederschlagsmenge pro Tag (in inch) dar
count_prec <- ggplot(data=unsere_station) +
  geom_point(aes(x=precipitation,
                 y=count),col="purple") +
  xlab("Niederschlagsmenge pro Tag (in inch)") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag vs. Regenmenge") +
  theme_minimal()


#Grafik stellt den Zusammenhang zwischen Fahrradausleihen pro Tag
#und der Windgeschwindigkeit (in mph) dar
count_wind <- ggplot(data=unsere_station) +
  geom_point(aes(x=wind_speed,
                 y=count),col="green") +
  xlab("Windgeschwindigkeit (in mph)") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag vs. Windgeschwindigkeit") +
  theme_minimal()


#Grafik stellt die Anzahl der Fahrradausleihen pro Tag dar
count_date <- ggplot(data=unsere_station) +
  geom_point(aes(x=date,
                 y=count),col="red") +
  xlab("Tag") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag") +
  theme_minimal()

#alle Grafiken zusammmen darstellen
grid.arrange(count_meantemp, count_prec,
             count_wind, count_date,
             nrow=2, ncol=2)



#### Aufgabe 4.2 ####


#Grafik stellt den Zusammenhang zwischen Fahrradausleihen pro Tag
#und der Durchschnittstemperatur (in °F), an Tagen an denen es nicht
#geregnet hat, dar
count_meantemp_norain <- ggplot(data=filter(unsere_station, 
                                            precipitation == 0)) + 
  geom_point(aes(x=mean_temperature,
                 y=count),col="red") +
  xlab("Durchschnittstemperatur (in °F)") +
  ylab("Anzahl ausgeliehener Fahrräder pro Tag") +
  ggtitle("Fahrradausleihen pro Tag vs. Durchschnittstemperatur(kein Regen)") +
  theme_minimal()



#Grafik stellt den Zusammenhang zwischen Fahrradausleihen pro Tag 
#und der Durchschnittstemperatur (in °F), an Tagen an denen es geregnet hat, dar
count_meantemp_rain <- ggplot(data=filter(unsere_station, precipitation > 0)) + 
  geom_point(aes(x=mean_temperature,
                 y=count),col="blue") +
  xlab("Durchschnittstemperatur (in °F)") +
  ylab("Fahrradausleihen pro Tag") +
  ggtitle("Fahrradausleihen pro Tag vs. Durchschnittstemperatur (Regen)") +
  theme_minimal()


#beides in einer Grafik darstellen
grid.arrange(count_meantemp_norain, count_meantemp_rain,
             ncol=2, nrow=1)



#### Aufgabe 4.3 ####


#Grafik stellt die Verteilung der Fahrradausleihen pro Tag als 
#Histogramm dar
verteilung_count <- ggplot(data=unsere_station) +
  geom_histogram(aes(x=count, y=after_stat(density)),
                 col="#a6611a",fill="#dfc27d") +
  xlab("Fahrradausleihen pro Tag") +
  ylab("Dichte") +
  ggtitle("Verteilung der Fahrradausleihen pro Tag") +
  theme_minimal()


#Grafik stellt die Verteilung der Durchschnittstemperaturen (in °F) als
#Histogramm dar
verteilung_meantemp <- ggplot(data=unsere_station) +
  geom_histogram(aes(x=mean_temperature, y=after_stat(density)),
                 col="#018571", fill="#80cdc1") +
  xlab("Durchschnittstemperatur (in °F)") +
  ylab("Dichte") +
  ggtitle("Verteilung der Durchschnittstemperaturen") +
  theme_minimal()


#Grafik stellt die Verteilung der Niederschlagsmenge (in inch) als 
#Histogramm dar
verteilung_prec <- ggplot(data=unsere_station) +
  geom_histogram(aes(x=precipitation, y=after_stat(density)),
                 col="#5e3c99",fill="#b2abd2") +
  xlab("Niederschlagsmenge (in inch)") +
  ylab("Dichte") +
  ggtitle("Verteilung der Niederschlagsmengen") +
  theme_minimal()


#Grafik stellt die Verteilung der Windgeschwindigkeit (in mph) als
#Histogramm dar
verteilung_wind <- ggplot(data=unsere_station) +
  geom_histogram(aes(x=wind_speed, y=after_stat(density)),
                 col="#4dac26",fill="#b8e186") +
  xlab("Windgeschwindigkeit (in mph)") +
  ylab("Dichte") +
  ggtitle("Verteilung der Durchschnittstemperaturen") +
  theme_minimal()


#alle Grafiken zusammen darstellen
grid.arrange(verteilung_count, verteilung_meantemp,
             verteilung_prec, verteilung_wind,
             nrow=2, ncol=2)



#### Aufgabe 4.4 ####


#die Datumsangaben in die 4 meteorologischen Jahreszeiten getrennt
Jahreszeiten <- cut(unsere_station$date, breaks = as.Date(c("2022-01-01",
                                                            "2022-03-01",
                                                            "2022-06-01",
                                                            "2022-09-01",
                                                            "2022-12-01")),
               labels = c("Winter", "Frühling", "Sommer", "Herbst"),
               right = FALSE)


#Grafik stellt die Verteilung der Fahrradausleihen pro Tag getrennt
#nach den 4 meteorologischen Jahreszeiten dar
count_jahreszeiten <- ggplot(unsere_station, aes(x=count, fill=Jahreszeiten)) +
  geom_density(aes(color=Jahreszeiten), alpha=0.5) +
  scale_fill_manual(values=c("Winter"="blue",
                             "Frühling"="green",
                             "Sommer"="yellow",
                             "Herbst"="red")) +
  scale_color_manual(values=c("Winter"="darkblue",
                              "Frühling"="darkgreen",
                              "Sommer"="orange",
                              "Herbst"="darkred")) +
  xlab("Fahrradausleihen pro Tag") +
  ylab("Dichte") +
  ggtitle("Verteilung der Fahrradausleihen pro Tag getrennt nach Jahreszeiten") +
  theme_minimal()



#### Aufgabe 4.5 ####


#laden erforderlicher Pakete
library(plotly)

#Grafik stellt die Fahrradausleihen pro Tag der Windgeschwindikeit (in mph) und 
#der Durchschnittstemperatur (in °F) gegenüber
count_wind_meantemp <- plot_ly(data=unsere_station,
        x=~mean_temperature,
        y=~wind_speed,
        z=~count,
        type="scatter3d",
        mode="markers",
        marker=list(size=5, opacity=0.5), color=~count) %>% layout(
        scene=list(xaxis=list(title="Durchschnittstemperatur (in °F)"),
                   yaxis=list(title="Windgeschwindigkeit (in mph)"),
                   zaxis=list(title="Anzahl ausgeliehener Fahrräder pro Tag")),
                   title=
"Fahrradausleihen pro Tag vs. Windgeschwindigkeit vs. Durchschnittstemperatur")
                             
      

 















