# ŞAH MAT
           
Veri Seti Tanıtımı              
Görmüş olduğunuz bu grafiklerin hepsi 2,6 Milyon oynanmış bir oyunun özetidir. Bu veri setinde oyuncuların hangi rankta oldukları, hangi açılışı yaptıkları , oyunun kaç hamlede bittiği gibi veriler yer almaktadır. Aşağıda verilmiş olan grafiklerde ilginizi çekebilecek noktalara değinmeye çalıştım.

Veri Seti:
--
[chess-game.csv](https://www.kaggle.com/datasets/arevel/chess-games) \
Grafik Çıktıları:
--
- ![Grafik 1](https://github.com/huseyinkayar/Veri_Gorsellestirme_Final_Odevi/blob/main/Graphics/1-Renge%20G%C3%B6re.png?raw=true)

- ![Grafik 2](https://github.com/huseyinkayar/Veri_Gorsellestirme_Final_Odevi/blob/main/Graphics/2-A%C3%A7%C4%B1l%C4%B1%C5%9F%20Hamlelerine%20G%C3%B6re%20Kazan%C4%B1lan%20Oyun%20Say%C4%B1s%C4%B1.png?raw=true)

- ![Grafik 3](https://github.com/huseyinkayar/Veri_Gorsellestirme_Final_Odevi/blob/main/Graphics/3-Derecelere%20G%C3%B6re%20En%20S%C4%B1k%20Kazanan%20A%C3%A7%C4%B1l%C4%B1%C5%9Flar.png?raw=true)

- ![Grafik 4](https://github.com/huseyinkayar/Veri_Gorsellestirme_Final_Odevi/blob/main/Graphics/4-Oyun%20T%C3%BCr%C3%BCne%20G%C3%B6re%20Yenilme%20%C5%9Eekli.png?raw=true)


Kodlar:
--
```r
#Import packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("tidyverse")
install.packages("MetBrewer")
install.packages("treemapify")
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)
library(MetBrewer)
library(treemapify)



#Import the data set
chessDataSet <- read.csv("chess_games.csv")

#nNmber of black wins
blackPlayersWin <- nrow(chessDataSet %>% filter(Result == "0-1"))

#Number of white wins
whitePlayersWin <- nrow(chessDataSet %>% filter(Result == "1-0"))

#Number of draws
draw <- nrow(chessDataSet %>% filter(Result == "1/2-1/2"))


#Creating data frame for winning rates 
winrate <- data.frame(
  winner = c("Siyah","Beyaz","Beraber"),
  percent = c(blackPlayersWin/nrow(chessDataSet),
              whitePlayersWin/nrow(chessDataSet), 
              draw/nrow(chessDataSet)
  ))


#Pie chart and legend for winning rates
pie(x = winrate$percent, label="", 
    col=c("grey1", "grey100", "grey65"), main="Kazanma Oranı") 
legend("right",
       legend=paste(winrate$winner, format(round(winrate$percent,2),nsmall=2), "%"),
       fill = c("grey1", "grey100", "grey65"),      
       border = "black") 


#Which opening is better in winning
chessOpeningWinningRate <- chessDataSet %>% 
  filter(Result=="1-0") %>%
  group_by(Opening) %>%
  summarise(number = n())


#Order the data set with descending number
chessOpeningWinningRate <- chessOpeningWinningRate[order(-chessOpeningWinningRate$number),]


#Firt 20 opening
chessOpeningWinningRateRestricted <- chessOpeningWinningRate[1:20,]


#Most won opening (first 20)
ggplot(chessOpeningWinningRateRestricted, aes(
  y = number, 
  x = fct_reorder(Opening,number),
  fill=fct_reorder(Opening,number))) + 
  geom_bar(position = "stack", 
           stat = "identity") +
  labs(x = "Açılış Hamlesi",
       y = "Kazanılan Oyun Sayısı",
       title = "Açılış Hamlelerine Göre Kaznılan Oyun Sayısı",
       subtitle = "En Çok Kullanılan 20 Hamle") +
  scale_x_discrete(labels= c("Hindistan Oyunu",
                             "Philidor Savunması",
                             "Vezir Gambiti Oyunu: Zukertort Varyansı",
                             "Fil Açılışı",
                             "İskoç Açılışı",
                             "Vezir Gambiti Oyunu: Chigorin Varyansı",
                             "Macar Açılışı",
                             "Sicilya Savunması: Bowdler Saldırısı",
                             "Vezir Gambiti Oyunu 2",
                             "Vezir Gambiti",
                             "Philidor Savunması 3",
                             "Sicilya Savunması",
                             "İskandinav Savunması",
                             "Caro-Kann Savunması",
                             "Owen'ın Savunması",
                             "Fransız Savunması : Şövalye Varyansı",
                             "Horwitz Savunması",
                             "Modern Savunma",
                             "Van't Kruijs Açılışı",
                             "İskandinav Savunması: Mieses-Kotroc Varyansı"))+
  coord_flip()+
  scale_fill_manual(values=met.brewer("Redon", 20))+
  theme(plot.title = element_text(size=20,face = "bold"),
        axis.text = element_text(size=15,face="bold"),
        axis.title= element_text(size = 20,face="bold"),
        legend.position = "none")


#Grouped data set for WhiteElo and Opening
openingAndElo <- chessDataSet %>% 
  group_by(WhiteElo,Opening) %>%
  summarise(number = n())

#Restricted the WhiteElo 
openingAndEloRestricted <- within(openingAndElo, {   
  WhiteElo[ WhiteElo < 1500 & WhiteElo >=700 ] <- "1.Derece"
  WhiteElo[ WhiteElo < 2500 & WhiteElo >=1500] <- "2.Derece"
  WhiteElo[ WhiteElo < 3200 & WhiteElo >=2500 ] <- "3.Derece"
})

#Select the random 20 sample in openings
openingsDataFrame <- data.frame(chessDataSet$Opening)
openingsSample <- openingsDataFrame[sample(nrow(openingsDataFrame), 20), ]

#Filter according to the "1.Derece" and grouped by elo and opening
openingAndEloRestrictedRank1 <- openingAndEloRestricted %>%
  filter(WhiteElo =="1.Derece",Opening %in% openingsSample) %>%
  group_by(WhiteElo,Opening) %>%
  summarise(number = n())

#Filter according to the "2.Derece" and grouped by elo and opening
openingAndEloRestrictedRank2 <- openingAndEloRestricted %>%
  filter(WhiteElo =="2.Derece",Opening %in% openingsSample) %>%
  group_by(WhiteElo,Opening) %>%
  summarise(number = n())

#Filter according to the "3.Derece" and grouped by elo and opening
openingAndEloRestrictedRank3 <- openingAndEloRestricted %>%
  filter(WhiteElo =="3.Derece",Opening %in% openingsSample) %>%
  group_by(WhiteElo,Opening) %>%
  summarise(number = n())

#Filtered elo datasets combined
finalOpeningAndElo <- rbind(rbind(openingAndEloRestrictedRank1,openingAndEloRestrictedRank2),openingAndEloRestrictedRank3)

#Graph for combined elo and openings sets
cbPalette <- c("#0072B2", "#D55E00", "#CC79A7")
ggplot(finalOpeningAndElo, aes(fill = WhiteElo, 
                               y = number, 
                               x = Opening)) +
  geom_bar(position = "dodge", 
           stat = "identity")+
  labs(x = "Açılış Hamlesi",
       y = "Kazanılan Oyun Sayısı",
       title = "Derecelere Göre En Sık Kullanılan Açılışlar",
       subtitle = "En Çok Kullanılan 20 Hamle",
       fill="Derece")+
  scale_x_discrete(labels= c("İngiliz Açılışı",
                             "Fransız Savunması 2",
                             "Fransız Savunması: Advance Varyansı,Euwe Varyansı",
                             "Fransız Savunması:İki Şövalye Varyansı",
                             "Horwitz Savunması",
                             "İtalyan Oyunu",
                             "İtalyan Oyunu:Anti-Fried Liver Savunması",
                             "Kral Gambiti:Macleod Saldırısı",
                             "Owen Savunması",
                             "Pirc Savunması 4",
                             "Ponziani Açılışı: Jaenisch Kontra Atağı",
                             "Vezir Gambiti Kabulü:Normal Varyansı",
                             "Vezir Gambiti Kabulü:Eski Varyansı",
                             "Vezir Gambiti Oyunu 2",
                             "Vezir Gambiti Oyunu:Colle Sistemi",
                             "Vezir Gambiti Oyunu:Muson Saldırısı",
                             "İskandinav Savunması: Kiel Varyansı",
                             "İskandinav Savunması: Mieses-Kotroc Varyansı",
                             "Sicilya Savunması",
                             "Sicilya Savunması:Najdorf,Lipnitsky Saldırısı",
                             "İskandinav Savunması: Mieses-Kotroc Varyansı"))+
  coord_flip()+
  scale_fill_manual(values=cbPalette)+
  theme(plot.title = element_text(size=20,face = "bold"),
        axis.text = element_text(size=15,face="bold"),
        axis.title= element_text(size = 20,face="bold"))


#Grouped by termination and event
chessTerminationAndEvent <- chessDataSet %>% 
  group_by(Event,Termination) %>%
  summarise(number = n()) %>%
  mutate(percent = number / sum(number))

#Changed the event and termination to turkish 
chessTerminationAndEventChanged <- within(chessTerminationAndEvent, {   
  Event[Event == "Classical"] <- "Klasik"
  Event[Event == "Classical tournament"] <- "Klasik Turnuva"
  Event[Event == "Blitz"] <- "Yıldırım"
  Event[Event == "Blitz tournament"] <- "Yıldırım Turnuvası"
  Event[Event == "Bullet"] <- "Hızlı"
  Event[Event == "Bullet tournament"] <- "Hızlı Turnuva"
  Event[Event == "Correspondence"] <- "Yazışmalı"
  Termination[Termination == "Time forfeit"] <- "Zaman Aşımı"
  Termination[Termination == "Abandoned"] <- "Çekilme"
})

#Graph for termination and event dataset
ggplot(chessTerminationAndEventChanged,aes(area=chessTerminationAndEvent$percent,
                       fill=chessTerminationAndEvent$Termination,
                       label=chessTerminationAndEvent$Termination,
                       subgroup=chessTerminationAndEvent$Event))+
  ggtitle("Oyun Türüne Göre Oyunun Sonlanma Şekli")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_treemap()+
  geom_treemap_text(colour="black",
                    place="center",
                    size=15,)+
  geom_treemap_subgroup_border(colour="white",
                               size=10)+
  geom_treemap_subgroup_text(place="center",
                             grow=TRUE,
                             alpha=0.30,
                             colour="black")+
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Purples")
```
