# ŞAH MAT
           
Görmüş olduğunuz bu grafiklerin hepsi 2,6 Milyon oynanmış bir oyunun özetidir. Bu veri setinde oyuncuların hangi rankta oldukları, hangi açılışı yaptıkları , oyunun kaç hamlede bittiği gibi veriler yer almaktadır. Aşağıda verilmiş olan grafiklerde ilginizi çekebilecek noktalar değinmeye çalıştım

Veri Seti:
--

Grafik Çıktıları:
--
- ![alt text](https://github.com/huseyinkayar/Veri_Gorsellestirme_Final_Odevi/blob/main/Graphics/Opening%20Win%20Rate.jpeg?raw=true)

- ![alt text](https://github.com/huseyinkayar/Veri_Gorsellestirme_Final_Odevi/blob/main/Graphics/Termination-Style.jpeg?raw=true)

- ![alt text](https://github.com/huseyinkayar/Veri_Gorsellestirme_Final_Odevi/blob/main/Graphics/White-Black%20Winning%20Rate.jpeg?raw=true)

Kodlar:
--
```r
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)
#import the data set
chessDataSet <- read.csv("chess_games.csv")

#number of black wins
blackPlayersWin <- nrow(chessDataSet %>% filter(Result == "0-1"))

#number of white wins
whitePlayersWin <- nrow(chessDataSet %>% filter(Result == "1-0"))

#number of draws
draw <- nrow(chessDataSet %>% filter(Result == "1/2-1/2"))


#creating data frame for winning rates 
winrate <- data.frame(
  winner = c("Siyah","Beyaz","Beraber"),
  percent = c(blackPlayersWin/nrow(chessDataSet),
              whitePlayersWin/nrow(chessDataSet), 
              draw/nrow(chessDataSet)
  ))


#pie chart and legend for winning rates
pie(x = winrate$percent, label="", 
    col=c("grey1", "grey100", "grey65"), main="Kazanma Oranı") 
legend("right",
       legend=paste(winrate$winner, format(round(winrate$percent,2),nsmall=2), "%"),
       fill = c("grey1", "grey100", "grey65"),      
       border = "black") 


#which opening is better in winning
chessOpeningWinningRate <- chessDataSet %>% 
  filter(Result=="1-0") %>%
  group_by(Opening) %>%
  summarise(number = n())

#order the data set with descending number
chessOpeningWinningRate <- chessOpeningWinningRate[order(-chessOpeningWinningRate$number),]


#firt 20 opening
chessOpeningWinningRateRestricted <- chessOpeningWinningRate[1:20,]

#most won opening (first 20)
ggplot(chessOpeningWinningRateRestricted, aes(
  y = number, 
  x = fct_reorder(Opening,number))) + 
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
  theme_bw()

openingAndElo <- chessDataSet %>% 
  group_by(WhiteElo,Opening) %>%
  summarise(number = n())

openingAndEloRestricted <- within(openingAndElo, {   
  WhiteElo[ WhiteElo < 1500 & WhiteElo >=700 ] <- "1.Derece"
  WhiteElo[ WhiteElo < 2500 & WhiteElo >=1500] <- "2.Derece"
  WhiteElo[ WhiteElo < 3200 & WhiteElo >=2500 ] <- "3.Derece"
})

openingsDataFrame <- data.frame(chessDataSet$Opening)
openingsSample <- openingsDataFrame[sample(nrow(openingsDataFrame), 20), ]



openingAndEloRestrictedRank1 <- openingAndEloRestricted %>%
  filter(WhiteElo =="1.Derece",Opening %in% openingsSample) %>%
  group_by(WhiteElo,Opening) %>%
  summarise(number = n())

openingAndEloRestrictedRank2 <- openingAndEloRestricted %>%
  filter(WhiteElo =="2.Derece",Opening %in% openingsSample) %>%
  group_by(WhiteElo,Opening) %>%
  summarise(number = n())

openingAndEloRestrictedRank3 <- openingAndEloRestricted %>%
  filter(WhiteElo =="3.Derece",Opening %in% openingsSample) %>%
  group_by(WhiteElo,Opening) %>%
  summarise(number = n())

finalOpeningAndElo <- rbind(rbind(openingAndEloRestrictedRank1,openingAndEloRestrictedRank2),openingAndEloRestrictedRank3)


ggplot(finalOpeningAndElo, aes(fill = WhiteElo, 
                        y = number, 
                        x = Opening)) + 
  geom_bar(position = "dodge", 
           stat = "identity")+
  coord_flip()

```
