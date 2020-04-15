#### erkki 


###paketit
#install.packages("readxls")
library(readxl)

#install.packages("unikn") 
library(unikn)

#install.packages("magrittr")
library(magrittr)

#install.packages("dplyr")
library(dplyr)

###working directory
setwd("C:/Users/topia/OneDrive/Documents/muuta/erik")

#lue filut
aineenopet <- read_xlsx("alkup. tiedostot excel-tiedostoina/aineenopet.xlsx", col_names = c("ura", "kirjat"), skip=1)
luokanopet <- read_xlsx("alkup. tiedostot excel-tiedostoina/luokanopet.xlsx", col_names = c("ura", "kirjat"), skip=1)
aikanopet <- read_xlsx("alkup. tiedostot excel-tiedostoina/aikanopet.xlsx", col_names = c("ura", "kirjat"), skip=1)

#tee tiedostot taulukoista
write.csv(table(aineenopet$kirjat, aineenopet$ura), "aineenopet_taulukko.csv")
write.csv(table(luokanopet$kirjat, luokanopet$ura), "luokanopet_taulukko.csv")
write.csv(table(aikanopet$kirjat, aikanopet$ura), "aikanopet_taulukko.csv")

#väripaletti
my_pal <- seecol(pal_unikn_light)

#histogrammien piirtoon funktio
visut <- function(file, teksti) {
  counts <- table(file$kirjat, file$ura)
  if ("1" %in% row.names(counts) == FALSE) {
    counts <- rbind(counts, "1" = list(0,0,0,0))
    counts <- counts[order(row.names(counts)),]
  }
  barplot(counts, main = teksti, xlab = "Opettajana toimimisen vuodet vastausluokittain", ylab = "Vastanneet", col = my_pal, legend = rownames(counts))
}

#piirtää kaikki histogrammit
visut(aineenopet, "Aineenopettaja")
visut(luokanopet, "Luokanopettaja")
visut(aikanopet, "Äidinkielen opettaja")

#funktio työvuosittain erotteluun
vuosittain <- function(file, teksti) {
  counts <- table(file$kirjat, file$ura)
  #if ("1" %in% row.names(counts) == FALSE) {
  #  counts <- rbind(counts, "1" = list(0,0,0,0))
  #  counts <- counts[order(row.names(counts)),]
  #}
  par(mfrow=c(2,2))
  for (i in 1:4) {
    barplot(counts[,i], col = my_pal[1], xlab = "Kirjojen lukeminen vastausluokittain", ylab = "Vastanneet", main = paste0(teksti, ", työkokemus ", i))
  }
}

#piirtää histogrammit työvuosittain
vuosittain(aineenopet, "Aineenopettaja")
vuosittain(luokanopet, "Luokanopettaja")
vuosittain(aikanopet, "Äidinkielen opettaja")

###ERITELTY
aineenopet_erit <- read_xlsx("alkup. tiedostot excel-tiedostoina/aineenopet_eritelty.xlsx")
#paska data, en jaksa tidyy. Testi :D
enkku <- aineenopet_erit[,c(1,21)] %>% na.omit() %>% table()
barplot(enkku, col = my_pal[1], xlab = "Kirjojen lukeminen vastausluokittain", ylab = "Vastanneet", main = "Englanti")

#tidyyn sittenkin
colnames(aineenopet_erit) <- c(rep("aine",20), "kirjat")
aineenopet_erit_tidy <- aineenopet_erit[,c(1,21)] 
for (i in 2:19) {
  aineenopet_erit_tidy <- bind_rows(aineenopet_erit_tidy, aineenopet_erit[,c(i,21)]) %>% na.omit()
}

#legend
legenda <- tibble(c(1:19), c("Englanti", "Ruotsi", "Muu kieli", "Matematiikka", "Biologia", "Maantieto", "Fysiikka", "Kemia", "Terveystieto", "Uskonto tai ET", "Historia", "Yhteiskuntaoppi", "Musiikki", "Kuvataide", "Käsityö", "Kotitalous", "Liikunta", "Muu aine", "Suomi 2. kiel."))
colnames(legenda) <- c("aine", "aine_kokonimi")
aineenopet_erit_tidy <- inner_join(aineenopet_erit_tidy, legenda, by="aine")

#tallenna tidy
write.csv(aineenopet_erit_tidy, "aineenopet_eritelty.csv")

###testi
#png(paste0(aineenopet_erit_tidy[aineenopet_erit_tidy$aine==1,3][1,]))
#table(aineenopet_erit_tidy[,-3])[2,] %>% barplot(col = my_pal[1], xlab = "Kirjojen lukeminen vastausluokittain", ylab = "Vastanneet", main = paste0(aineenopet_erit_tidy[aineenopet_erit_tidy$aine==2,3][1,]))
#dev.off()

#piirtohommia
par(mfrow=c(1,1))

#piirtää jokaisen aineen taulun ja tallentaa sen .png-tiedostona
for (i in 1:19) {
  png(paste0(aineenopet_erit_tidy[aineenopet_erit_tidy$aine==i,3][1,],".png"))
  table(aineenopet_erit_tidy[,-3])[i,] %>% barplot(col = my_pal[1], xlab = "Kirjojen lukeminen vastausluokittain", ylab = "Vastanneet", main = paste0(aineenopet_erit_tidy[aineenopet_erit_tidy$aine==i,3][1,]))
  dev.off()
}
