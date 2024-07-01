# Script voor het maken van testbestand GMJV.

# Auteur(s)
# Sjanne van der Stappen

# Voorbereiding -----------------------------------------------------------

# Leegmaken environment
rm(list=ls())

# Gebruikersinstellingen --------------------------------------------------

# In deze sectie kan een working directory (map) worden gedefinieerd.
# Daarnaast moeten een aantal aantal R objecten door de gebruiker worden aangemaakt.

# 1. Het vastleggen van de 'working directory', oftewel de map waarin de output zal worden opgeslagen. Let op dat bij het
# invoeren van padnamen een forward slash (/) wordt gebruikt en geen backward slash (\).
setwd("C:/Users/sjannes/OneDrive - VRLN/PSchijf/Gezondheidsmonitor/2022 Gezondheidsmonitor jongvolwassenen/4. Analyse/Data/3. Eindbestand (opgeschoond) vanuit RIVM 21 2 2023 v4")

# 2. Toevoegen van data naam.
# Voeg de naam van de data hieronder in.
data_naam <- "Regiobestand_CGMJV2022_GGD Limburg-Noord_versie 4.sav"

# Packages ----------------------------------------------------------------

# Het script maakt gebruik van een aantal packages
# Deze moeten bij de eerste keer lokaal worden geinstalleerd. 
# Dat doe je met behulp van de functie: install.packages() 
# Je kunt een package installeren door bijvoorbeeld: install.packages("tidyverse") te runnen (op een nieuwe regel)
# Je kunt ook een verzameling van packages installeren met behulp van:
# (Verwijder de # aan het begin van onderstaande regel om de code te runnen en de benodigde packages te installeren.)
# install.packages(c('tidyverse', 'haven', 'labelled'))

# Packages hoeven maar een keer geinstalleerd te worden, 
# maar packages moeten wel voor iedere nieuwe sessie worden aangeroepen met de functie: library()
# Hieronder worden de benodige packages geladen
library(tidyverse) # Meer informatie over het tidyverse is te vinden op: https://www.tidyverse.org/
library(haven) # Package Voor het laden van spss data bestanden
library(labelled) # Package om labels aan te passen

# Data inladen ------------------------------------------------------------

# Data 2022 laden, vul bij de select() functie de variabelen in voor jouw gewenste trends
data2022 <- read_spss(data_naam) %>%
  select(AGOJB401,
         Stratum,
         Standaardisatiefactor,
         AGLFA401,
         AGLFA402,
         AGGSA401,
         AGGSA402,
         MIREB401,
         GGDregio,
         Gemeentecode,
         Stedelijkheid,
         AGETS414,
         AGOWS401,
         AGOWS402,
         AGOWS403,
         AGOWS404,
         GZGGA402,
         LVPKS403,
         LVTEA401,
         LVTEA403,
         LVTEA405,
         LVSTA415,
         LVVTA404,
         LVVTA405,
         LVVTA406,
         LVVTA407,
         LVVTA408,
         LVVTA409,
         LVVTA410,
         LVEES402,
         LVEES403,
         LVEES404) %>%
  summarise_all(sample, size = nrow(.), replace = F) # 2022 data door elkaar mixen om herleidbaarheid weg te halen

# Gemeentenamen wijzigen: labels voor nepgemeentes toevoegen
data2022$Gemeentecode %>%
  set_value_labels(NepgemeenteA = 1,
                   NepgemeenteB = 2,
                   NepgemeenteC = 3,
                   NepgemeenteD = 4,
                   NepgemeenteE = 5,
                   NepgemeenteF = 6,
                   get_value_labels(data2022$Gemeentecode)) -> data2022$Gemeentecode

# Gemeentenamen wijzigen: gemeentecodes vervangen
data2022$Gemeentecode <- car::recode(var = data2022$Gemeentecode,
            recodes = "889 = 1; 893 = 1; 907 = 2; 944 = 2; 946 = 3; 957 = 3;
            983 = 4; 984 = 4; 988 = 5; 1507 = 5; 1640 = 6; 1641 = 6; 1669 = 1;
            1711 = 2; 1894 = 3")

# Nep-data 2024 genereren en toevoegen
data2024 <- read_spss(data_naam) %>%
  select(#AGOJB401,
         Standaardisatiefactor,
         Stratum,
         AGLFA401,
         AGLFA402,
         AGGSA401,
         AGGSA402,
         MIREB401,
         GGDregio,
         Stedelijkheid,
         AGETS414,
         AGOWS401,
         AGOWS402,
         AGOWS403,
         AGOWS404,
         GZGGA402,
         LVPKS403,
         LVTEA401,
         LVTEA403,
         LVTEA405,
         LVSTA415,
         LVVTA404,
         LVVTA405,
         LVVTA406,
         LVVTA407,
         LVVTA408,
         LVVTA409,
         LVVTA410,
         LVEES402,
         LVEES403,
         LVEES404) %>%
  mutate(AGOJB401 = 2024) %>% # Onderzoeksjaar aanpassen
  mutate(Gemeentecode = c(rep(1:6, nrow(.) / 6), 1, 2)) %>%
  summarise_all(sample, size = 2000, replace = T) %>%
  mutate(NIEUS101 = sample(x = c(0, 1, NA), size = nrow(.), replace = T)) # Nieuwe variabele die alleen in 2024 uitgevraagd is toevoegen

# Gemeentenamen wijzigen: labels voor nepgemeentes toevoegen
data2024$Gemeentecode <- set_value_labels(data2024$Gemeentecode, get_value_labels(data2022$Gemeentecode))

# Data samenvoegen --------------------------------------------------------

data <- data2022 %>%
  full_join(data2024)

# Data opslaan ------------------------------------------------------------

#setwd("C:/Users/sjannes/OneDrive - VRLN/PSchijf/Gezondheidsmonitor/2024 Gezondheidsmonitor jongvolwassenen/4. Analyse/Rapportage maken")
setwd("~/gmjv-landelijk-format")
write_sav(data, "nep testdata GMJV - Regionaal trendbestand 2022-2024.sav")
