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
         LVEES404,
         AGETS411,
         AGHHA403,
         AGHHA401,
         AGOWA401,
         AGWSA401,
         AGWSA410,
         AGWSA411,
         AGWSA412,
         AGWSA406,
         FIIKA401,
         FIIKA402,
         FIIKA403,
         LVPKA407,
         LVTEA402,
         LVTEA404,
         LVTEA406,
         LVPKS402,
         NGSGA401,
         NGSGA402,
         LVEES407,
         LVEES410,
         LVVKA401,
         OJWHS402,
         LFBWA401,
         LFBWA403,
         LFBWA405,
         LFALS401,
         LFALS402,
         LFALA413,
         LFALA414,
         LFALA416,
         LFRKA403,
         LFRKA407,
         LFRKA406,
         LFDGA427,
         LFDGS404,
         LFDGB403,
         LFDGB404,
         LFDGB405,
         LFDGB406,
         LFDGB407,
         LFDGB408,
         LFDGB409,
         LFDGB410,
         LFDGB411) %>%
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
        LVEES404,
        AGETS411,
        AGHHA403,
        AGHHA401,
        AGOWA401,
        AGWSA401,
        AGWSA410,
        AGWSA411,
        AGWSA412,
        AGWSA406,
        FIIKA401,
        FIIKA402,
        FIIKA403,
        LVPKA407,
        LVTEA402,
        LVTEA404,
        LVTEA406,
        LVPKS402,
        NGSGA401,
        NGSGA402,
        LVEES407,
        LVEES410,
        LVVKA401,
        OJWHS402,
        LFBWA401,
        LFBWA403,
        LFBWA405,
        LFALS401,
        LFALS402,
        LFALA413,
        LFALA414,
        LFALA416,
        LFRKA403,
        LFRKA407,
        LFRKA406,
        LFDGA427,
        LFDGS404,
        LFDGB403,
        LFDGB404,
        LFDGB405,
        LFDGB406,
        LFDGB407,
        LFDGB408,
        LFDGB409,
        LFDGB410,
        LFDGB411) %>%
  mutate(AGOJB401 = 2024) %>% # Onderzoeksjaar aanpassen
  mutate(Gemeentecode = c(rep(1:6, nrow(.) / 6), 1, 2)) %>%
  summarise_all(sample, size = 2000, replace = T) %>%
  mutate(AGWSA413 = sample(x = c(0:5, NA), size = nrow(.), replace = T),
         GZSLA401 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         GZSLA402 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         LVPDA413 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         LVPDA414 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         NGSGA405 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         NGSGA406 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         LFRKA409 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         LFRKA415 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         LFRKA413 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         LFRKA412 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         LFRKA419 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         LFRKA418 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         DISMS401 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         DIGMS401 = sample(x = c(0, 1, NA), size = nrow(.), replace = T),
         LVVKA403 = sample(x = c(0, 1, NA), size = nrow(.), replace = T)
  ) # Nieuwe variabele die alleen in 2024 uitgevraagd is toevoegen

# Gemeentenamen wijzigen: labels voor nepgemeentes toevoegen
data2024$Gemeentecode <- set_value_labels(data2024$Gemeentecode, get_value_labels(data2022$Gemeentecode))


# Data samenvoegen --------------------------------------------------------

data <- data2022 %>%
  full_join(data2024)


# meer value labels aanpassen -------------------------

#alleen nepgemeenten overhouden
nieuwe_labels <- labelled::val_labels(data$Gemeentecode) %>% head() #1e 6 codes zijn Nepgemeenten
val_labels(data$Gemeentecode) <- nieuwe_labels #oude labels overschrijven

#labels toevoegen aan jaarvariabele
data$AGOJB401 %>% 
  labelled(c('2022' = 2022, '2024' = 2024)) -> data$AGOJB401

#ook var_label toegevoegd voor alt-text
var_label(data$AGOJB401) <- "Jaar"

# Data opslaan ------------------------------------------------------------

#setwd("C:/Users/sjannes/OneDrive - VRLN/PSchijf/Gezondheidsmonitor/2024 Gezondheidsmonitor jongvolwassenen/4. Analyse/Rapportage maken")
setwd("~/gmjv-landelijk-format")
write_sav(data, "nep testdata GMJV - Regionaal trendbestand 2022-2024.sav")
