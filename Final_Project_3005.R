library(tidyverse)
library(haven)
library(ggplot2)
library(ggthemes)
library(stats)
library(dplyr)
library(lubridate)
library(nlme)
library(foreign)
library(ggplot2)
## 27.05.2022
## Final Project

# B - Lade die Datensätze
prices <- read.csv('1_Data/prices.csv')
sales <- read.csv('1_Data/sales.csv')
view(prices)
view(sales)

# C - Datensätze säubern
# Aus prices nur id und price übernehmen. Sonst doppelt
prices2 <- prices %>%
  select(X, AveragePrice)
prices2

class(sales$SmallMediumAvocado)

view(sales)

# Factor zu character
sales$SmallMediumAvocado <- (as.character(sales$SmallMediumAvocado))
sales$LargeAvocado <- (as.character(sales$LargeAvocado))
sales$ExtraLargeAvocado <- (as.character(sales$ExtraLargeAvocado))

class(sales$SmallMediumAvocado)

# löschen von 4046_ (Kategorie)
sales$SmallMediumAvocado <- str_remove(sales$SmallMediumAvocado, '4046_')
sales$LargeAvocado <- str_remove(sales$LargeAvocado, '4225_')
sales$ExtraLargeAvocado <- str_remove(sales$ExtraLargeAvocado, '4770_')
# konvertieren von charactor zu numeric
sales$SmallMediumAvocado <- as.numeric(sales$SmallMediumAvocado)
sales$LargeAvocado <- as.numeric(sales$LargeAvocado)
sales$ExtraLargeAvocado <- as.numeric(sales$ExtraLargeAvocado)
class(sales$ExtraLargeAvocado)

view(sales)
summary(sales)

# Rename X1 von sales zu X
sales <- sales %>%
  rename(X = X1)
view(sales)


# Join dataframes
x <- sales
x <- as.numeric(sales$Date)

df <- inner_join(sales, prices, by=c('X', 'region', 'Date', 'type'))
view(df)

# Exploration der Daten (deskriptiv)
summary(df$AveragePrice)
table(df$region, df$type)
summary(df$SmallMediumAvocado)
summary(df$LargeAvocado)
table(df$type)
length(unique(df$region))


## Preisverteilung
ggplot(data=df, mapping = aes(x=AveragePrice)) +
  geom_histogram(fill='#2874A6') +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust = 1, vjust=0.3)) +
  labs(x = "Durchschnittspreis", y = "Häufigkeit", title = "Preisverteilung") +
  scale_x_continuous(breaks = pretty(df$AveragePrice, n = 20)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank() 
  ) 
summary(df$AveragePrice)
# Preise zwischen 0.9 und 2.9 kommen am meisten vor, Preise darüber und darunter kommen selten vor 

## Verteilung der Regionen
ggplot(data=df, aes(x=region)) +
  geom_bar(fill='#2874A6') +
  labs(x = "Regionen", y = "Haeufigkeit", title = "Verteilung der Regionen") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.3))
# Alle Regionen kommen gleich oft vor. Nur WestTexNewMexico nicht ganz. Die Namen stimmen nicht ganz. Könnten wir noch ändenrn
  
# Region vs Preis
ggplot(data=df, mapping = aes(x=region, y=AveragePrice)) +
  geom_boxplot(color='#2874A6') +
  labs(x = "Region", y = "Durchschnittspreis", title = "Preise nach Regionen") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.3))
# Die drei teuersten Regionen sind: SanFrancisco, Seattle, Charlotte. In Miami gibt es Outliers in beide Richtungen. 
# Günstige Avocados gibt es in: Houston und Nashville und Phoenix 

# Drop TotalUS from region
length(unique(df$region))
df <- df[df$region != 'TotalUS', ]
length(unique(df$region))


# Regression
RegRegion <- lm(AveragePrice ~ region, data = df)
summary(RegRegion)

# Einfluss Datum auf Preise der Avocados

# Verteilung der Daten
ggplot(data=df, aes(x=Date)) +
  geom_bar(fill='#2874A6') +
  labs(x = "Datum", title = "Verteilung der Daten") +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust = 1, vjust=0.3))
# Alle Daten kommen etwa gleich oft vor.

# Datum und Preis
year <- dplyr::select(contains("Date"))

# Scatterplot mit allen Daten und Preisen
df$Date
df$Date <- as.Date(df$Date, format = "%d.%m.%y")
view(df)

# Scatterplot + Regressionslinie
ggplot(data=df, mapping = aes(x=Date, y=AveragePrice)) +
  geom_point(color='#2874A6', alpha = 0.2) +
  labs(x = "Datum", y = "Durchschnittspreis", title = "Preise nach Datum") +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust = 1, vjust=0.3)) +
  geom_smooth(method = "lm", color = "red")

# Durchschnitt nach Jahren aufteilen
years <- mutate(df, Year = factor(format(Date, "20%y")))
years_x <- group_by(years, Year) %>%
  summarise(avg = mean(AveragePrice))

view(years_x)

# Barplot als Ergänzung zu Scatterplot
ggplot(data=years_x, mapping = aes(x=Year, y=avg)) +
  geom_bar(fill='#2874A6', stat = "identity") +
  labs(x = "Jahr", y = "Durchschnittspreis", title = "Durschnittspreise nach Jahren") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0.3))

# Im Jahr 2017 sind Avocados im Vergleich mit anderen Jahren teurer

# Nach Monaten aufteilen
months <- mutate(df, Month = factor(format(Date, "%m")))
view(months)

months_x <- group_by(months, Month) %>%
  summarise(avg = mean(AveragePrice))
# Nach Monat Durchschnitte berechnet

view(months_x)

# Durchschnittspreis nach Monaten
ggplot(data=months_x, mapping = aes(x=Month, y=avg)) +
  geom_bar(fill='#2874A6', stat = "identity") +
  labs(x = "Monate", y = "Durchschnittspreis", title = "Durchschnittspreis nach Monaten") +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust = 1, vjust=0.3))

# Als Ergänzung Linechart erstellt
ggplot(data=months_x, mapping = aes(x=Month, y=avg, group = 1)) +
  geom_line(color="#2874A6", stat = "identity", size = 2) +
  labs(x = "Monate", y = "Durchschnittspreis", title = "Durchschnittspreis nach Monaten") +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust = 1, vjust=0.3))+
  expand_limits(y = 0) +
  theme(panel.grid.major.x = element_blank())
# Im Herbst sind Avocados teurer, Anfang und Ende Jahr günstiger


# Regression
RegDatum <- lm(AveragePrice ~ Date, data = df)

summary(RegDatum)

# Einfluss Typ der Avocado auf den Preis

# Verteilung Typen Avocado
table(df$type)

ggplot(data=df, mapping = aes(x=type)) +
  geom_bar(fill='#2874A6') +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust = 0.5)) +
  labs(x = "Typ", y = "Häufigkeit", title = "Typenverteilung") 
# Fast gleiche Verteilung der zwei Typen: Conventional 9126 Bestellungen, organic 9123 Bestellungen


# Vergleich Typ Avocado mit Preis
ggplot(data=df, mapping = aes(x=type, y = AveragePrice)) +
  geom_violin(fill='#2874A6', draw_quantiles = c(.25, .50, .75)) +
  theme_minimal() +
  theme(axis.text.x=element_text(hjust = 0.5)) +
  labs(x = "Typ", y = "Preis", title = "Preis nach Art") +
  expand_limits(y = 0) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank() 
  ) 

# Quantile zeigen, dass das unterste Q in organic noch höher ist, als das 75% Quantil bei conventional
# Plot zeigt, dass organic-Avocados im Durchschnitt teurer sind als conventional-Avocados
# Conventional-Avocados haben kleinere Preisstreuung als organic-Avocados

# Bestätigen mit Zahlen:
# Zwei neue Datensätze mit je einem Typen erstellen, um summary zu machen
df_con <- df %>%
  filter(type == "conventional")
mean(df_con$AveragePrice)
summary(df_con$AveragePrice)

df_org <- df %>%
  filter(type == "organic")

summary(df_org$AveragePrice)

# Zahlen bestätigen die Plots: Durchschnittspreis conventional ist 1.158, organic 1.654
# Interessant: Mindestpreis organic ist tiefer als bei conventional (0.44 vs. 0.46)
# Höchster Preis bei conventional ist mehr als ein Euro tiefer als bei organic (2.22 vs. 3.25)

# Regression
RegTyp <- lm(AveragePrice ~ type, data = df)
summary(RegTyp)

# Plot mit Datum, Typ und Region


# Alle Regionen zu unübersichtlich, darum wählen wir die folgenden sechs: SanFrancisco, Seattle, Charlotte, Houston und Nashville und Phoenix 
# Drei teuerste vs. drei günstigste Regionen, absteigend ordnen in Boxplot mit reorder()

ggplot(data=df_reg, mapping = aes(x=reorder(region, -AveragePrice), y = AveragePrice, fill = type)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 30, hjust = 1, vjust=1.3)) +
  labs(x = "Region", y = "Preis", title = "Preis und Art nach Region") +
  expand_limits(y = 0) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank() 
  ) 

# Phoenix zeigt den grössten Preisunterschied zw. conventional und organic Avocados
# In Seattle gehen die Preise für organische Avocados stark nach unten
# Houston zeigt die tiefsten Preise für organische Avocados
# Ausreisser: Überall Ausreisser ausser in SF organic und Charlotte conventional. Nashville conventional hat viele Ausreisser nach oben
# Unterschied zw conventional und organic in Phoenix mit Abstand am grössten

# Alle signifikant
# Regionen von oben
df_reg <- df%>%
  filter(region %in% c("SanFrancisco", "Seattle", "Charlotte", "Houston", "Nashville", "PhoenixTucson"))
# ANOVA
RegDatum <- lm(AveragePrice ~ Date, data = df)
anova(RegDatum)

RegRegion <- lm(AveragePrice ~ region, data = df)
anova(RegRegion)

RegTyp <- lm(AveragePrice ~ type, data = df)
anova(RegTyp)

RegMult <- lm(AveragePrice ~ type + Date + region, data = df_reg)
anova(RegMult)

# Koennen einzelne anova-Resultate der einzelnen Regressionen mit den anova-Resultaten der multiplen
# Regression vergleichen, bspw. Mean bei Type der linearen Regression viel hoeher als bei date

