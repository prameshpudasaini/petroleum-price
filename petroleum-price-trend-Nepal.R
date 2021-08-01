rm(list = ls())
graphics.off()

# install.packages("pacman")

pacman::p_load(ggplot2, 
               dplyr, 
               tidyr,
               stringr, 
               patchwork,
               tidyverse,
               purrr,
               data.table,
               lubridate,
               googlesheets)


## ====================================== ##
## DATA: NOCL PRICE OF PETROLEUM PRODUCTS ##
## ====================================== ##

# read data
data_nocl <- read.csv(file = "./nocl_price_archive.csv", header = T,
                      stringsAsFactors = F)

# some data wrangling
names(data_nocl)
data_nocl <- data_nocl %>% 
  rename(Date = Effective.date.1,
         ATF_DP = ATF..DP.,
         ATF_DF = ATF..DF.) %>% 
  select(Date, Petrol, Diesel, Kerosene, LPG, ATF_DP, ATF_DF) %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(","))) %>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed("$"))) %>%
  mutate(Date = parse_date_time(Date, orders = c("mdy", "dmy", "ymd"), 
                                tz = "")) %>% 
  mutate(Date = as.Date(Date, "%Y-%m-%d", tz = ""),
         Petrol = as.numeric(Petrol),
         Diesel = as.numeric(Diesel),
         Kerosene = as.numeric(Kerosene),
         LPG = as.numeric(LPG),
         ATF_DP = as.numeric(ATF_DP),
         ATF_DF = as.numeric(ATF_DF))

# add new observations
names(data_nocl)
data_nocl <- data_nocl %>% add_row(Date = as.Date("2021-04-01"),
                                   Petrol = 120,
                                   Diesel = 103,
                                   Kerosene = 103)
data_nocl <- data_nocl %>% add_row(Date = as.Date("2021-04-16"),
                                   Petrol = 121,
                                   Diesel = 104,
                                   Kerosene = 104,
                                   ATF_DP = 76)
data_nocl <- data_nocl %>% add_row(Date = as.Date("2021-05-16"),
                                   Petrol = 123,
                                   Diesel = 106,
                                   Kerosene = 106,
                                   ATF_DP = 78)
data_nocl <- data_nocl %>% add_row(Date = as.Date("2021-06-01"),
                                   Petrol = 125,
                                   Diesel = 108,
                                   Kerosene = 108,
                                   ATF_DP = 80)
data_nocl <- data_nocl %>% add_row(Date = as.Date())

# save data with new observations
save("data_nocl", file = "./data/data_nocl_2021_04_16.RData")




## =========================== ##
## DATA: BRENT CRUDE OIL PRICE ##
## =========================== ##

# data source
browseURL(url = "https://www.investing.com/commodities/brent-oil-historical-data")

# read data
data_bco1 <- read.csv(file = "./brent_crude_oil_price_2000_2010.csv", 
                      header = T, stringsAsFactors = F)
data_bco2 <- read.csv(file = "./brent_crude_oil_price_2011_2020.csv", 
                      header = T, stringsAsFactors = F)
data_bco3 <- read.csv(file = "./brent_crude_oil_price_2021_2021.csv", 
                      header = T, stringsAsFactors = F)
data_bco <- bind_rows(data_bco1, data_bco2, data_bco3)

# some data wrangling
names(data_bco)
data_bco <- data_bco %>% 
  rename(Date = ï..Date,
         Brent = Price) %>% 
  select(Date, Brent) %>% 
  mutate(Date = parse_date_time(Date, orders = c("mdy", "dmy", "ymd"), 
                                tz = "")) %>% 
  arrange(desc(Date)) %>% 
  mutate(Date = as.Date(Date, "%Y-%m-%d", tz = ""),
         Brent = as.numeric(Brent))

# save data
save("data_bco", file = "./data/data_bco_2021_04_21.RData")




## =========================== ##
## DATA: USD-NRS EXCHANGE RATE ##
## =========================== ##

# data source
browseURL(url = "https://www.investing.com/currencies/usd-npr-historical-data")

# read data
data_exch <- read.csv(file = "./exchange_rate_usd_nrs.csv", header = T,
                      stringsAsFactors = F)

# some data wrangling
names(data_exch)
data_exch <- data_exch %>% 
  rename(Date = ï..Date,
         USD = Price) %>% 
  select(Date, USD) %>% 
  mutate(Date = parse_date_time(Date, orders = c("mdy", "dmy", "ymd"), 
                                tz = "")) %>% 
  arrange(desc(Date)) %>% 
  mutate(Date = as.Date(Date, "%Y-%m-%d", tz = ""),
         USD = as.numeric(USD))

# save data
save("data_exch", file = "./data/data_exch_2021_04_22.RData")




## =================================================== ##
## MERGE DATA FRAMES AND CREATE INDIVIDUAL DATA FRAMES ##
## =================================================== ##

load(file = "./data/data_nocl_2021_04_16.RData")
load(file = "./data/data_bco_2021_04_21.RData")
load(file = "./data/data_exch_2021_04_22.RData")

# full_join and left_join data frames
df_all <- list(data_nocl, data_bco, data_exch) %>% 
  reduce(full_join, by = "Date")
df_merge <- list(data_nocl, data_bco, data_exch) %>% 
  reduce(left_join, by = "Date")

# add decade labels in BS and year in AD
df_all <- df_all %>% 
  mutate(Decade = 
           case_when(Date >= "1973-04-13" & Date < "1983-04-14" ~ "2030s", 
                     Date >= "1983-04-14" & Date < "1993-04-13" ~ "2040s",
                     Date >= "1993-04-13" & Date < "2003-04-14" ~ "2050s",
                     Date >= "2003-04-14" & Date < "2013-04-14" ~ "2060s",
                     Date >= "2013-04-14" & Date < "2023-04-14" ~ "2070s")) %>% 
  mutate(Year = lubridate::year(Date), .before = Date)

df_merge <- df_merge %>% 
  mutate(Decade = 
           case_when(Date >= "1973-04-13" & Date < "1983-04-14" ~ "2030s", 
                     Date >= "1983-04-14" & Date < "1993-04-13" ~ "2040s",
                     Date >= "1993-04-13" & Date < "2003-04-14" ~ "2050s",
                     Date >= "2003-04-14" & Date < "2013-04-14" ~ "2060s",
                     Date >= "2013-04-14" & Date < "2023-04-14" ~ "2070s")) %>% 
  mutate(Year = lubridate::year(Date), .before = Date)

df_diesel <- df_merge %>% 
  select(Year, Date, Diesel, Decade) %>% 
  filter(!is.na(Diesel))
df_petrol <- df_merge %>% 
  select(Year, Date, Petrol, Decade) %>% 
  filter(!is.na(Petrol))
df_kerosene <- df_merge %>% 
  select(Year, Date, Kerosene, Decade) %>% 
  filter(!is.na(Kerosene))
df_lpg <- df_merge %>% 
  select(Year, Date, LPG, Decade) %>% 
  filter(!is.na(LPG))
df_atfdp <- df_merge %>% 
  select(Year, Date, ATF_DP, Decade) %>% 
  filter(!is.na(ATF_DP))
df_atfdf <- df_merge %>% 
  select(Year, Date, ATF_DF, Decade) %>% 
  filter(!is.na(ATF_DF))

df_brent <- df_all %>% 
  select(Year, Date, Brent, Decade) %>% 
  filter(!is.na(Brent))

df_exch <- df_all %>% 
  select(Year, Date, USD, Decade) %>% 
  filter(!is.na(USD))

# save data frames
save(list = c("df_all", "df_merge", "df_diesel", "df_petrol", "df_kerosene",
              "df_lpg", "df_atfdp", "df_atfdf", "df_brent", "df_exch"),
     file = "./data/data_frames.RData")

load(file = "./data/data_frames.RData")




## ========================================================== ##
## REGRESSION: PRODUCT PRICE, CRUDE OIL PRICE & EXCHANGE RATE ##
## ========================================================== ##

dbe_fit <- lm(data = df_all, formula = Diesel ~ Brent + USD)
summary(dbe_fit)

predicted_diesel <- data.frame(diesel_pred = predict(dbe_fit, df_all), 
                               y = df_all$Brent)


## =================================================================== ##
## PLOT: HISTORICAL TREND OF DIESEL PRICE, BRENT PRICE & EXCHANGE RATE ##
## =================================================================== ##

df_dbe <- df_all %>% 
  select(Year, Date, Diesel, Brent, USD, Decade) %>% 
  pivot_longer(cols = c("Diesel", "Brent", "USD"), 
               names_to = "Measure",
               values_to = "Price") %>% 
  filter(!is.na(Price)) %>% 
  filter(Date >= "2000-01-01")

df_diesel_2000 <- df_diesel %>% 
  filter(Date >= "2000-01-01")

ggplot(df_dbe) + 
  geom_line(aes(x = Date, y = Price, color = Measure)) + 
  geom_line(data = df_diesel_2000, aes(x = Date, y = Diesel),
            color = "blue", size = 0.8) + 
  geom_line(data = predicted_diesel, aes(x = diesel_pred, y = y), 
            color = "red") +
  geom_rug(data = df_diesel_2000, aes(x = Date), outside = F, sides = "t",
           color = "blue", length = unit(8, "pt"), alpha = 2/3) + 
  scale_color_manual(labels = c("Brent crude oil price (USD per barrel)",
                                "Diesel price (NRs per liter)",
                                "Exchange rate (NRs per USD)"),
                     values = c("grey70", "blue", "limegreen")) + 
  theme_bw()

geom_line(color='red',data = predicted_df, aes(x=mpg_pred, y=hp))

geom_abline(slope = coef(data.lm)[[2]], intercept = coef(data.lm)[[1]])

?predict

pl_dbu <- ggplot() + 
  geom_line(data = df_plot_dbu, aes(x = Date, y = Price, color = Measure),
            size = 0.7) + 
  geom_point(data = df_plot_d, aes(x = Date, y = Price),
             color = "black", size = 0.9, alpha = 2/3) + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = 0, ymax = 160,
                              fill = label),
            alpha = 1/5, show.legend = F) +
  geom_label(data = decade.labels, aes(x = date, y = Price, label = label), 
             color = "black", size = 5) + 
  geom_rug(data = df_plot_d, aes(x = Date), outside = F, sides = "t", 
           length = unit(10, "pt"), color = "grey20", alpha = 1/2) + 
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) + 
  guides(shape = F) + 
  xlab("Year") + 
  ylab("Price") + 
  ggtitle("Historical Trend of Diesel Price, Brent Crude Oil Price & Exchange Rate",
          subtitle = "(Created by: Pramesh Pudasaini)") + 
  theme_bw()



## -------------------------------- ##
## MORE DATA WRANGLING FOR PLOTTING ##
## -------------------------------- ##


load(file = "./data/data_raw_wrangled.RData")

# create data frames for plotting products price, brent & exchange rate trend

df_dbe <- df_all %>% 
  select(Year, Date, Diesel, Brent, USD, Decade) %>% 
  pivot_longer(cols = c("Diesel", "Brent", "USD"), 
               names_to = "Measure",
               values_to = "Price") %>% 
  filter(!is.na(Price))

df_brent <- df_all %>% 
  select(Year, Date, Brent, Decade) %>% 
  filter(!is.na(Brent))

df_exch <- df_all %>% 
  select(Year, Date, USD, Decade) %>% 
  filter(!is.na(USD))


## -------------------------------- ##
## DEFINE PLOT GEOMS AND PARAMETERS ##
## -------------------------------- ##


load(file = "./data/data_plotting.RData")

# define geom_rects for different plots

# 2030 baishakh 1 - 1973 April 13
# 2040 baishakh 1 - 1983 April 14
# 2050 baishakh 1 - 1993 April 13
# 2060 baishakh 1 - 2003 April 14
# 2070 baishakh 1 - 2013 April 14

df_diesel$Decade[1]
df_petrol$Decade[1]
df_kerosene$Decade[1]
df_lpg$Decade[1]
df_atfdp$Decade[1]
df_atfdf$Decade[1]

date_start_2030 <- as.Date(df_atfdp$Date[1] - as.difftime(365, unit = "days"))
date_start_2040 <- as.Date(df_diesel$Date[1] - as.difftime(365, unit = "days"))
date_start_2050 <- as.Date(df_lpg$Date[1] - as.difftime(365, unit = "days"))
date_end <- as.Date(last(df_diesel$Date) + as.difftime(365, unit = "days"))

# rect 2030s applies to ATF_DP & ATF_DF
rects_2030 <- data.frame(xstart = c(date_start_2030, 
                                    as.Date("1983-04-14"),
                                    as.Date("1993-04-13"), 
                                    as.Date("2003-04-14"),
                                    as.Date("2013-04-14")),
                         xend = c(as.Date("1983-04-13"),
                                  as.Date("1993-04-12"),
                                  as.Date("2003-04-13"),
                                  as.Date("2013-04-13"),
                                  date_end),
                         label = c("2030s", "2040s", "2050s", "2060s", "2070s"))

# rect 2040s applies to combined plot, Diesel, Petrol & Kerosene
rects_2040 <- data.frame(xstart = c(date_start_2040,
                                    as.Date("1993-04-13"), 
                                    as.Date("2003-04-14"),
                                    as.Date("2013-04-14")),
                         xend = c(as.Date("1993-04-12"),
                                  as.Date("2003-04-13"),
                                  as.Date("2013-04-13"),
                                  date_end),
                         label = c("2040s", "2050s", "2060s", "2070s"))

# rect 2050s applies to LPG
rects_2050 <- data.frame(xstart = c(date_start_2050,
                                    as.Date("2003-04-14"),
                                    as.Date("2013-04-14")),
                         xend = c(as.Date("2003-04-13"),
                                  as.Date("2013-04-13"),
                                  date_end),
                         label = c("2050s", "2060s", "2070s"))

# define decade labels for different geom_rect()

dec_lab_2030 <- data.frame(date = c(as.Date("1979-04-13"),
                                    as.Date("1988-04-13"),
                                    as.Date("1998-04-13"),
                                    as.Date("2008-04-13"),
                                    as.Date("2017-04-13")),
                           label = c("2030s", "2040s", "2050s", "2060s", 
                                     "2070s"))
dec_lab_2040 <- data.frame(date = c(as.Date("1989-04-13"),
                                    as.Date("1998-04-13"),
                                    as.Date("2008-04-13"),
                                    as.Date("2018-04-13")),
                           label = c("2040s", "2050s", "2060s", "2070s"))
dec_lab_2050 <- data.frame(date = c(as.Date("1999-04-13"),
                                    as.Date("2008-04-13"),
                                    as.Date("2018-04-13")),
                           label = c("2050s", "2060s", "2070s"))

# set y scale for individual product plots

round_any = function(x, accuracy, f = round){f(x / accuracy) * accuracy}

ymin.diesel <- max(0, round_any(min(df_diesel$Diesel) - 10, 20, f = floor))
ymax.diesel <- round_any(max(df_diesel$Diesel) + 10, 20, f = ceiling)

ymin.petrol <- max(0, round_any(min(df_petrol$Petrol) - 10, 20, f = floor))
ymax.petrol <- round_any(max(df_petrol$Petrol) + 10, 20, f = ceiling)

ymin.kerosene <- max(0, round_any(min(df_kerosene$Kerosene) - 10, 20, f = floor))
ymax.kerosene <- round_any(max(df_kerosene$Kerosene) + 10, 20, f = ceiling)

ymin.lpg <- max(0, round_any(min(df_lpg$LPG) - 50, 50, f = floor))
ymax.lpg <- round_any(max(df_lpg$LPG) + 50, 50, f = ceiling)

ymin.atfdp <- max(0, round_any(min(df_atfdp$ATF_DP) - 10, 20, f = floor))
ymax.atfdp <- round_any(max(df_atfdp$ATF_DP) + 10, 20, f = ceiling)

ymin.atfdf <- max(0, round_any(min(df_atfdf$ATF_DF) - 100, 100, f = floor))
ymax.atfdf <- round_any(max(df_atfdf$ATF_DF) + 100, 100, f = ceiling)




## --------------------------- ##
## CUSTOM THEMEs FOR ALL PLOTS ##
## --------------------------- ##


TH <- theme_bw()

th1 <- theme(axis.title = element_text(size = 12),
             axis.text = element_text(size = 10),
             axis.text.x = element_text(margin = margin(t=4, r=0, b=2, l=2,
                                                        unit = "pt"), 
                                        hjust = 0.65),
             axis.text.y = element_text(margin = margin(t=0, r=4, b=2, l=0,
                                                        unit = "pt")),
             axis.ticks = element_line(color = "black"),
             axis.ticks.length = unit(2, "pt"),
             plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
             plot.subtitle = element_text(size = 11, face = "italic", 
                                          hjust = 0.5),
             plot.caption = element_text(size = 10, vjust = 1),
             plot.margin = margin(t=2, r=8, b=2, l=2, unit = "pt"),
             panel.border = element_rect(color = "black", fill = NA, size = 1.3),
             legend.position = c(0.2, 0.85),
             legend.title = element_blank(),
             legend.text = element_text(size = 12),
             legend.key = element_rect(fill = "white", color = "white"), 
             legend.key.width = unit(25, "pt"), 
             legend.key.size = unit(1.5, "line"),
             legend.background = element_rect(fill = "white", size = 0.5, 
                                              linetype = "solid", 
                                              color = "black"), 
             strip.background = element_rect(color = "black", fill = "grey80", 
                                             size = 1.3),
             strip.text = element_text(size = 13, face = "bold"))

# for ghost-facetted individual plots

th2 <- theme(axis.title = element_text(size = 12),
             axis.text = element_text(size = 10),
             axis.text.x = element_text(margin = margin(t=4, r=0, b=2, l=2,
                                                        unit = "pt"), 
                                        hjust = 0.65),
             axis.text.y = element_text(margin = margin(t=0, r=4, b=2, l=0,
                                                        unit = "pt")),
             axis.ticks = element_line(color = "black"),
             axis.ticks.length = unit(2, "pt"),
             plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
             plot.subtitle = element_text(size = 11, face = "italic", 
                                          hjust = 0.5),
             plot.caption = element_text(size = 10, vjust = 1),
             plot.margin = margin(t=2, r=8, b=2, l=2, unit = "pt"),
             panel.border = element_rect(color = "black", fill = NA, size = 1.3),
             panel.grid.minor = element_blank(),
             legend.position = c(0.2, 0.85),
             legend.title = element_blank(),
             legend.text = element_text(size = 12),
             legend.key = element_rect(fill = "white", color = "white"), 
             legend.key.width = unit(25, "pt"), 
             legend.key.size = unit(1.5, "line"),
             legend.background = element_rect(fill = "white", size = 0.5, 
                                              linetype = "solid", 
                                              color = "black"), 
             strip.background = element_rect(color = "black", fill = "grey80", 
                                             size = 1.3),
             strip.text = element_text(size = 13, face = "bold"))


## ----------------------------------------------------------- ##
## PLOT: HISTORICAL TREND OF PETROLEUM PRODUCTS PRICE IN NEPAL ##
## ----------------------------------------------------------- ##


line.size <- 0.5

# 1. DIESEL

df_diesel$title <- "Diesel"

p.diesel <- ggplot(df_diesel) + 
  geom_line(aes(x = Date, y = Diesel), color = "blue", size = line.size) + 
  geom_rug(aes(x = Date), outside = F, sides = "t", size = 0.25, 
           length = unit(6, "pt"), color = "black", alpha = 1) + 
  facet_grid(. ~ title) + 
  geom_rect(data = rects_2040, aes(xmin = xstart, xmax = xend, 
                                   ymin = ymin.diesel, ymax = ymax.diesel,
                                   fill = label),
            alpha = 1/5, show.legend = F) + 
  geom_label(data = dec_lab_2040, aes(x = date, y = c(40, 40, 20, 20), 
                                      label = label),
             color = "black", size = 3.5) + 
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(ymin.diesel, ymax.diesel, 20),
                     limits = c(ymin.diesel, ymax.diesel), expand = c(0, 0)) + 
  scale_fill_manual(values = c("grey60", "grey90", "grey60", "grey90")) + 
  xlab("Year") + 
  ylab("Price (Rs per liter)")

# 2. PETROL

df_petrol$title <- "Petrol"

p.petrol <- ggplot(df_petrol) + 
  geom_line(aes(x = Date, y = Petrol), color = "red", size = line.size) + 
  geom_rug(aes(x = Date), outside = F, sides = "t", size = 0.25, 
           length = unit(6, "pt"), color = "black", alpha = 1) + 
  facet_grid(. ~ title) + 
  geom_rect(data = rects_2040, aes(xmin = xstart, xmax = xend, 
                                   ymin = ymin.petrol, ymax = ymax.petrol,
                                   fill = label),
            alpha = 1/5, show.legend = F) + 
  geom_label(data = dec_lab_2040, aes(x = date, y = c(40, 20, 20, 20), 
                                      label = label),
             color = "black", size = 3.5) + 
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(ymin.petrol, ymax.petrol, 20),
                     limits = c(ymin.petrol, ymax.petrol), expand = c(0, 0)) + 
  scale_fill_manual(values = c("grey60", "grey90", "grey60", "grey90")) + 
  xlab("Year") + 
  ylab("Price (Rs per liter)")

# 3. KEROSENE

df_kerosene$title = "Kerosene"

p.kerosene <- ggplot(df_kerosene) + 
  geom_line(aes(x = Date, y = Kerosene), color = "grey20", size = line.size) + 
  geom_rug(aes(x = Date), outside = F, sides = "t", size = 0.25, 
           length = unit(6, "pt"), color = "black", alpha = 1) + 
  facet_grid(. ~ title) + 
  geom_rect(data = rects_2040, aes(xmin = xstart, xmax = xend, 
                                   ymin = ymin.kerosene, ymax = ymax.kerosene,
                                   fill = label),
            alpha = 1/5, show.legend = F) + 
  geom_label(data = dec_lab_2040, aes(x = date, y = c(40, 40, 20, 20), 
                                      label = label),
             color = "black", size = 3.5) + 
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(ymin.kerosene, ymax.kerosene, 20),
                     limits = c(ymin.kerosene, ymax.kerosene), expand = c(0, 0)) + 
  scale_fill_manual(values = c("grey60", "grey90", "grey60", "grey90")) + 
  xlab("Year") + 
  ylab("Price (Rs per liter)")

# 4. LP GAS

df_lpg$title = "LP Gas"

p.lpg <- ggplot(df_lpg) + 
  geom_line(aes(x = Date, y = LPG), color = "darkgreen", size = line.size) + 
  geom_rug(aes(x = Date), outside = F, sides = "t", size = 0.25, 
           length = unit(6, "pt"), color = "black", alpha = 1) + 
  facet_grid(. ~ title) + 
  geom_rect(data = rects_2050, aes(xmin = xstart, xmax = xend, 
                                   ymin = ymin.lpg, ymax = ymax.lpg,
                                   fill = label),
            alpha = 1/5, show.legend = F) + 
  geom_label(data = dec_lab_2050, aes(x = date, y = c(800, 500, 500), 
                                      label = label),
             color = "black", size = 3.5) + 
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(ymin.lpg, ymax.lpg, 200),
                     limits = c(ymin.lpg, ymax.lpg), expand = c(0, 0)) + 
  scale_fill_manual(values = c("grey60", "grey90", "grey60", "grey90")) + 
  xlab("Year") + 
  ylab("Price (Rs per cylinder)")

# 5. ATF (DUTY PAID)

df_atfdp$title = "ATF (Duty Paid)"

p.atfdp <- ggplot(df_atfdp) + 
  geom_line(aes(x = Date, y = ATF_DP), color = "darkmagenta", size = line.size) + 
  geom_rug(aes(x = Date), outside = F, sides = "t", size = 0.25, 
           length = unit(6, "pt"), color = "black", alpha = 1) + 
  facet_grid(. ~ title) + 
  geom_rect(data = rects_2030, aes(xmin = xstart, xmax = xend, 
                                   ymin = ymin.atfdp, ymax = ymax.atfdp,
                                   fill = label),
            alpha = 1/5, show.legend = F) + 
  geom_label(data = dec_lab_2030, aes(x = date, y = c(80, 80, 80, 20, 20), 
                                      label = label),
             color = "black", size = 3.5) + 
  scale_x_date(date_breaks = "6 years", date_labels = "%Y", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(ymin.atfdp, ymax.atfdp, 40),
                     limits = c(ymin.atfdp, ymax.atfdp), expand = c(0, 0)) + 
  scale_fill_manual(values = c("grey60", "grey90", "grey60", "grey90", 
                               "grey60")) + 
  xlab("Year") + 
  ylab("Price (Rs per liter)")

# 6. ATF (DUTY FREE)

df_atfdf$title = "ATF (Duty Free)"

p.atfdf <- ggplot(df_atfdf) + 
  geom_line(aes(x = Date, y = ATF_DF), color = "darkorange2", size = line.size) + 
  geom_rug(aes(x = Date), outside = F, sides = "t", size = 0.25, 
           length = unit(6, "pt"), color = "black", alpha = 1) + 
  facet_grid(. ~ title) + 
  geom_rect(data = rects_2030, aes(xmin = xstart, xmax = xend, 
                                   ymin = ymin.atfdf, ymax = ymax.atfdf,
                                   fill = label),
            alpha = 1/5, show.legend = F) + 
  geom_label(data = dec_lab_2030, aes(x = date, y = c(800, 800, 800, 400, 400), 
                                      label = label),
             color = "black", size = 3.5) + 
  scale_x_date(date_breaks = "6 years", date_labels = "%Y", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(ymin.atfdf, ymax.atfdf, 400),
                     limits = c(ymin.atfdf, ymax.atfdf), expand = c(0, 0)) + 
  scale_fill_manual(values = c("grey60", "grey90", "grey60", "grey90", 
                               "grey60")) + 
  xlab("Year") + 
  ylab("Price (USD per 1000 liter)")

# all 6 plots

plot.diesel <- p.diesel + TH + th2
plot.petrol <- p.petrol + TH + th2
plot.kerosene <- p.kerosene + TH + th2
plot.lpg <- p.lpg + TH + th2
plot.atfdp <- p.atfdp + TH + th2
plot.atfdf <- p.atfdf + TH + th2

plot.diesel
plot.petrol
plot.kerosene
plot.lpg
plot.atfdp
plot.atfdf

(plot.diesel | plot.petrol | plot.kerosene) / 
  (plot.lpg | plot.atfdp | plot.atfdf)

plot.all <- (plot.diesel | plot.petrol | plot.kerosene) / 
  (plot.lpg | plot.atfdp | plot.atfdf) + 
  plot_annotation(title = "Historical Trend of Petroleum Products Price in Nepal", 
                  subtitle = "Pramesh Pudasaini", 
                  theme = theme(plot.title = element_text(size = 18, 
                                                          face = "bold", 
                                                          hjust = 0.5),
                                plot.subtitle = element_text(size = 11, 
                                                             face = "italic",
                                                             hjust = 0.5)))
plot.all

ggsave(filename = "./figures/historical_trend_petroleum_products_price_nepal_april_2021.png",
       plot = plot.all, units = "cm", width = 29.7, height = 21, dpi = 600)




## ------------------------------------------------------------------- ##
## PLOT: HISTORICAL TREND OF DIESEL PRICE, BRENT PRICE & EXCHANGE RATE ##
## ------------------------------------------------------------------- ##

names(df_dbe)

df_dbe2000 <- df_dbe %>% 
  filter(Date >= "2000-01-01")

ggplot(df_dbe) + 
  geom_line(aes(x = Date, y = Price, color = Measure)) + 
  geom_line(data = df_diesel, aes(x = Date, y = Diesel), 
            color = "blue", size = 0.8) +
  geom_rug(data = df_diesel, aes(x = Date), outside = F, sides = "t", 
           color = "blue", length = unit(10, "pt"), alpha = 1) + 
  # geom_rug(data = df_brent, aes(x = Date), outside = F, sides = "t", 
  #          color = "grey70", length = unit(15, "pt"), alpha = 1) + 
  # geom_rug(data = df_exch, aes(x = Date), outside = F, sides = "t", 
  #          color = "limegreen", length = unit(10, "pt"), alpha = 1) + 
  scale_color_manual(labels = c("Brent crude oil price (USD per barrel)", 
                                "Diesel price (NRs per liter)",
                                "Exchange rate (NRs per USD)"),
                     values = c("grey70", "blue", "limegreen"))

pl_dbu <- ggplot() + 
  geom_line(data = df_plot_dbu, aes(x = Date, y = Price, color = Measure),
            size = 0.7) + 
  geom_point(data = df_plot_d, aes(x = Date, y = Price),
             color = "black", size = 0.9, alpha = 2/3) + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = 0, ymax = 160,
                              fill = label),
            alpha = 1/5, show.legend = F) +
  geom_label(data = decade.labels, aes(x = date, y = Price, label = label), 
             color = "black", size = 5) + 
  geom_rug(data = df_plot_d, aes(x = Date), outside = F, sides = "t", 
           length = unit(10, "pt"), color = "grey20", alpha = 1/2) + 
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) + 
  guides(shape = F) + 
  xlab("Year") + 
  ylab("Price") + 
  ggtitle("Historical Trend of Diesel Price, Brent Crude Oil Price & Exchange Rate",
          subtitle = "(Created by: Pramesh Pudasaini)") + 
  theme_bw()

names(data_nocl)
df_nocl <- data_nocl %>% 
  pivot_longer(cols = c("Petrol", "Diesel", "Kerosene", "LPG", "ATF_DP", 
                        "ATF_DF"), 
               names_to = "Product", values_to = "Price") %>% 
  filter(!is.na(Price)) %>% 
  mutate(Decade = 
           case_when(Date >= "1973-04-13" & Date < "1983-04-14" ~ "2030s", 
                     Date >= "1983-04-14" & Date < "1993-04-13" ~ "2040s",
                     Date >= "1993-04-13" & Date < "2003-04-14" ~ "2050s",
                     Date >= "2003-04-14" & Date < "2013-04-14" ~ "2060s",
                     Date >= "2013-04-14" & Date < "2023-04-14" ~ "2070s"))

# add decade labels in BS for geom_rect()

df_nocl$Product <- factor(df_nocl$Product, 
                          levels = c("Diesel", "Petrol", "Kerosene",
                                     "LPG", "ATF_DF", "ATF_DP"))

# plot Price trend of products for NOCL data
ggplot() + 
  geom_line(data = df_nocl, aes(x = Date, y = Price, color = Product)) + 
  scale_color_manual(values = c("deepskyblue", "darkorange", "blue", "grey20", 
                                "limegreen", "red"))

ggplot(df_nocl) + 
  geom_line(aes(x = Date, y = Price)) + 
  facet_wrap(~Product, scales = "free") 

ggplot(subset(df_nocl, Product %in% "Diesel")) + 
  geom_line(aes(x = Date, y = Price), color = "blue")

ggplot()

ggplot(subset(df, dose %in% c("D0.5", "D1")), aes(x = dose, y = len))+
  geom_col(aes(fill = supp), width = 0.7) +
  scale_fill_viridis_d()

# select data frame for plotting
df_plot <- df_merge %>% 
  select(Date, Petrol, Diesel, Brent, USD)

df_plot <- df_plot %>% 
  mutate(Decade = 
           case_when(Date >= "1973-04-13" & Date < "1983-04-14" ~ "2030s", 
                     Date >= "1983-04-14" & Date < "1993-04-13" ~ "2040s",
                     Date >= "1993-04-13" & Date < "2003-04-14" ~ "2050s",
                     Date >= "2003-04-14" & Date < "2013-04-14" ~ "2060s",
                     Date >= "2013-04-14" & Date < "2023-04-14" ~ "2070s"))

df_plot_pd <- df_plot %>% 
  select(Date, Petrol, Diesel, Decade)
df_plot_dbu <- df_plot %>% 
  select(Date, Diesel, Brent, USD, Decade)
df_plot_d <- df_plot %>% 
  select(Date, Diesel, Decade)

# convert to long format
# omit prices with NA values
df_plot <- df_plot %>% 
  pivot_longer(cols = c("Petrol", "Diesel", "Brent", "USD"),
               names_to = "Measure", values_to = "Price") %>% 
  filter(!is.na(Price))

df_plot_pd <- df_plot_pd %>% 
  pivot_longer(cols = c("Petrol", "Diesel"),
               names_to = "Measure", values_to = "Price") %>% 
  filter(!is.na(Price))

df_plot_dbu <- df_plot_dbu %>% 
  pivot_longer(cols = c("Diesel", "Brent", "USD"),
               names_to = "Measure", values_to = "Price") %>% 
  filter(!is.na(Price))

df_plot_d <- df_plot_d %>% 
  rename(Price = Diesel) %>% 
  filter(!is.na(Price)) %>% 
  filter(!is.na(Decade))

# add decade labels in BS for geom_rect()

decade.labels <- data.frame(date = c(as.Date("1978-04-13"),
                                     as.Date("1988-04-13"),
                                     as.Date("1998-04-13"),
                                     as.Date("2008-04-13"),
                                     as.Date("2018-04-13")),
                            Price = c(40, 40, 40, 10, 10),
                            label = c("2030s", "2040s", "2050s", "2060s", 
                                      "2070s"))


rects <- data.frame(xstart = c(data_date_start,
                               as.Date("1993-04-13"),
                               as.Date("2003-04-14"),
                               as.Date("2013-04-14")),
                    xend = c(as.Date("1993-04-12"),
                             as.Date("2003-04-13"),
                             as.Date("2013-04-13"),
                             data_date_end),
                    label = c("2040s", "2050s", "2060s", "2070s"))

# custom theme
th <- theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.text.x = element_text(margin = margin(t=8, r=0, b=5, l=5,
                                                       unit = "pt")),
            axis.text.y = element_text(margin = margin(t=0, r=8, b=5, l=0,
                                                       unit = "pt")),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(5, "pt"),
            plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 14, hjust = 0.5),
            plot.caption = element_text(size = 12, vjust = 1),
            plot.margin = margin(t=10, r=20, b=10, l=10, unit = "pt"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.5),
            legend.position = c(0.2, 0.85),
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            legend.key = element_rect(fill = "white", color = "white"), 
            legend.key.width = unit(25, "pt"), 
            legend.key.size = unit(1.5, "line"),
            legend.background = element_rect(fill = "white", size = 0.5, 
                                             linetype = "solid", 
                                             color = "black"))

# plot: petrol & diesel vs brent vs USD

pl_pdbu <- ggplot() + 
  geom_line(data = df_plot, aes(x = Date, y = Price, color = Measure),
            size = 1) + 
  geom_point(data = df_plot_pd, aes(x = Date, y = Price),
             color = "black", size = 0.9, alpha = 1) + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = 0, ymax = 160,
                              fill = label),
            alpha = 1/5, show.legend = F) +
  geom_label(data = decade.labels, aes(x = date, y = Price, label = label), 
             color = "black", size = 5) + 
  geom_rug(data = df_plot_pd, aes(x = Date), outside = F, sides = "t", 
           length = unit(10, "pt"), color = "grey20", alpha = 1/2) + 
  scale_color_manual(labels = c("Brent crude oil price (USD per barrel)", 
                                "Diesel price (NRs per liter)", 
                                "Petrol price (NRs per liter)",
                                "Exchange rate (NRs per USD)"),
                     values = c("grey70", "blue", "red", "lawngreen")) + 
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) + 
  scale_x_date(breaks = seq(data_date_start, data_date_end,
                            by = "2 years"), 
               date_labels = "%Y", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, 160, 20), limits = c(0, 160), 
                     expand = c(0, 0)) + 
  guides(shape = F) + 
  xlab("Year") + 
  ylab("Price") + 
  ggtitle("Historical Trend: Price of Petroleum Products, Brent Crude Price & Exchange Rate") + 
  theme_bw()

pl_pdbu + th

# plot: diesel vs brent vs USD

pl_dbu <- ggplot() + 
  geom_line(data = df_plot_dbu, aes(x = Date, y = Price, color = Measure),
            size = 0.7) + 
  geom_point(data = df_plot_d, aes(x = Date, y = Price),
             color = "black", size = 0.9, alpha = 2/3) + 
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = 0, ymax = 160,
                              fill = label),
            alpha = 1/5, show.legend = F) +
  geom_label(data = decade.labels, aes(x = date, y = Price, label = label), 
             color = "black", size = 5) + 
  geom_rug(data = df_plot_d, aes(x = Date), outside = F, sides = "t", 
           length = unit(10, "pt"), color = "grey20", alpha = 1/2) + 
  scale_color_manual(labels = c("Brent crude oil price (USD per barrel)", 
                                "Diesel price (NRs per liter)",
                                "Exchange rate (NRs per USD)"),
                     values = c("grey70", "blue", "lawngreen")) + 
  scale_fill_manual(values = c("grey60", "grey80", "grey60", "grey80")) + 
  scale_x_date(breaks = seq(data_date_start, data_date_end,
                            by = "2 years"), 
               date_labels = "%Y", expand = c(0, 0)) + 
  scale_y_continuous(breaks = seq(0, 160, 20), limits = c(0, 160), 
                     expand = c(0, 0)) + 
  guides(shape = F) + 
  xlab("Year") + 
  ylab("Price") + 
  ggtitle("Historical Trend of Diesel Price, Brent Crude Oil Price & Exchange Rate",
          subtitle = "(Created by: Pramesh Pudasaini)") + 
  theme_bw()

plot_dbu <- pl_dbu + th
plot_dbu

ggsave(filename = "./figures/historical_trend_diesel_brent_exchange.png",
       plot = plot_dbu, units = "cm", width = 29.7, height = 21, dpi = 1200)




## ------------- ##
## DATA ANALYSIS ##
## ------------- ##


# maximum and minimum prices in each decade 

min.dec <- as.data.frame(df_all %>% 
                           group_by(Decade) %>% 
                           summarize(across(where(is.numeric) & !"Date",
                                            .fns = min, 
                                            na.rm = T,
                                            .names = "min.{col}")) %>% 
                           select(-min.Year))
max.dec <- as.data.frame(df_all %>% 
                           group_by(Decade) %>% 
                           summarize(across(where(is.numeric) & !"Date",
                                            .fns = max,
                                            na.rm = T,
                                            .names = "max.{col}")) %>% 
                           select(-max.Year))

min.max <- full_join(min.dec, max.dec)
colnames(min.max)
col_order <- c("Decade", 
               "min.Diesel", "max.Diesel", 
               "min.Petrol", "max.Petrol", 
               "min.Kerosene", "max.Kerosene",
               "min.LPG", "max.LPG",
               "min.ATF_DP", "max.ATF_DP",
               "min.ATF_DF", "max.ATF_DF",
               "min.Brent", "max.Brent",
               "min.USD", "max.USD")

min.max <- min.max[, col_order]
min.max[sapply(min.max, is.infinite)] <- NA
