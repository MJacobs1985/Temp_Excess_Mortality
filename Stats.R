rm(list = ls())
#### load libraries ----
library(readxl)
library(readr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(timetk)
library(gghighlight)
library(anomalize)
library(imputeTS)
library(AnomalyDetection)
library(urca)
#### LOAD, WRANGLE AND EXPLORE CBS DATA    - EXCESS MORTALITY ----
Excess_mortality <- readr::read_csv("~/MSJ/Projects/2023/ZonMW/UitgesteldeZorg/Data/CBS/Excess_mortality.csv")
Excess_mortality%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Jaar) + 
                                 lubridate::weeks(Week)))%>%
  ggplot(aes(x=Date, 
             y=Overledenen))+
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin=Verwacht_aantal_overledenen_laag, 
                  ymax=Verwacht_aantal_overledenen_hoog, 
                  fill="Excess Mortality"), 
              alpha=0.2, show.legend = FALSE)+
  theme_bw()+
  labs(x="Date", 
       y="Mortality", 
       caption="Source: CBS", 
       title="Mortality and expected mortality in The Netherlands")

#### LOAD, WRANGLE AND EXPLORE WMD DATA    - MORTALITY & EXCESS MORTALITY - DKOBAK ----
excess_mortality_timeseries <- read_csv("~/MSJ/Projects/2023/ZonMW/UitgesteldeZorg/Data/Dkobak_WMD/excess-mortality-timeseries.csv")
MD_NL<-read_csv("~/MSJ/Projects/2023/ZonMW/UitgesteldeZorg/Data/Akerlinsky_WMD/Oversterfte/world_mortality.csv")%>%
  filter(country_name=="Netherlands")%>%
  mutate(date = lubridate::make_datetime(year = year) + lubridate::weeks(time))%>%
  select(date, country_name, deaths)
EMD_NL<-excess_mortality_timeseries%>%
  filter(country_name=="Netherlands")%>%
  mutate(date = lubridate::make_datetime(year = year) + lubridate::weeks(time))%>%
  rename(deaths = `excess deaths`)%>%
  select(date, country_name, deaths)
ggplot()+
  geom_line(data=MD_NL, 
            aes(x=date, 
                y=deaths, 
                col="Mortality"))+
  geom_line(data=EMD_NL,
            aes(x=date,
                y=deaths,
                col="Excess Mortality"))+
  theme_bw()+
  labs(x="Date", 
       y="Mortality",
       col="",
       title="Mortality and excess mortality in the Netherlands", 
       caption="Source: World Mortality Data by Ariel Karlinsky and Dmitry Kobak")+
  theme(legend.position="bottom")


MD_NL%>%
  rename(mortality=deaths)%>%
  select(-country_name)%>%
  left_join(., EMD_NL, by = join_by(date))%>%
  rename(EM = deaths)%>%
  mutate(difference=mortality-EM)%>%
  ggplot(aes(x=date))+
  geom_line(aes(y=mortality, 
                col="Observed"))+
  geom_line(aes(y=EM,
                col="Excess"))+
  geom_line(aes(y=difference,
                col="Expected"))+
  theme_bw()+
  labs(x="Date", 
       y="Mortality",
       col="",
       title="Mortality in the Netherlands", 
       subtitle="Looking at observed mortality, expected mortality, and excess mortality",
       caption="Source: World Mortality Data by Ariel Karlinsky and Dmitry Kobak")+
  theme(legend.position="bottom")




#### https://www.knmi.nl/nederland-nu/klimatologie/daggegevens----
KNMI_deBilt <- read_excel("~/MSJ/Blogging/Global_Average_Temp/Data/KNMI_deBilt.xlsx")%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))

KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "year", 
    TG_mean  = mean(TG), 
    TN_mean  = mean(TN),
    TX_mean  = mean(TX))%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG_mean))+
  geom_ribbon(aes(x=Date, ymin=TN_mean, ymax=TX_mean), alpha=0.4)+
  theme_bw()+
  labs(x="Date", 
       y="Average temperature", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Yearly values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")


KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  filter(Date>"1979-12-01")%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG_mean))+
  geom_ribbon(aes(x=Date, ymin=TN_mean, ymax=TX_mean), alpha=0.4)+
  theme_bw()+
  labs(x="Date", 
       y="Average temperature (month)", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Monthly values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    TG_mean  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  filter(Date>"2012-12-01")%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG_mean))+
  geom_ribbon(aes(x=Date, ymin=TN_mean, ymax=TX_mean), alpha=0.4)+
  theme_bw()+
  labs(x="Date", 
       y="Average temperature (month)", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Monthly values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")

KNMI_deBilt%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "week", 
    TG_mean  = mean(TG/10), 
    TN_mean  = mean(TN/10),
    TX_mean  = mean(TX/10))%>%
  filter(Date>"2018-12-01")%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG_mean))+
  geom_ribbon(aes(x=Date, ymin=TN_mean, ymax=TX_mean), alpha=0.4)+
  theme_bw()+
  labs(x="Date", 
       y="Average temperature (week)", 
       title="Average temperature over time for the Netherlands", 
       subtitle = "Weekly values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens")


KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(Hot16 = if_else(TX>=16.5, 1, 0), 
         Hot20 = if_else(TX>=20, 1, 0),
         Hot25 = if_else(TX>=25, 1, 0),
         Hot30 = if_else(TX>=30, 1, 0))%>%
  filter(Year>2017)%>%
  group_by(Year)%>%
  summarise(Hot16=sum(Hot16), 
            Hot20=sum(Hot20), 
            Hot25=sum(Hot25), 
            Hot30=sum(Hot30))%>%
  pivot_longer(cols = -(Year), 
               values_to = "Value", 
               names_to = "Threshold")%>%
  ggplot()+
  geom_bar(aes(x=factor(Year), y=Value, fill=Threshold),stat="identity", position="dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(y="Frequency", 
       x="",
       title="Number of days temperature above threshold for the Netherlands", 
       subtitle = "Based on maximum daily temperatures")


KNMI_deBilt%>%
  mutate(TG=(TG/10),
         TN=(TN/10),
         TX=(TX/10),
         month_num  = lubridate::month(Date),
         Year       = lubridate::year(Date),
         Day_month  = lubridate::mday(Date),
         Day        = lubridate::yday(Date),
         month_lab  = month.abb[month_num])%>%
  mutate(Hot16 = if_else(TX>=16.5, 1, 0), 
         Hot20 = if_else(TX>=20, 1, 0),
         Hot25 = if_else(TX>=25, 1, 0),
         Hot30 = if_else(TX>=30, 1, 0))%>%
  filter(Year>1972)%>%
  group_by(Year)%>%
  summarise(Hot16=sum(Hot16), 
            Hot20=sum(Hot20), 
            Hot25=sum(Hot25), 
            Hot30=sum(Hot30))%>%
  pivot_longer(cols = -(Year), 
               values_to = "Value", 
               names_to = "Threshold")%>%
  ggplot()+
  geom_point(aes(x=Year, y=Value, col=Threshold))+
  geom_line(aes(x=Year, y=Value, col=Threshold))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(y="Frequency", 
       x="",
       title="Number of days temperature above threshold for the Netherlands", 
       subtitle = "Based on maximum daily temperatures")




#### LOAD, WRANGLE AND EXPLORE CBS DATA    - MORTALITY ----
CBS_Sterfte <- readr::read_delim("~/MSJ/Projects/2023/ZonMW/UitgesteldeZorg/Data/CBS/Sterfte.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
CBS_Sterfte[CBS_Sterfte == "."] <- NA
CBS_Sterfte%>%
  select(Geslacht, Overledenen_1, Perioden, LeeftijdOp31December)%>%
  mutate(Overledenen_1 = as.numeric(Overledenen_1), 
         Year = as.numeric(substr(Perioden, 1, 4)), 
         Week = as.numeric(substr(Perioden, 7, 8)), 
         Date = lubridate::ymd(lubridate::make_datetime(year = Year) + 
                                 lubridate::weeks(Week)))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 1100, "Total"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 10000, "Total"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 21700, "80_and_older"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 41700, "0_to_65"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 53950, "65_to_80"))%>%
  filter(!LeeftijdOp31December=="Total")%>%
  filter(!Geslacht=="Total")%>%
  drop_na()%>%
  ggplot(., aes(x=Date, 
                y=Overledenen_1, 
                col=factor(Geslacht)))+
  #geom_point()+
  geom_line()+
  facet_grid(~LeeftijdOp31December, scales="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       col="Sex", 
       y="Deceased", 
       title="Number of deceased by Date, Sex, and Age category", 
       caption = "Source: CBS")

CBS_Sterfte%>%
  select(Geslacht, Overledenen_1, Perioden, LeeftijdOp31December)%>%
  mutate(Overledenen_1 = as.numeric(Overledenen_1), 
         Year = as.numeric(substr(Perioden, 1, 4)), 
         Week = as.numeric(substr(Perioden, 7, 8)), 
         Date = lubridate::ymd(lubridate::make_datetime(year = Year) + 
                                 lubridate::weeks(Week)))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 1100, "Total"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 10000, "Total"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 21700, "80_and_older"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 41700, "0_to_65"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 53950, "65_to_80"))%>%
  filter(!LeeftijdOp31December=="Total")%>%
  filter(!Geslacht=="Total")%>%
  drop_na()%>%
  ggplot(., aes(x=Date, 
                y=Overledenen_1, 
                col=factor(LeeftijdOp31December)))+
  geom_line()+
  facet_grid(~Geslacht, scales="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       col="Age category", 
       y="Deceased", 
       title="Number of deceased by Date, Sex, and Age category", 
       caption = "Source: CBS")
#### LOAD, WRANGLE AND EXPLORE OWID DATA   - MORTALITY                    - RAW DEATH ----
excess_mortality_raw_death_count <- readr::read_csv("~/MSJ/Projects/2023/ZonMW/UitgesteldeZorg/Data/OWID/Oversterfte/excess-mortality-raw-death-count.csv")
excess_mortality_raw_death_count%>%
  filter(Entity=="Netherlands")%>%
  select(Day)%>%
  dim()
excess_mortality_raw_death_count%>%
  filter(Entity=="Netherlands")%>%
  mutate(Week=row_number())%>%
  select(-c(projected_deaths_since_2020_all_ages, Day))%>%
  pivot_longer(-c(Entity, Code, Week),
               names_to = "Jaar",
               values_to = "Value",
               values_drop_na = TRUE)%>%
  mutate(Jaar = substring(Jaar,8,11))%>%
  ggplot()+
  geom_point(aes(x=Week, y=Value, col=Jaar, group=Jaar))+
  geom_line(aes(x=Week, y=Value, col=Jaar, group=Jaar))+
  gghighlight::gghighlight(Jaar>2019)+
  theme_bw()+
  labs(x="Week", 
       y="Raw death count ", 
       title="Mortality for The Netherlands", 
       caption="Source: Our World in Data")










#### Combine temperature and mortality data in one - CBS ----
df_sterfte <- readr::read_delim("~/MSJ/Projects/2023/ZonMW/UitgesteldeZorg/Data/CBS/Sterfte.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
df_sterfte[df_sterfte == "."] <- NA
df_sterfte<-df_sterfte%>%
  select(Geslacht, Overledenen_1, Perioden, LeeftijdOp31December)%>%
  mutate(Overledenen_1 = as.numeric(Overledenen_1), 
         Year = as.numeric(substr(Perioden, 1, 4)), 
         Week = as.numeric(substr(Perioden, 7, 8)), 
         Date = lubridate::ymd(lubridate::make_datetime(year = Year) + 
                                 lubridate::weeks(Week)))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 1100, "Total"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 10000, "Total"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 21700, "80_and_older"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 41700, "0_to_65"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 53950, "65_to_80"))%>%
  filter(!LeeftijdOp31December=="Total")%>%
  filter(!Geslacht=="Total")%>%
  drop_na()%>%
  dplyr::rename(Mortality = Overledenen_1)%>%
  select(Geslacht, Mortality, Date, Year, Week, LeeftijdOp31December)

df_sterfte_all<-df_sterfte%>%
  arrange(Date)%>%
  group_by(Year, Week)%>%
  mutate(All_cause_mort=sum(Mortality))%>%
  select(Year, Week, All_cause_mort)%>%
  distinct()%>%
  ungroup()

df_temp <- read_excel("~/MSJ/Blogging/Global_Average_Temp/Data/KNMI_deBilt.xlsx")%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))%>%
  filter(Date>="1995-01-08" & Date<"2023-08-07")%>%
  mutate(Year =lubridate::year(Date), 
         Week =lubridate::week(Date))%>%
  group_by(Year, Week)%>%
  select(TG, TN, TX, Year, Week)%>%
  mutate(TG_mean  = mean(TG), 
         TN_mean  = mean(TN),
         TX_mean  = mean(TX))%>%
  select(Year, Week, TG_mean, TN_mean, TX_mean)%>%
  distinct()

combined<-df_temp%>%
  dplyr::left_join(., df_sterfte_all, by=c("Year", "Week"))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Year) + 
                            lubridate::weeks(Week)))%>%
  ungroup()

combined%>%
  ggplot()+
  geom_line(aes(x=Date, y=TG_mean))+
  geom_line(aes(x=Date, y=All_cause_mort))

combined%>%
  select(-c(Year, Week))%>%
  mutate(All_cause_mort_imp = imputeTS::na.kalman(All_cause_mort))%>%
  ggplot()+
  geom_line(aes(x=Date, y=All_cause_mort_imp), col="red")+
  geom_line(aes(x=Date, y=All_cause_mort), col="black")

combined%>%
  select(-c(Year, Week))%>%
  mutate(All_cause_mort = imputeTS::na_kalman(All_cause_mort))%>%
  mutate(All_cause_mort_normal = as.vector(scale(All_cause_mort)), 
         TG_mean_normal = as.vector(scale(TG_mean)), 
         TX_mean_normal = as.vector(scale(TX_mean)), 
         TN_mean_normal = as.vector(scale(TN_mean)))%>%
  ggplot()+
  geom_line(aes(x=Date, y=All_cause_mort_normal, col="Mortality"))+
  geom_line(aes(x=Date, y=TX_mean_normal, col="Temperature"))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       y="Normalized value", 
       title="Highest temperature and all-cause mortality over time for the Netherlands", 
       color="",
       subtitle = "Weekly temperature values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens & CBS")

combined%>%
  select(-c(Year, Week))%>%
  mutate(All_cause_mort = imputeTS::na_kalman(All_cause_mort))%>%
  mutate(All_cause_mort_normal = as.vector(scale(All_cause_mort)), 
         TX_mean_normal = as.vector(scale(TX_mean)))%>%
  select(Date, All_cause_mort_normal, TX_mean_normal)%>%
  pivot_longer(-Date,
               names_to = "Metric", 
               values_to = "Value")%>%
  group_by(Metric)%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    mean_value  = mean(Value))%>%
  plot_time_series(Date, mean_value, .facet_ncol = 1, .interactive = FALSE)

roll_avg_12 <- slidify(.f = mean, .period = 12, .align = "center", .partial = TRUE)
combined%>%
  select(-c(Year, Week))%>%
  mutate(All_cause_mort = imputeTS::na_kalman(All_cause_mort))%>%
  mutate(All_cause_mort_normal = as.vector(scale(All_cause_mort)), 
         TX_mean_normal = as.vector(scale(TX_mean)))%>%
  select(Date, All_cause_mort_normal, TX_mean_normal)%>%
  pivot_longer(-Date,
               names_to = "Metric", 
               values_to = "Value")%>%
  group_by(Metric)%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "month", 
    mean_value  = mean(Value))%>%
  mutate(roll_avg_12 = roll_avg_12(mean_value))%>%
  tidyr::pivot_longer(cols = c(mean_value, roll_avg_12)) %>%
  plot_time_series(Date, value, .color_var = name,
                   .facet_ncol = 1, .smooth = FALSE, 
                   .interactive = FALSE)

combined%>%
  select(-c(Year, Week))%>%
  mutate(All_cause_mort = imputeTS::na_kalman(All_cause_mort))%>%
  mutate(All_cause_mort_normal = as.vector(scale(All_cause_mort)), 
         TX_mean_normal = as.vector(scale(TX_mean)))%>%
  select(Date, All_cause_mort_normal, TX_mean_normal)%>%
  pivot_longer(-Date,
               names_to = "Metric", 
               values_to = "Value")%>%
  group_by(Metric)%>%
  summarise_by_time(
    .date_var = Date,
    .by       = "year", 
    mean_value  = mean(Value))%>%
  ggplot()+
  geom_point(aes(x=Date, y=mean_value, col=Metric))+
  geom_line(aes(x=Date, y=mean_value, col=Metric))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Date", 
       y="Normalized value", 
       title="Highest temperature and all-cause mortality over time for the Netherlands", 
       color="",
       subtitle = "Weekly temperature values coming from De Bilt", 
       caption="Source: https://www.knmi.nl/nederland-nu/klimatologie/daggegevens & CBS")





#### Cointegration ----
df_sterfte <- readr::read_delim("~/MSJ/Projects/2023/ZonMW/UitgesteldeZorg/Data/CBS/Sterfte.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
df_sterfte[df_sterfte == "."] <- NA
df_sterfte<-df_sterfte%>%
  select(Geslacht, Overledenen_1, Perioden, LeeftijdOp31December)%>%
  mutate(Overledenen_1 = as.numeric(Overledenen_1), 
         Year = as.numeric(substr(Perioden, 1, 4)), 
         Week = as.numeric(substr(Perioden, 7, 8)), 
         Date = lubridate::ymd(lubridate::make_datetime(year = Year) + 
                                 lubridate::weeks(Week)))%>%
  mutate(Geslacht = replace(Geslacht, 
                            Geslacht == 1100, "Total"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 3000, "Man"),
         Geslacht = replace(Geslacht, 
                            Geslacht == 4000, "Women"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 10000, "Total"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 21700, "80_and_older"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 41700, "0_to_65"),
         LeeftijdOp31December = replace(LeeftijdOp31December, 
                                        LeeftijdOp31December == 53950, "65_to_80"))%>%
  filter(!LeeftijdOp31December=="Total")%>%
  filter(!Geslacht=="Total")%>%
  drop_na()%>%
  dplyr::rename(Mortality = Overledenen_1)%>%
  select(Geslacht, Mortality, Date, Year, Week, LeeftijdOp31December)

df_sterfte_all<-df_sterfte%>%
  arrange(Date)%>%
  group_by(Year, Week)%>%
  mutate(All_cause_mort=sum(Mortality))%>%
  select(Year, Week, All_cause_mort)%>%
  distinct()%>%
  ungroup()

df_temp <- read_excel("~/MSJ/Blogging/Global_Average_Temp/Data/KNMI_deBilt.xlsx")%>%
  mutate(Date = lubridate::ymd(YYYYMMDD))%>%
  select(-c(STN, YYYYMMDD))%>%
  filter(Date>="1995-01-08" & Date<"2023-08-07")%>%
  mutate(Year =lubridate::year(Date), 
         Week =lubridate::week(Date))%>%
  group_by(Year, Week)%>%
  select(TG, TN, TX, Year, Week)%>%
  mutate(TG_mean  = mean(TG), 
         TN_mean  = mean(TN),
         TX_mean  = mean(TX))%>%
  select(Year, Week, TG_mean, TN_mean, TX_mean)%>%
  distinct()

combined<-df_temp%>%
  dplyr::left_join(., df_sterfte_all, by=c("Year", "Week"))%>%
  mutate(Date = lubridate::ymd(lubridate::make_datetime(year = Year) + 
                                 lubridate::weeks(Week)))%>%
  ungroup()%>%
  select(-c(Year, Week))%>%
  mutate(All_cause_mort = imputeTS::na_kalman(All_cause_mort))%>%
  mutate(All_cause_mort_normal = as.vector(scale(All_cause_mort)), 
         TX_mean_normal = as.vector(scale(TX_mean)))%>%
  select(Date, All_cause_mort_normal, TX_mean_normal)

Mort_ts<-combined%>%
  select(All_cause_mort_normal)%>%
  ts(. ,
     start=c(1995, 1), 
     end=c(2023, 8), frequency=52)

Temp_ts<-combined%>%
  select(TX_mean_normal)%>%
  ts(. ,
     start=c(1995, 1), 
     end=c(2023, 8), frequency=52)

plot(Mort_ts)
acf(Mort_ts)
plot(diff(Mort_ts))
acf(diff(Mort_ts))

plot(Temp_ts)
acf(Temp_ts)
plot(diff(Temp_ts))
acf(diff(Temp_ts))

VECM_ECT <- Mort_ts - Temp_ts
plot(VECM_ECT)
acf(VECM_ECT)
plot(diff(VECM_ECT))
acf(diff(VECM_ECT))

cor(Mort_ts, Temp_ts)
ccfvalues<-ccf(as.numeric(Temp_ts), as.numeric(Mort_ts)); ccfvalues

astsa::lag2.plot (as.numeric(Temp_ts), 
                  as.numeric(Mort_ts), 10)
astsa::lag2.plot (as.numeric(Mort_ts), 
                  as.numeric(Temp_ts), 10)

ADF_Temp_ts<-urca::ur.df(Temp_ts, 
                 lags = 14, 
                 selectlags = "AIC", 
                 type = "trend")      
summary(ADF_Temp_ts)
plot.ts(ADF_Temp_ts@res, ylab = "Residuals")
abline(h = 0, col = "red")
tsm::ac(ADF_Temp_ts@res, max.lag = 20)
summary(urca::ur.ers(Temp_ts,
               model = "trend", 
               lag.max = 14))    

tseries::adf.test(Temp_ts)     
tseries::kpss.test(Temp_ts, null="Trend")
Box.test(Temp_ts, lag=14, type="Ljung-Box")

ADF_Deathts<-urca::ur.df(Mort_ts, 
                   lags = 4, 
                   selectlags = "AIC", 
                   type = "trend")      
summary(ADF_Deathts)
plot(ADF_Deathts)
tsm::ac(ADF_Deathts@res, max.lag = 20)
tsm::gts_ur(ADF_Deathts)

summary(ur.ers(Mort_ts,
               model = "trend", 
               lag.max = 2))         # lag at 2
tseries::adf.test(Mort_ts)     
tseries::kpss.test(Mort_ts, null="Trend")
Box.test(Mort_ts, lag=2, 
         type="Ljung-Box")


VECM_ECT<-Mort_ts - Temp_ts
ADF_DeathtsTEMP<-ur.df(VECM_ECT, 
                      lags = 24, 
                      selectlags = "AIC", 
                      type = "trend")
summary(ADF_DeathtsTEMP)
plot(ADF_DeathtsTEMP)
tsm::ac(ADF_DeathtsTEMP@res, max.lag = 24)
tsm::gts_ur(ADF_DeathtsTEMP)

summary(ur.ers(Mort_ts - Temp_ts, 
               type="DF-GLS",
               model = "trend", 
               lag.max=2))         
tseries::adf.test(VECM_ECT, k=14)  
Box.test(VECM_ECT, lag=14,type="Ljung-Box")

TEMP_Death <- dynlm::dynlm(Mort_ts ~ Temp_ts)
summary(TEMP_Death)
par(mfrow = c(2, 2))
plot(TEMP_Death)
z_hat <- resid(TEMP_Death)
dev.off();plot(z_hat)
acf(z_hat)
tsm::gts_ur(z_hat)

aTSA::coint.test(Mort_ts, Temp_ts, d = 1, nlag = 4, output = TRUE)

VECM_EQ1<-dynlm(d(Mort_ts)~L(d(Temp_ts),1:2)+L(d(Mort_ts),1:2)+ L(VECM_ECT))
VECM_EQ2<-dynlm(d(Temp_ts)~L(d(Temp_ts),1:2)+L(d(Mort_ts),1:2) + L(VECM_ECT))
names(VECM_EQ1$coefficients) <- c("Intercept", "D_Temp_ts_l1", "D_Temp_ts_l2", "D_Mort_ts_l1", "D_Mort_ts_l2", "ect_l1")
names(VECM_EQ2$coefficients) <- names(VECM_EQ1$coefficients)
lmtest::coeftest(VECM_EQ1,vcov.=sandwich::NeweyWest(VECM_EQ1,prewhite=F,adjust=T))
lmtest::coeftest(VECM_EQ2,vcov.=sandwich::NeweyWest(VECM_EQ2,prewhite=F,adjust=T))


car::linearHypothesis(VECM_EQ1, 
                      hypothesis.matrix=c("D_Temp_ts_l1", 
                                          "D_Temp_ts_l2"),
                      vcov. = sandwich::sandwich)
car::linearHypothesis(VECM_EQ2, 
                      hypothesis.matrix=c("D_Mort_ts_l1",
                                          "D_Mort_ts_l2"),
                      vcov. = sandwich::sandwich)

lmtest::grangertest(Mort_ts~Temp_ts, order = 2)
lmtest::grangertest(Temp_ts~Mort_ts, order = 2)

jotest=ca.jo(data.frame(Mort_ts,Temp_ts), 
             type="trace", 
             K=2, 
             ecdet="none", 
             spec="longrun")
summary(jotest) 
plot(jotest)

combined<-ts.union(diff(Mort_ts), diff(Temp_ts))
plot(combined)
VAR_est <- vars::VAR(y = combined, p = 14)
summary(VAR_est)

