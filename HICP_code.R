library(ggplot2)
library(eurostat)
library(dplyr)


toc <- get_eurostat_toc()
toc
head(toc)

d <-  get_eurostat("prc_hicp_manr", type = "code", time_format = "date", filters = list(coicop="CP00"))


d1 <- d %>% 
  filter(geo != "UK", geo != "EA" , geo != "EA18", geo != "EA19", geo != "EEA", geo != "EU", geo != "EU27_2020",
         geo != "EU28", geo != "US", geo != "XK", geo != "CH", geo != "AL", geo != "NO", geo != "ME",
         geo != "TR", geo != "MK", geo != "RS", geo != "IS", geo != "IS", geo != "EA20",
         time >= "2000-02-01", time <= "2022-09-01") %>%
  mutate(country = case_when(geo == 'AT' ~ 'Austria', geo == 'BE' ~ 'Belgium', 
                             geo == 'BG' ~ 'Bulgaria', geo == 'CY' ~ 'Cyprus', 
                             geo == 'CZ' ~ 'Czechia', geo == 'DE' ~ 'Germany', geo == 'DK' ~ 'Denmark', 
                             geo == 'EE' ~ 'Estonia', geo == 'EL' ~ 'Greece', geo == 'ES' ~ 'Spain', 
                             geo == 'FR' ~ 'France', geo == 'FI' ~ 'Finland', geo == 'HR' ~ 'Croatia',
                             geo == 'HU' ~ 'Hungary', geo == 'IE' ~ 'Ireland',
                             geo == 'IT' ~ 'Italy', geo == 'LT' ~ 'Lithuania', geo == 'LU' ~ 'Luxembourg',
                             geo == 'LV' ~ 'Latvia',
                             geo == 'MT' ~ 'Malta', geo == 'NL' ~ 'Netherlands',
                             geo == 'PL' ~ 'Poland', geo == 'PT' ~ 'Portugal', geo == 'RO' ~ 'Romania',
                             geo == 'SE' ~ 'Sweden', geo == 'SI' ~ 'Slovenia',
                             geo == 'SK' ~ 'Slovakia'))


#head(d1)

#sort(unique(d1$geo))

ggplot(d1, aes(x = time, y = values, color = country)) + geom_line()

dat <- xtabs(values ~ country + time, data = d1)
mat <- as.matrix(dat)
dis <- dist(mat, method = "minkowski", p = 1.5)
dend <- hclust(dis,  method = "complete")
plot(dend, main = "Clustering countries based on HICP", sub ="", xlab = "")
rect.hclust(dend, k=4, border="red")


