knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "../")

libs = c("readr","dplyr","tibble","tidyverse","RCurl","XML")
for (lib in libs){
  # if required, install before loading
  if (!require(lib, character.only = T)) {
    install.packages(lib)
  }
  do.call(library, list(lib))
}

# Read in new Social Determinants definitions
SocialDeterminants <- read_csv("data/SocialDeterminants.csv")

final.determinants <- SocialDeterminants[SocialDeterminants$Keep == 1,]["Code"]
final.determinants <- append(final.determinants$Code, "county_fips", after = 0)

# Load all data
chr.data.2019 <- readRDS("data/chr.data.2019.rds")
popu <- subset(chr.data.2019, select = c(county_fips, population))
chr.data.2019 <- chr.data.2019 %>%
  as_data_frame %>%
  select(final.determinants)

# Load name map
chr.namemap.2019 <- SocialDeterminants %>% select("Code", "Name")

geo.namemap <- readRDS("data/geo.namemap.rds")
geo <- subset(geo.namemap, select = -c(state_fips))
geo$county_fips <- str_pad(geo$county_fips, 5, pad = "0")

km.func <- function(data.mat, cluster.num=3, seed=200) {
  
  set.seed(seed)
  cluster.num <- min(nrow(data.mat) - 1, cluster.num)
  data.mat <- na.omit(data.mat)
  
  km.result <- kmeans(dplyr::select(data.mat, -county_fips), cluster.num)
  return(
    tibble(
      county_fips = dplyr::pull(data.mat, county_fips),
      cluster = as.character(km.result$cluster)
    )
  )
}

kendall.func <- function(x.data, sd.data) {
  
  align <- dplyr::left_join(x.data, sd.data, by = "county_fips") %>% 
    dplyr::select(-dplyr::one_of(c("county_fips", "state_name", "county_name")))

  x <- as.numeric(dplyr::pull(align, 1))
  sd <- dplyr::select(align, -1)
  
  cor.res <- list()
  for (n in names(sd)) {
    y <- dplyr::pull(sd, n)
    if (is_character(y)) {
      # print(y)
      y <- readr::parse_number(y)
    }
    
    if (sum(is.na(y)) < 0.5 * length(y)) {
      # print(tibble(x, y, n))
      cor.res[[n]] <- cor.test(
        x = x, y = y, use = "pairwise.complete.obs", method = "kendall", exact = F
      )
    }
  }
  
  tibble(
    chr_code = names(cor.res),
    kendall_cor = sapply(cor.res, function(r) r$estimate),
    kendall_p = sapply(cor.res, function(r) r$p.value)
  )
}

# Import COVID-19 data from JHU github repo

date_of_study = "2020-11-15"


# Historical data
covid_hist = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-22-2020.csv")))
covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)
covid_us_hist = subset(covid_us_hist, select = c(FIPS, Deaths))
covid_us_hist$FIPS <- str_pad(covid_us_hist$FIPS, 5, pad = "0")
covid_us_hist = merge(x = covid_us_hist,y = geo, by.x = "FIPS", by.y = "county_fips", all.y = TRUE)
covid_us_hist = merge(x = covid_us_hist,y = popu, by.x = "FIPS", by.y = "county_fips", all.x = TRUE)
covid_us_hist$period = "04-14-2020"

# collect data from March to Month of study on 23rd of each month
date_of_all = format(seq(as.Date("2020-04-15"), as.Date(strptime(date_of_study,"%Y-%m-%d")), by = "month"),"%m-%d-%Y")

for (i in 1:length(date_of_all)){
  covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_all[i],".csv")))
  covid_daily = subset(covid_daily,Country_Region == "US" & is.na(FIPS)==F)
  covid_daily = subset(covid_daily, select = c(FIPS, Deaths))
  covid_daily$FIPS <- str_pad(covid_daily$FIPS, 5, pad = "0")
  covid_daily = merge(x = covid_daily,y = geo, by.x = "FIPS", by.y = "county_fips", all.y = TRUE)
  covid_daily = merge(x = covid_daily,y = popu, by.x = "FIPS", by.y = "county_fips", all.x = TRUE)
  covid_daily$period = date_of_all[i]
  covid_us_hist <- rbind(covid_us_hist, covid_daily)
}

# calculation of death rate
covid_us_hist <- dplyr::rename(covid_us_hist, c(county_fips = FIPS, death_num = Deaths))
covid_us_hist$death_rate <- covid_us_hist$death_num/covid_us_hist$population*100000
covid.data <- covid_us_hist %>% drop_na(death_num,population)


# Infrastructure data on # of hospital beds
hospitals = read.csv(text=getURL("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
hospitals$BEDS[hospitals$BEDS < 0] = NA

county_hospitals_aggregated = hospitals %>%
  group_by(COUNTYFIPS) %>%
  dplyr::summarise(beds = sum(BEDS, na.rm=TRUE))
county_hospitals_aggregated$county_fips = str_pad(county_hospitals_aggregated$COUNTYFIPS, 5, pad = "0")
county_hospitals_aggregated <- merge(county_hospitals_aggregated,popu,by = "county_fips", all = T)
county_hospitals_aggregated <- county_hospitals_aggregated %>% drop_na(population, beds)
county_hospitals_aggregated$popu_beds = county_hospitals_aggregated$beds/county_hospitals_aggregated$population*1000
county_hospitals_aggregated <- subset(county_hospitals_aggregated, select = c(county_fips, popu_beds))

factors = merge(county_hospitals_aggregated, chr.data.2019, by = "county_fips", all = T)
namemap <- chr.namemap.2019 %>% add_row(Code = "popu_beds", Name = "Hospital Beds over Population")
namemap <- column_to_rownames(namemap, "Code")
names(namemap)[1] <- "name"