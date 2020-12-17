knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "../")

source("data/Source.R")

# extract death rate data for selected state
mort.rate <- covid.data %>% 
  dplyr::filter( 
    state_abbr == "MA",
    period == tail(date_of_all, n=1) ) %>%
  dplyr::select(county_fips, death_rate)

scatter.data <- dplyr::inner_join(
                  mort.rate,
                  factors[c("county_fips", "pct_less_18")],
                  by = "county_fips")

plot.scatter <- ggplot(scatter.data) +
                geom_point(
                  aes_string(
                    x = "death_rate",
                    y = "pct_less_18" )) +
                xlab("Mortality Rate") +
                ylab("Younger Than 18")

#plot.scatter
#ggsave("result/ma_18_deathrate.png")

# generate raw kendall correlation from mort.rate and factors
mort.kendall.cor <- mort.rate %>% 
  dplyr::mutate(VAR = death_rate) %>%
  kendall.func(factors) %>%
  dplyr::mutate(
    DIR = dplyr::if_else(
      kendall_cor <= 0,
      "Protective",
      "Destructive"),
    chr_code = namemap[chr_code, 1]) %>% 
  na.omit()

write.csv(mort.kendall.cor, "result/kendall_result.csv")

# sort kendall cor
mort.kendall.cor.new <- mort.kendall.cor %>% 
  dplyr::filter(kendall_p < 0.1) %>% 
  dplyr::arrange(desc(kendall_cor)) %>% 
  dplyr::top_n(15, abs(kendall_cor)) %>% 
  dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))

write.csv(mort.kendall.cor.new, "result/kendall_top15.csv")

mort.kendall.cor.new %>% 
  ggplot(
    aes(x = chr_code, 
        y = kendall_cor, 
        color = DIR, 
        fill = DIR) ) + 
  
  # Lollipop top
  geom_point(
    # stat = 'identity', 
    size = 15) + 
  # lollipop tail
  geom_segment(
    size = 1,
    aes(y = 0, 
        x = chr_code, 
        yend = kendall_cor, 
        xend = chr_code, 
        color = DIR) ) +
  # factor name by side
  geom_text(
    aes(label = chr_code, 
        y = ifelse(DIR == "Protective", 0.1, -0.1),
        hjust = ifelse(DIR == "Protective", 0, 1) ), 
    color = "#565254", 
    size = 4 ) +
  # correlation values on lollipop
  geom_text(
    aes(label = round(kendall_cor, 2)), 
    color = "#565254", 
    size = 3 ) +
  
  # Coordinates
  coord_flip() + 
  scale_y_continuous(breaks = seq(-1, 1, by = .5), 
                     limits = c(-1, 1)) +
  
  # Themes
  geom_hline(yintercept = .0, linetype = "dashed") + 
  labs(y = "Correlation",
       x = NULL,
       title = "Factors Associated with COVID-19 Deaths for MA",
       fill = "Association",
       color = "Association") +
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.position="top")

ggsave("result/MA_kendall.png", width = 8, height = 10)