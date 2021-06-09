# Jorge Camoes plot of excess deaths
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# produce a plot of excess deaths inspired by one posted on Twitter by Jorge Camoes


# in this script --------------------------------------------------------------
# 1 read
# 2 wrangle
# 3 Camoes plot
# 4 save plot
# 5 plot annotations
# 6 musings




# 1 read ----------------------------------------------------------------------
wkDthsTot       <- readRDS(str_c(.datDir, "dat05_total_weekly_deaths_2010_to_2021.RDS"))
wkCovidDthsTot  <- readRDS(str_c(.datDir, "dat05_total_weekly_covidDths_2020_to_2021.RDS"))




# 2 wrangle -------------------------------------------------------------------
expDths <- wkDthsTot %>% 
  filter(isoYr > 2014, isoYr < 2020) %>% 
  group_by(isoWk) %>% 
  summarise(mnDths = mean(dths))

plotDat <- wkDthsTot %>%
  filter(isoYr > 2014, isoYr < 2021) %>% 
  mutate(flag = case_when(isoYr == 2020 ~ 1, TRUE ~ 0))

ribbonDat <- plotDat %>% 
  filter(flag == 1) %>% 
  select(-isoYr, -flag) %>% 
  left_join(expDths, by = "isoWk")

barDat <- plotDat %>%
  filter(flag == 1) %>% 
  select(-flag) %>%
  mutate(isoYrChar = as.character(isoYr)) %>% 
  bind_rows(
    expDths %>% 
      mutate(isoYrChar = "mean") %>% 
      rename(dths = mnDths)
    ) %>%
  arrange(isoWk) %>% 
  mutate(diff = dths - lead(dths, 1L)) %>%
  filter(isoYr == 2020) %>% 
  bind_rows(tibble(isoYr = 2020, isoWk = 1:53, dths = 0, diff = 0, isoYrChar = "2020"))

covidDths <- wkCovidDthsTot %>%
  filter(isoYr == 2020)




# 3 Camoes plot ---------------------------------------------------------------
# (3a) timeseries
plotTs <- ggplot(plotDat %>% filter(flag == 0)) +
  geom_point(aes(x = isoWk, y = dths, group = isoYr)
             , shape = 19, size = 1.8, color = "#AEB3B6", alpha = .8) +
  geom_line(aes(x = isoWk, y = mnDths)
            , data = expDths
            , size = 1, color = "#686F73") +
  geom_line(aes(x = isoWk, y = dths)
            , data = plotDat %>% filter(flag == 1)
            , size = 1.4, color = "#EC6555") +
  geom_point(aes(x = isoWk, y = dths)
             , data = plotDat %>% filter(flag == 1)
             , shape = 19, size = 3, color = "#FFFFFF") +
  geom_point(aes(x = isoWk, y = dths), data = plotDat %>% filter(flag == 1)
             , shape = 19, size = 2, color = "#EC6555") +
  geom_ribbon(aes(x = isoWk, ymin = dths, ymax = mnDths)
              , data = ribbonDat %>% filter(isoWk > 10)
              , fill = "#EC6555", alpha = .35) +
  scale_y_continuous(position = "right", name = NULL, limits = c(6e3, 23e3)
                     , breaks = seq(10e3, 23e3, 4e3), labels = as.character(seq(10, 23, 4))) +
    scale_x_continuous(name = NULL, limits = c(0, 54)) +
    theme(
      axis.ticks = element_blank()
      , axis.text.x = element_blank()
      , plot.background = element_rect(fill = "#F5ECE7", colour = NA)
      , panel.background = element_rect(fill = "#F5ECE7", colour = NA)
      , panel.grid.minor = element_blank()
      , panel.grid.major.x = element_blank()
      , panel.grid.major.y = element_line(size = .4, color = "#FFFFFF")
      , plot.margin =     margin(40, 24, 4, 24)
      , text = element_text(
        family =       "Fira Sans"
        , face =       "plain"
        , colour =     "#2C2825"
        , size =       11
        , lineheight = .9
        , hjust =      .5
        , vjust =      .5
        , angle =      0
        , margin =     margin()
        , debug =      FALSE))

# ggsave(str_c(.figDir, "plotTs.png"), plotTs, width = 80, height = 45, units = "mm", dpi = 300)

# (3b) bars
plotBar <- ggplot(barDat) +
  geom_hline(aes(yintercept = 0), size = .2, color = "#2C2825") +
  geom_bar(aes(x = isoWk, y = diff), stat = "identity", fill = "#5881C1", width = .8) +
  geom_errorbar(aes(x = isoWk, ymin = dths, ymax = dths)
                , data = covidDths
                , color = "#F9BF07", orientation = "x", size = 1, width = .8) +
  scale_y_continuous(position = "right", name = NULL, limits = c(-8e3, 12e3)
                     , breaks = seq(-6e3, 12e3, 6e3), labels = as.character(seq(-6, 12, 6))) +
  scale_x_continuous(name = NULL, limits = c(0, 54), breaks = seq(2, 52, 2), labels = as.character(seq(2, 52, 2))) +
  labs(caption = NULL) +
  theme(
    axis.ticks = element_blank()
    , axis.ticks.length.x = unit(0, "mm")
    , plot.background = element_rect(fill = "#F5ECE7", colour = NA)
    , panel.background = element_rect(fill = "#F5ECE7", colour = NA)
    , panel.grid.minor = element_blank()
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_line(size = .4, color = "#FFFFFF")
    , plot.margin = margin(4, 24, 40, 24)
    , text = element_text(
      family =       "Fira Sans"
      , face =       "plain"
      , colour =     "#2C2825"
      , size =       11
      , lineheight = .9
      , hjust =      .5
      , vjust =      .5
      , angle =      0
      , margin =     margin()
      , debug =      FALSE)) 

# ggsave(str_c(.figDir, "plotBar.png"), plotBar, width = 50, height = 45, units = "mm", dpi = 300)

# patchwork setup
# area(t, l, b = t, r = l)
layout <- c(
  area(1, 1, 2, 3)
  , area(3, 1, 3, 3)
  )
plot(layout)

betaCamoes <- plotTs + plotBar + plot_layout(design = layout)




# 4 save plot -----------------------------------------------------------------
# png
ggsave(str_c(.figDir, "beta_Camoes_2020", ".png"), betaCamoes, width = 216, height = 148, units = "mm", dpi = 300)
# vector
ggsave(str_c(.figDir, "beta_Camoes_2020", ".svg"), betaCamoes, width = 216, height = 148, units = "mm", dpi = 300)




# 5 plot annotations ----------------------------------------------------------
# observed deaths week 10 onward
a <- wkDthsTot %>%
  filter(isoYr == 2020, isoWk >= 10) %>% 
  summarise(dths = sum(dths))

# expected deaths (mean 2015-19)
b <- expDths %>% 
  filter(isoWk >= 10) %>% 
  summarise(mnDths = sum(mnDths))

# excess deaths
a - b  # 79,578
a / b  # 19%

# official Covid deaths
wkCovidDthsTot %>%
  filter(isoYr == 2020) %>%
  summarise(dths = sum(dths))
# 80,830




# 6 musings ---------------------------------------------------------------
wkDthsGrp <- wkDthsTot %>%
  filter(isoYr == 2020) %>% 
  left_join(
    wkCovidDthsTot %>%
      filter(isoYr == 2020) %>% 
      rename(covidDths = dths)
    , by = c("isoWk", "isoYr", "isoYrWk")) %>%
  mutate(ncDths = dths - covidDths) %>%
  select(-dths) %>% 
  pivot_longer(c(covidDths, ncDths), names_to = "dthsGrp", values_to = "dths")

ggplot(wkDthsGrp) +
  geom_bar(aes(x = isoWk, y = dths, group = dthsGrp, fill = dthsGrp), stat = "identity") +
  geom_errorbar(aes(x = isoWk, ymin = mnDths, ymax = mnDths), data = expDths)

cumCovidDths <- wkCovidDthsTot %>%
  filter(isoYr == 2020) %>% 
  group_by(isoWk) %>%
  summarise(dths = sum(dths)) %>% 
  rename(covidDths = dths) %>% 
  mutate(cumCovidDths = cumsum(covidDths))

ggplot(cumCovidDths) +
  geom_line(aes(x = isoWk, y = cumCovidDths))

dths19 <- wkDthsTot %>% 
  filter(isoYr == 2019) %>% 
  rename(dths19 = dths) %>% 
  select(isoWk, dths19)

dthsIdx <- wkDthsTot %>%
  filter(isoYr == 2020, isoWk != 53) %>% 
  select(isoWk, dths) %>% 
  left_join(dths19, by = "isoWk") %>% 
  mutate(dthsIdx = dths / dths19) %>%  
  mutate(
    lci = map_dbl(dths19, ~poisson.test(.x)$conf.int[1])
    , uci = map_dbl(dths19, ~poisson.test(.x)$conf.int[2])
  ) %>% 
  mutate(lciIdx = lci / dths19, uciIdx = uci / dths19)

ggplot(dthsIdx) +
  geom_line(aes(x = isoWk, y = dthsIdx)) +
  geom_hline(aes(yintercept = 1L), linetype = "4242") +
  geom_ribbon(aes(x = isoWk, ymin = lciIdx, ymax = uciIdx), fill = "grey", alpha = .5) +
  scale_y_continuous(name = NULL)

