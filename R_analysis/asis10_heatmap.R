# Heatmap of Covid mortality by age group over time
# What effect will Covid-19 have on key PH measures? (708)
# author Paul Seamer
# last edit "2021-03-04 09:45:40 GMT"
Sys.time()


# README ----------------------------------------------------------------------
# 


# in this script --------------------------------------------------------------
# 1 read
# 2 calculate xs deaths
# 3 heatmap




# 1 read ----------------------------------------------------------------------
wkDthsAgeGrp <- readRDS(str_c(.datDir, "dat05_age_grp_weekly_deaths_2010_to_2021.RDS"))




# 2 calculate xs deaths -------------------------------------------------------
# this is for full year 2020
expDthsMn5 <- wkDthsAgeGrp %>%
  filter(isoYr %in% c(2015:2019), ageGrp != "under1") %>% 
  group_by(isoWk, isoYr, isoYrWk, ageGrp) %>%
  summarise(dths = sum(dths)) %>%
  ungroup() %>% 
  select(isoWk, ageGrp, dths) %>% 
  group_by(isoWk, ageGrp) %>% 
  summarise(mn5dths = mean(dths)) %>% 
  ungroup() %>% 
  mutate(
    isoYr = 2020
    , isoYrWk = str_c("2020", "-W", formatC(isoWk, width = 2, format = "d", flag = "0")))

trueDths <- wkDthsAgeGrp %>%
  filter(isoYr == 2020, ageGrp != "under1") %>% 
  group_by(isoWk, isoYr, isoYrWk, ageGrp) %>%
  summarise(dths = sum(dths)) %>%
  ungroup() %>% 
  select(isoWk, ageGrp, dths) %>% 
  mutate(
    isoYr = 2020
    , isoYrWk = str_c("2020", "-W", formatC(isoWk, width = 2, format = "d", flag = "0")))




# 3 heatmap -------------------------------------------------------------------
plotDatP <- expDthsMn5 %>% 
  left_join(trueDths, by = c("isoWk", "isoYr", "isoYrWk", "ageGrp")) %>% 
  mutate(pScore = (dths - mn5dths) / dths)

# plot weekly P score
ggplot(plotDatP) +
  geom_line(aes(x = isoWk, y = pScore, group = 1L)) +
  facet_wrap(vars(ageGrp)) +
  theme(
    strip.text = element_text(
      size = rel(.8), hjust = 0, vjust = .5
      , margin = margin(t = 8 * .5, l = 8 * .5))
  )

# plot heatmap
# https://stackoverflow.com/questions/37482977/what-is-a-good-palette-for-divergent-colors-in-r-or-can-viridis-and-magma-b
ncol <- 100
coolwarm_breaks <- seq(-60, 60, length.out = ncol+1)

plotDatHm <- plotDatP %>%
  filter(ageGrp != "01-14") %>% 
  mutate(cutPscore = cut(pScore*100, breaks = coolwarm_breaks))

cw <- colorspace::diverging_hcl(
  ncol, h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))

ggplot(plotDatHm) +
  geom_tile(aes(x = isoWk, y = ageGrp, fill = cutPscore)
            , show.legend = FALSE) +
  scale_fill_manual(values = cw, drop = FALSE)

