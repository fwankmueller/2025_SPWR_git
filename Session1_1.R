##### Setup -------------------------
install.packages("Require")

# Load/install main packages at start (others later)
Require::Require(c("data.table", "tidyverse"))



##### READ data --------------------
# all_daytime_good <- fread("~/work/2025-spwr-session1/drought-2018-ICOS-FLUXNETformat_allDaytimeGood.csv")      # takes about 1.5 minutes
all_daytime_good <- readRDS("~/work/2025-spwr-session1/drought-2018-ICOS-FLUXNETformat_allDaytimeGood.RDS")    # takes 2 seconds

# # locally for me:
# all_daytime_good <- readRDS("~/R/Teaching/2025_SoilPlantWaterRelationsExercises/flux-towers/drought-2018-ICOS-FLUXNETformat_allDaytimeGood.RDS")


##### WRANGLE data -----------------
# you may check the data such as how many data points there are per sites (data.table way)
all_daytime_good[, .N, site]
# How many sites are present? Which site has the most data?



##### COMPUTE new (your job!) ------
# EF calculation
all_daytime_good[LE_F_MDS > 0 & H_F_MDS > 0 & (NETRAD > 0 | is.na(NETRAD)), EF := ]

# save new data? --> download (More > Export ...)
# save script --> download (More > Export ...)



##### PLOTS -----------------------
### PLOT examples CH-Cha
# EF ~ SWC
g2 <-
  all_daytime_good[site %in% c("FLX_CH-Cha")] %>%
  ggplot(aes(x = SWC_F_MDS_1, y = EF, col = VPD_F_MDS/10)) +    # /10 because VPD_F_MDS is in hPa, but we like kPa :)
  # geom_col() +
  geom_point() +
  scale_colour_viridis_c(option = "turbo", name = "VPD (kPa)", limits = c(0,5)) +
  facet_wrap(~site, scales = "free") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 1), expand = T) +
  theme()
g2

# Monthly Temperatures Ridgeplot
Require::Require("ggridges")
g3 <-
  all_daytime_good[site %in% c("FLX_CH-Cha")]%>%
  ggplot(aes(
    x = TA_F_MDS,
    y = factor(data.table::month(day)),
    fill = after_stat(x)
  )) +
  ggridges::geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [°C]", option = "C") +
  facet_wrap(~site, scales = "free") +
  labs(y = "Month",
       x = "Temp (°C)") +
  # coord_cartesian(xlim = c(-10, 40)) +
  theme_bw() +
  theme(
    legend.position = "none",
    #"top",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
g3


# save MAIN plots --> download
ggsave(plot = g2, filename = "~/work/EF~SM.pdf", device = cairo_pdf,
       width = 180, height = 170, units = "mm",
       scale = 1)
ggsave(plot = g3, filename = "~/work/Temp-season.pdf", device = cairo_pdf,
       width = 180, height = 170, units = "mm",
       scale = 1)



##### Before it's too late:
# save new data? --> download (More > Export ...)
# save script --> download (More > Export ...)
# save Plots --> download (More > Export ...)
# --> You can export all at once (.zip) by ticking them all before pressing Export ...






########## ----------------ADD-ON ---- or next week!-------------------------------------------
###
# ----------------------- Dry down calculations ------------------------------------------------
# average SM over a full day
EFs_daily <-
  all_daytime_good %>%
  .[, .(SWC_F_MDS_1 = mean(SWC_F_MDS_1, na.rm = TRUE)), by = .(site, day)]
setorder(EFs_daily, site, day)

# flag decreases
EFs_daily[, decrease := shift(SWC_F_MDS_1) > SWC_F_MDS_1, by = site]
# potentially, be less strict due to imprecision of SWC sensors: ---> # EFs_daily[, decrease := shift(round(SWC_F_MDS_1, 2)) >= round(SWC_F_MDS_1, 2), by = site]

# run IDs for consecutive streaks
EFs_daily[, run_id := rleid(decrease), by = site]

# keep only runs of at least 10 consecutive decreases
EFs_daily[, is_drydown := decrease & .N >= 10, by = .(site, run_id)]

# extract dry-down runs
drydown_days <- EFs_daily[is_drydown == TRUE]

# How many dry-down days are there in summer months? --> 636
drydown_days[is_drydown == T & format(day, format = "%m") %in% c("06", "07", "08"),
             run_id %>% unique(), .(site)] %>% .[, .N]

# Get start–end dates for each dry-down
drydown_events <-
  EFs_daily[is_drydown == TRUE, .(start = min(day), end = max(day)), by = .(site, run_id)]

# How many dry-downs events now? --> 459 (compared to Fu's 428)
summer_events <-
  drydown_events[format(start, format = "%m") %in% c("06", "07", "08") &
                   format(end, format = "%m") %in% c("06", "07", "08")]

# Subset the original high-resolution data for those events
EFs_drydown <-
  EFs_easy[drydown_events,
           on = .(site, day >= start, day <= end),
           nomatch = 0L,
           allow.cartesian = TRUE]

EFs_summer <-
  EFs_easy[summer_events,
           on = .(site, day >= start, day <= end),
           nomatch = 0L,
           allow.cartesian = TRUE]
