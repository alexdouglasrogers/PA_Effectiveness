#### WDBPA Explore #####

library(tidyverse)

base_path <- "/Users/alexrogers/Desktop/Global Anaysis /sample_design/inputs"
df <- read_csv(file.path(base_path, "WDPA_Sep2026_Public_csv.csv"))


l <- as.numeric(length(df$TYPE))
df <- df[df$REALM != "Marine",]
print(paste(
  "Marine realm dropped: n obs = ", l - length(df$TYPE),
  paste0("(", round(100 * (l - length(df$TYPE)) / l, 1), "%)"),
  ": Retained", length(df$TYPE)
))


l2 <- as.numeric(length(df$TYPE))
df <- df[df$STATUS %in% c("Designated", "Established"), ]      
print(paste(
  "Non-designated/established dropped: n obs = ", l2 - length(df$TYPE),
  paste0("(", round(100 * (l2 - length(df$TYPE)) / l2, 1), "%)"),
  ": Retained", length(df$TYPE)
))      

hist(df$STATUS_YR[df$STATUS_YR >= 1950 & df$STATUS_YR <= 2025], main = "N parks established by year")

pie(table(df$DESIG_TYPE),
    main = "Protected Areas by Designation Type")

pie(table(df$IUCN_CAT),
    main = "Protected Areas by IUCN Category")

pie(table(df$GOV_TYPE),
    main = "Protected Areas by Governmant Type")

pie(table(df$OWN_TYPE),
    main = "Protected Areas by Ownership")


library(dplyr)
library(tidyr)
library(ggplot2)

plot_df <- df %>%
  select(DESIG_TYPE, IUCN_CAT, GOV_TYPE, OWN_TYPE) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  filter(!is.na(Category)) %>%
  count(Variable, Category) %>%
  mutate(
    Variable = recode(
      Variable,
      DESIG_TYPE = "Designation Type",
      IUCN_CAT   = "IUCN Category",
      GOV_TYPE   = "Governance Type",
      OWN_TYPE   = "Ownership"
    )
  )

library(dplyr)
library(tidyr)
library(ggplot2)

#--------------------------------------------------
# Prepare data
#--------------------------------------------------

plot_df <- df %>%
  
  select(DESIG_TYPE, IUCN_CAT, GOV_TYPE, OWN_TYPE) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  
  filter(!is.na(Category)) %>%
  
  count(Variable, Category) %>%
  
  # Rename facets
  mutate(
    Variable = recode(
      Variable,
      DESIG_TYPE = "Designation Type",
      IUCN_CAT   = "IUCN Category",
      GOV_TYPE   = "Governance Type",
      OWN_TYPE   = "Ownership"
    )
  ) %>%
  
  # Give IUCN categories their full names
  mutate(
    Category = case_when(
      Variable == "IUCN Category" & Category == "Ia" ~
        "Ia — Strict Nature Reserve",
      
      Variable == "IUCN Category" & Category == "Ib" ~
        "Ib — Wilderness Area",
      
      Variable == "IUCN Category" & Category == "II" ~
        "II — National Park",
      
      Variable == "IUCN Category" & Category == "III" ~
        "III — Natural Monument or Feature",
      
      Variable == "IUCN Category" & Category == "IV" ~
        "IV — Habitat/Species Management Area",
      
      Variable == "IUCN Category" & Category == "V" ~
        "V — Protected Landscape/Seascape",
      
      Variable == "IUCN Category" & Category == "VI" ~
        "VI — Sustainable Use of Natural Resources",
      
      TRUE ~ Category
    )
  ) %>%
  
  # Create unique category names for each facet
  mutate(
    Category_facet = paste(Category, Variable, sep = "___")
  ) %>%
  
  # Order bars independently within each facet
  group_by(Variable) %>%
  arrange(n, .by_group = TRUE) %>%
  mutate(
    Category_facet = factor(
      Category_facet,
      levels = unique(Category_facet)
    )
  ) %>%
  
  ungroup()


#--------------------------------------------------
# Plot
#--------------------------------------------------

ggplot(plot_df, aes(x = n, y = Category_facet)) +
  
  geom_col() +
  
  facet_wrap(
    ~Variable,
    scales = "free",
    ncol = 2
  ) +
  
  # Remove facet identifier from category labels
  scale_y_discrete(
    labels = function(x) sub("___.*$", "", x)
  ) +
  
  # Use commas rather than scientific notation
  scale_x_continuous(
    labels = scales::comma
  ) +
  
  labs(
    title = "Protected Area Characteristics",
    x = "Number of Protected Areas",
    y = NULL
  ) +
  
  theme_bw() +
  
  theme(
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    axis.text.y = element_text(
      size = 9
    ),
    plot.title = element_text(
      size = 14,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 10
    )
  )














library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ============================================================
# 1. PREPARE TIMELINE DATA
# ============================================================

timeline_df <- df %>%
  
  # Keep years 1950–2024
  filter(
    !is.na(STATUS_YR),
    STATUS_YR >= 1950,
    STATUS_YR <= 2024
  ) %>%
  
  select(
    STATUS_YR,
    DESIG_TYPE,
    IUCN_CAT,
    GOV_TYPE,
    OWN_TYPE
  ) %>%
  
  # Convert the four PA characteristics to long format
  pivot_longer(
    cols = c(DESIG_TYPE, IUCN_CAT, GOV_TYPE, OWN_TYPE),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  
  # Clean missing categories and rename variables
  mutate(
    Category = case_when(
      is.na(Category) ~ "Not Reported",
      Category == "NA" ~ "Not Reported",
      Category == "" ~ "Not Reported",
      TRUE ~ Category
    ),
    
    Variable = recode(
      Variable,
      DESIG_TYPE = "Designation Type",
      IUCN_CAT   = "IUCN Category",
      GOV_TYPE   = "Governance Type",
      OWN_TYPE   = "Ownership"
    )
  ) %>%
  
  # Number of PAs receiving status in each year
  count(
    Variable,
    Category,
    STATUS_YR,
    name = "n_new"
  ) %>%
  
  # Calculate cumulative count within each category
  group_by(
    Variable,
    Category
  ) %>%
  
  arrange(STATUS_YR, .by_group = TRUE) %>%
  
  mutate(
    n_cumulative = cumsum(n_new)
  ) %>%
  
  ungroup()


# ============================================================
# 2. FUNCTION FOR INDIVIDUAL TIMELINE PLOTS
# ============================================================

make_timeline <- function(data, variable_name) {
  
  ggplot(
    data %>% filter(Variable == variable_name),
    aes(
      x = STATUS_YR,
      y = n_cumulative,
      fill = Category
    )
  ) +
    
    # Stacked cumulative categories
    geom_area(
      position = "stack",
      alpha = 0.85
    ) +
    
    # Overall cumulative PA total
    stat_summary(
      aes(group = 1),
      fun = sum,
      geom = "line",
      linewidth = 0.7,
      colour = "black"
    ) +
    
    # X axis
    scale_x_continuous(
      breaks = c(seq(1950, 2020, 10), 2024),
      limits = c(1950, 2024),
      expand = c(0, 0)
    ) +
    
    # Y axis
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, 280000),
      expand = expansion(mult = c(0, 0.02))
    ) +
    
    labs(
      title = variable_name,
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    
    theme_bw() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11,
        hjust = 0.5
      ),
      
      legend.position = "bottom",
      
      legend.text = element_text(
        size = 7
      ),
      
      legend.key.size = grid::unit(
        0.35,
        "cm"
      ),
      
      panel.grid.minor = element_blank()
    ) +
    
    guides(
      fill = guide_legend(
        ncol = 2,
        byrow = TRUE
      )
    )
}


# ============================================================
# 3. CREATE FOUR INDIVIDUAL PLOTS
# ============================================================

p1 <- make_timeline(
  timeline_df,
  "Designation Type"
)

p2 <- make_timeline(
  timeline_df,
  "Governance Type"
)

p3 <- make_timeline(
  timeline_df,
  "IUCN Category"
)

p4 <- make_timeline(
  timeline_df,
  "Ownership"
)


# ============================================================
# 4. COMBINE INTO 2 × 2 FIGURE
# ============================================================

final_plot <- (p1 + p2) /
  (p3 + p4) +
  
  plot_annotation(
    title = "Growth of the Global Protected Area Network",
    subtitle = "Cumulative number of protected areas by characteristic, 1950–2024"
  ) &
  
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )


# ============================================================
# 5. DISPLAY
# ============================================================

final_plot









######################### 2 ################################


library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(colorspace)

# ============================================================
# 1. PREPARE TIMELINE DATA — REPORTED AREA
# ============================================================

timeline_area_df <- df %>%
  filter(
    !is.na(STATUS_YR),
    STATUS_YR >= 1950,
    STATUS_YR <= 2024
  ) %>%
  
  select(
    STATUS_YR,
    REP_AREA,
    DESIG_TYPE,
    IUCN_CAT,
    GOV_TYPE,
    OWN_TYPE
  ) %>%
  
  pivot_longer(
    cols = c(DESIG_TYPE, IUCN_CAT, GOV_TYPE, OWN_TYPE),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  
  mutate(
    Category = case_when(
      is.na(Category) ~ "Not Reported",
      Category == "NA" ~ "Not Reported",
      Category == "" ~ "Not Reported",
      TRUE ~ Category
    ),
    
    Variable = recode(
      Variable,
      DESIG_TYPE = "Designation Type",
      IUCN_CAT   = "IUCN Category",
      GOV_TYPE   = "Governance Type",
      OWN_TYPE   = "Ownership"
    )
  ) %>%
  
  # Area newly protected each year
  group_by(
    Variable,
    Category,
    STATUS_YR
  ) %>%
  
  summarise(
    area_new = sum(REP_AREA),
    .groups = "drop"
  ) %>%
  
  # IMPORTANT: fill missing years WITHIN each category
  group_by(
    Variable,
    Category
  ) %>%
  
  complete(
    STATUS_YR = 1950:2024,
    fill = list(area_new = 0)
  ) %>%
  
  arrange(STATUS_YR, .by_group = TRUE) %>%
  
  # Now cumulative area can only stay constant or increase
  mutate(
    area_cumulative = cumsum(area_new)
  ) %>%
  
  ungroup()


# ============================================================
# 2. FUNCTION FOR INDIVIDUAL AREA TIMELINES
# ============================================================

make_area_timeline <- function(data, variable_name) {
  
  # Data for this panel
  plot_data <- data %>%
    filter(Variable == variable_name)
  
  # Categories present in this panel
  categories <- sort(unique(plot_data$Category))
  
  # Give Not Reported grey; generate distinct colors for everything else
  real_categories <- setdiff(categories, "Not Reported")
  
  category_colors <- colorspace::qualitative_hcl(
    length(real_categories),
    palette = "Dark 3"
  )
  
  names(category_colors) <- real_categories
  
  # Add grey for missing/not reported
  if ("Not Reported" %in% categories) {
    category_colors <- c(
      category_colors,
      "Not Reported" = "grey70"
    )
  }
  
  
  ggplot(
    plot_data,
    aes(
      x = STATUS_YR,
      y = area_cumulative / 1e6,
      fill = Category
    )
  ) +
    
    # Stacked cumulative reported area
    geom_area(
      position = "stack",
      alpha = 0.9
    ) +
    
    # Black total cumulative curve
    stat_summary(
      aes(group = 1),
      fun = sum,
      geom = "line",
      linewidth = 0.8,
      colour = "black"
    ) +
    
    # Explicit high-contrast colors
    scale_fill_manual(
      values = category_colors
    ) +
    
    scale_x_continuous(
      breaks = seq(1950, 2020, 10),
      limits = c(1950, 2024),
      expand = c(0, 0)
    ) +
    
    scale_y_continuous(
      labels = scales::label_number(
        accuracy = 1
      ),
      expand = expansion(
        mult = c(0, 0.03)
      )
    ) +
    
    labs(
      title = variable_name,
      x = NULL,
      y = "Cumulative Reported Area (million km²)",
      fill = NULL
    ) +
    
    theme_bw() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 11,
        hjust = 0.5
      ),
      
      axis.title.y = element_text(
        size = 9
      ),
      
      legend.position = "bottom",
      
      legend.text = element_text(
        size = 7
      ),
      
      legend.key.size = grid::unit(
        0.4,
        "cm"
      ),
      
      panel.grid.minor = element_blank()
    ) +
    
    guides(
      fill = guide_legend(
        ncol = 2,
        byrow = TRUE,
        
        # Make legend swatches fully opaque
        override.aes = list(
          alpha = 1
        )
      )
    )
}


# ============================================================
# 3. CREATE FOUR INDIVIDUAL PLOTS
# ============================================================

p1_area <- make_area_timeline(
  timeline_area_df,
  "Designation Type"
)

p2_area <- make_area_timeline(
  timeline_area_df,
  "Governance Type"
)

p3_area <- make_area_timeline(
  timeline_area_df,
  "IUCN Category"
)

p4_area <- make_area_timeline(
  timeline_area_df,
  "Ownership"
)


# ============================================================
# 4. COMBINE INTO 2 × 2 FIGURE
# ============================================================

final_area_plot <- (p1_area + p2_area) /
  (p3_area + p4_area) +
  
  plot_annotation(
    title = "Growth of the Global Protected Area Network",
    subtitle = paste0(
      "Cumulative reported protected area by characteristic, 1950–2024 "
    )
  ) &
  
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )


# ============================================================
# 5. DISPLAY
# ============================================================

final_area_plot



















library(dplyr)
library(tidyr)

#############################
# ============================================================
# EXPLORE LEVELS WITHIN EACH PA CHARACTERISTIC
# ============================================================
#############################

characteristic_summary <- df %>%
  
  select(
    REP_AREA,
    DESIG_TYPE,
    IUCN_CAT,
    GOV_TYPE,
    OWN_TYPE
  ) %>%
  
  pivot_longer(
    cols = c(
      DESIG_TYPE,
      IUCN_CAT,
      GOV_TYPE,
      OWN_TYPE
    ),
    names_to = "Characteristic",
    values_to = "Category"
  ) %>%
  
  mutate(
    # Standardize missing values
    Category = case_when(
      is.na(Category) ~ "Not Reported",
      Category == "" ~ "Not Reported",
      Category == "NA" ~ "Not Reported",
      TRUE ~ Category
    ),
    
    # Cleaner characteristic names
    Characteristic = recode(
      Characteristic,
      DESIG_TYPE = "Designation Type",
      IUCN_CAT   = "IUCN Category",
      GOV_TYPE   = "Governance Type",
      OWN_TYPE   = "Ownership"
    )
  ) %>%
  
  group_by(
    Characteristic,
    Category
  ) %>%
  
  summarise(
    n_parks = n(),
    area_km2 = sum(REP_AREA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  arrange(
    Characteristic,
    desc(n_parks)
  )

# ============================================================
# 1. HOW MANY FACTOR LEVELS ARE IN EACH CHARACTERISTIC?
# ============================================================

factor_counts <- characteristic_summary %>%
  group_by(Characteristic) %>%
  summarise(
    n_categories = n(),
    n_categories_reported = sum(Category != "Not Reported"),
    .groups = "drop"
  )

print(factor_counts)


# ============================================================
# 2. WHAT ARE THE ACTUAL CATEGORIES?
# ============================================================

print(
  characteristic_summary,
  n = Inf
)


# ============================================================
# 3. OPTIONAL: COMPACT LIST OF CATEGORY NAMES
# ============================================================

category_names <- characteristic_summary %>%
  filter(Category != "Not Reported") %>%
  group_by(Characteristic) %>%
  summarise(
    categories = paste(Category, collapse = " | "),
    .groups = "drop"
  )

print(category_names)


library(dplyr)
library(tidyr)
library(gt)
library(scales)

# ============================================================
# CREATE SUMMARY TABLE
# ============================================================

pa_characteristic_table <- df %>%
  
  select(
    REP_AREA,
    DESIG_TYPE,
    IUCN_CAT,
    GOV_TYPE,
    OWN_TYPE
  ) %>%
  
  pivot_longer(
    cols = c(
      DESIG_TYPE,
      IUCN_CAT,
      GOV_TYPE,
      OWN_TYPE
    ),
    names_to = "Characteristic",
    values_to = "Category"
  ) %>%
  
  # Clean categories
  mutate(
    Category = case_when(
      is.na(Category) ~ "Not Reported",
      Category == "" ~ "Not Reported",
      Category == "NA" ~ "Not Reported",
      
      # Add full IUCN category meanings
      Characteristic == "IUCN_CAT" & Category == "Ia" ~
        "Ia — Strict Nature Reserve",
      
      Characteristic == "IUCN_CAT" & Category == "Ib" ~
        "Ib — Wilderness Area",
      
      Characteristic == "IUCN_CAT" & Category == "II" ~
        "II — National Park",
      
      Characteristic == "IUCN_CAT" & Category == "III" ~
        "III — Natural Monument or Feature",
      
      Characteristic == "IUCN_CAT" & Category == "IV" ~
        "IV — Habitat/Species Management Area",
      
      Characteristic == "IUCN_CAT" & Category == "V" ~
        "V — Protected Landscape/Seascape",
      
      Characteristic == "IUCN_CAT" & Category == "VI" ~
        "VI — Protected Area with Sustainable Use of Natural Resources",
      
      TRUE ~ Category
    ),
    
    Characteristic = recode(
      Characteristic,
      DESIG_TYPE = "Designation Type",
      GOV_TYPE   = "Governance Type",
      IUCN_CAT   = "IUCN Category",
      OWN_TYPE   = "Ownership"
    )
  ) %>%
  
  # Summarize
  group_by(
    Characteristic,
    Category
  ) %>%
  
  summarise(
    n_parks = n(),
    area_km2 = sum(REP_AREA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Calculate shares WITHIN each characteristic
  group_by(Characteristic) %>%
  
  mutate(
    pct_parks = n_parks / sum(n_parks),
    pct_area  = area_km2 / sum(area_km2)
  ) %>%
  
  # Sort largest categories first
  arrange(
    Characteristic,
    desc(area_km2)
  ) %>%
  
  ungroup()


# ============================================================
# MAKE NICE GT TABLE
# ============================================================

pa_characteristic_table_gt <- pa_characteristic_table %>%
  
  gt(
    groupname_col = "Characteristic",
    rowname_col = "Category"
  ) %>%
  
  cols_label(
    n_parks   = "Number of PAs",
    pct_parks = "% of PAs",
    area_km2  = "Area (km²)",
    pct_area  = "% of Area"
  ) %>%
  
  fmt_number(
    columns = n_parks,
    decimals = 0,
    sep_mark = ","
  ) %>%
  
  fmt_number(
    columns = area_km2,
    decimals = 0,
    sep_mark = ","
  ) %>%
  
  fmt_percent(
    columns = c(pct_parks, pct_area),
    decimals = 1
  ) %>%
  
  tab_header(
    title = md("**Global Protected Area Characteristics**"),
    subtitle = "Number and reported area of protected areas by category"
  ) %>%
  
  tab_source_note(
    source_note = "Area based on WDPA reported area (REP_AREA)."
  ) %>%
  
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 18,
    heading.subtitle.font.size = 12,
    row_group.font.weight = "bold",
    table.width = pct(100)
  )

pa_characteristic_table_gt




############
###########
##########

library(dplyr)
library(tidyr)

# ============================================================
# COUNTRY × PA CHARACTERISTIC × CATEGORY DATASET
# ============================================================

country_pa <- df %>%
  
  select(
    ISO3,
    REP_AREA,
    DESIG_TYPE,
    GOV_TYPE,
    IUCN_CAT,
    OWN_TYPE
  ) %>%
  
  pivot_longer(
    cols = c(
      DESIG_TYPE,
      GOV_TYPE,
      IUCN_CAT,
      OWN_TYPE
    ),
    names_to = "Characteristic",
    values_to = "Category"
  ) %>%
  
  mutate(
    
    # Clean missing categories
    Category = case_when(
      is.na(Category) ~ "Not Reported",
      Category == ""  ~ "Not Reported",
      Category == "NA" ~ "Not Reported",
      
      # IUCN labels
      Characteristic == "IUCN_CAT" & Category == "Ia" ~
        "Ia — Strict Nature Reserve",
      
      Characteristic == "IUCN_CAT" & Category == "Ib" ~
        "Ib — Wilderness Area",
      
      Characteristic == "IUCN_CAT" & Category == "II" ~
        "II — National Park",
      
      Characteristic == "IUCN_CAT" & Category == "III" ~
        "III — Natural Monument or Feature",
      
      Characteristic == "IUCN_CAT" & Category == "IV" ~
        "IV — Habitat/Species Management Area",
      
      Characteristic == "IUCN_CAT" & Category == "V" ~
        "V — Protected Landscape/Seascape",
      
      Characteristic == "IUCN_CAT" & Category == "VI" ~
        "VI — Protected Area with Sustainable Use of Natural Resources",
      
      TRUE ~ Category
    ),
    
    Characteristic = recode(
      Characteristic,
      DESIG_TYPE = "Designation Type",
      GOV_TYPE   = "Governance Type",
      IUCN_CAT   = "IUCN Category",
      OWN_TYPE   = "Ownership"
    )
  ) %>%
  
  filter(
    !is.na(ISO3),
    ISO3 != ""
  ) %>%
  
  group_by(
    ISO3,
    Characteristic,
    Category
  ) %>%
  
  summarise(
    n_parks = n(),
    area_km2 = sum(REP_AREA, na.rm = TRUE),
    .groups = "drop"
  )

country_pa


# ============================================================
# HOW GEOGRAPHICALLY WIDESPREAD IS EACH CATEGORY?
# ============================================================

geographic_coverage <- country_pa %>%
  
  group_by(
    Characteristic,
    Category
  ) %>%
  
  summarise(
    n_countries = n_distinct(ISO3),
    n_parks = sum(n_parks),
    area_km2 = sum(area_km2),
    .groups = "drop"
  ) %>%
  
  arrange(
    Characteristic,
    desc(n_countries)
  )

print(
  geographic_coverage,
  n = Inf
)







# ============================================================
# GEOGRAPHIC COVERAGE + COUNTRY CONCENTRATION 
####ALLLLL COUNTRIES!!!!
# ============================================================

geographic_coverage <- country_pa %>%
  
  group_by(
    Characteristic,
    Category
  ) %>%
  
  summarise(
    
    # Number of countries represented
    n_countries = n_distinct(ISO3),
    
    # Share of PA records in the single most represented country
    top_country_share =
      max(n_parks) / sum(n_parks),
    
    # Share of PA records in the five most represented countries
    top5_country_share =
      sum(head(sort(n_parks, decreasing = TRUE), 5)) /
      sum(n_parks),
    
    .groups = "drop"
  )


# ============================================================
# ADD GEOGRAPHIC METRICS TO EXISTING SUMMARY TABLE
# ============================================================

pa_characteristic_table_with_geo <- pa_characteristic_table %>%
  
  left_join(
    geographic_coverage,
    by = c("Characteristic", "Category")
  )


# ============================================================
# MAKE NICE GT TABLE
# ============================================================

pa_characteristic_table_gt <- pa_characteristic_table_with_geo %>%
  
  gt(
    groupname_col = "Characteristic",
    rowname_col = "Category"
  ) %>%
  
  cols_label(
    n_parks            = "Number of PAs",
    n_countries        = "Countries",
    top_country_share  = "Largest Country",
    top5_country_share = "Top 5 Countries",
    area_km2           = "Area (km²)",
    pct_parks          = "% of PAs",
    pct_area           = "% of Area"
  ) %>%
  
  fmt_number(
    columns = c(
      n_parks,
      n_countries
    ),
    decimals = 0,
    sep_mark = ","
  ) %>%
  
  fmt_number(
    columns = area_km2,
    decimals = 0,
    sep_mark = ","
  ) %>%
  
  fmt_percent(
    columns = c(
      pct_parks,
      pct_area,
      top_country_share,
      top5_country_share
    ),
    decimals = 1
  ) %>%
  
  # Arrange columns
  cols_move(
    columns = n_countries,
    after = n_parks
  ) %>%
  
  cols_move(
    columns = top_country_share,
    after = n_countries
  ) %>%
  
  cols_move(
    columns = top5_country_share,
    after = top_country_share
  ) %>%
  
  tab_header(
    title = md("**Global Protected Area Characteristics**"),
    subtitle = "Number, geographic representation, concentration, and reported area of protected areas by category"
  ) %>%
  
  tab_source_note(
    source_note = paste0(
      "Area based on WDPA reported area (REP_AREA). ",
      "Countries based on unique ISO3 codes. ",
      "Largest Country and Top 5 Countries show the share of PA records ",
      "within each category located in the most represented countries."
    )
  ) %>%
  
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 18,
    heading.subtitle.font.size = 12,
    row_group.font.weight = "bold",
    table.width = pct(100)
  )

pa_characteristic_table_gt






library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# ============================================================
# 1. WORLD MAP
# ============================================================

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  select(iso_a3, name, geometry) %>%
  filter(
    name != "Antarctica",
    iso_a3 != "-99"
  )

# ============================================================
# 2. FUNCTION TO MAP AREA (km²) BY ISO3 ENTITY
# ============================================================

make_area_map <- function(
    characteristic_name,
    plot_title,
    ncol = 3,
    exclude_not_reported = TRUE
) {
  
  # ----------------------------------------------------------
  # Filter characteristic
  # ----------------------------------------------------------
  
  map_data <- country_pa %>%
    filter(
      Characteristic == characteristic_name
    )
  
  if (exclude_not_reported) {
    map_data <- map_data %>%
      filter(
        Category != "Not Reported"
      )
  }
  
  # ----------------------------------------------------------
  # Order categories by geographic representation
  # ----------------------------------------------------------
  
  category_order <- map_data %>%
    group_by(Category) %>%
    summarise(
      n_entities = n_distinct(ISO3),
      .groups = "drop"
    ) %>%
    arrange(desc(n_entities)) %>%
    pull(Category)
  
  # ----------------------------------------------------------
  # Sum protected area within country × category
  # ----------------------------------------------------------
  
  map_data <- map_data %>%
    group_by(
      Category,
      ISO3
    ) %>%
    summarise(
      area_km2 = sum(area_km2),
      .groups = "drop"
    ) %>%
    mutate(
      Category = factor(
        Category,
        levels = category_order
      )
    )
  
  # ----------------------------------------------------------
  # Create complete country × category grid
  # ----------------------------------------------------------
  
  map_full <- expand_grid(
    Category = factor(
      category_order,
      levels = category_order
    ),
    iso_a3 = world$iso_a3
  ) %>%
    left_join(
      map_data %>%
        rename(
          iso_a3 = ISO3
        ),
      by = c(
        "Category",
        "iso_a3"
      )
    ) %>%
    mutate(
      area_km2_plot = if_else(
        is.na(area_km2) | area_km2 <= 0,
        NA_real_,
        area_km2
      )
    )
  
  # ----------------------------------------------------------
  # Join to world geometry
  # ----------------------------------------------------------
  
  map_sf <- world %>%
    left_join(
      map_full,
      by = "iso_a3"
    ) %>%
    st_as_sf()
  
  # ----------------------------------------------------------
  # Plot
  # ----------------------------------------------------------
  
  ggplot(map_sf) +
    
    geom_sf(
      aes(fill = area_km2_plot),
      color = "white",
      linewidth = 0.05
    ) +
    
    facet_wrap(
      ~ Category,
      ncol = ncol
    ) +
    
    scale_fill_gradientn(
      
      # Clearly distinguish low values from grey absence
      colours = c(
        "#9ecae1",
        "#4292c6",
        "#08519c"
      ),
      
      na.value = "grey85",
      
      # Compress extreme values while preserving differences
      trans = "sqrt",
      
      # Keep legend labels sparse
      breaks = scales::breaks_pretty(
        n = 3
      ),
      
      # Format as 500K, 1M, 2M etc.
      labels = scales::label_number(
        scale_cut = scales::cut_short_scale(),
        accuracy = 0.1
      ),
      
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        barwidth = grid::unit(10, "cm"),
        barheight = grid::unit(0.55, "cm")
      )
    ) +
    
    coord_sf(
      expand = FALSE
    ) +
    
    labs(
      title = plot_title,
      subtitle = "Shade of blue indicates summed protected area (km²) per ISO3 entity",
      fill = "Area (km²)",
      caption = "Gray indicates no protected areas in that category for the ISO3 entity."
    ) +
    
    theme_bw() +
    
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      
      strip.text = element_text(
        face = "bold",
        size = 10
      ),
      
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      
      plot.subtitle = element_text(
        size = 11
      ),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        size = 11
      ),
      
      legend.text = element_text(
        size = 9
      ),
      
      legend.box.spacing = grid::unit(
        0.4,
        "cm"
      )
    )
}

# ============================================================
# 3. GOVERNANCE TYPE
# ============================================================

p_gov_area <- make_area_map(
  characteristic_name = "Governance Type",
  plot_title = "Global Geographic Representation of Protected Area Governance Types",
  ncol = 3
)

p_gov_area


# ============================================================
# 4. IUCN CATEGORY
# ============================================================

p_iucn_area <- make_area_map(
  characteristic_name = "IUCN Category",
  plot_title = "Global Geographic Representation of IUCN Protected Area Categories",
  ncol = 3
)

p_iucn_area


# ============================================================
# 5. OWNERSHIP
# ============================================================

p_ownership_area <- make_area_map(
  characteristic_name = "Ownership",
  plot_title = "Global Geographic Representation of Protected Area Ownership Types",
  ncol = 3
)

p_ownership_area


# ============================================================
# 6. DESIGNATION TYPE
# ============================================================

p_designation_area <- make_area_map(
  characteristic_name = "Designation Type",
  plot_title = "Global Geographic Representation of Protected Area Designation Types",
  ncol = 2
)

p_designation_area


########## 2009–2019 TABLE + MAPS + ALL-YEARS % COUNTRY MAPS ##########

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(gt)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)


# ============================================================
# 1. HELPER: BUILD COUNTRY × CHARACTERISTIC DATA
#
# Geographic area is based on GIS_AREA rather than REP_AREA.
#
# GIS_AREA   = mapped PA area (km²)
# GIS_M_AREA = mapped marine area (km²)
#
# Terrestrial area = GIS_AREA - GIS_M_AREA
# ============================================================

build_country_pa <- function(x) {
  
  x %>%
    
    mutate(
      GIS_AREA   = as.numeric(GIS_AREA),
      GIS_M_AREA = as.numeric(GIS_M_AREA),
      
      GIS_LAND_AREA = case_when(
        
        !is.na(GIS_AREA) & !is.na(GIS_M_AREA) ~
          pmax(GIS_AREA - GIS_M_AREA, 0),
        
        !is.na(GIS_AREA) ~ GIS_AREA,
        
        TRUE ~ NA_real_
      )
    ) %>%
    
    select(
      ISO3,
      GIS_AREA,
      GIS_LAND_AREA,
      DESIG_TYPE,
      GOV_TYPE,
      IUCN_CAT,
      OWN_TYPE
    ) %>%
    
    pivot_longer(
      cols = c(
        DESIG_TYPE,
        GOV_TYPE,
        IUCN_CAT,
        OWN_TYPE
      ),
      names_to = "Characteristic",
      values_to = "Category"
    ) %>%
    
    mutate(
      
      Category = case_when(
        
        is.na(Category) |
          Category == "" |
          Category == "NA" ~
          "Not Reported",
        
        Characteristic == "IUCN_CAT" & Category == "Ia" ~
          "Ia — Strict Nature Reserve",
        
        Characteristic == "IUCN_CAT" & Category == "Ib" ~
          "Ib — Wilderness Area",
        
        Characteristic == "IUCN_CAT" & Category == "II" ~
          "II — National Park",
        
        Characteristic == "IUCN_CAT" & Category == "III" ~
          "III — Natural Monument or Feature",
        
        Characteristic == "IUCN_CAT" & Category == "IV" ~
          "IV — Habitat/Species Management Area",
        
        Characteristic == "IUCN_CAT" & Category == "V" ~
          "V — Protected Landscape/Seascape",
        
        Characteristic == "IUCN_CAT" & Category == "VI" ~
          "VI — Protected Area with Sustainable Use of Natural Resources",
        
        TRUE ~ Category
      ),
      
      Characteristic = recode(
        Characteristic,
        DESIG_TYPE = "Designation Type",
        GOV_TYPE   = "Governance Type",
        IUCN_CAT   = "IUCN Category",
        OWN_TYPE   = "Ownership"
      )
    ) %>%
    
    filter(
      !is.na(ISO3),
      ISO3 != ""
    ) %>%
    
    group_by(
      ISO3,
      Characteristic,
      Category
    ) %>%
    
    summarise(
      n_parks = n(),
      
      area_km2 = sum(
        GIS_AREA,
        na.rm = TRUE
      ),
      
      land_area_km2 = sum(
        GIS_LAND_AREA,
        na.rm = TRUE
      ),
      
      n_with_gis_area = sum(
        !is.na(GIS_AREA)
      ),
      
      .groups = "drop"
    )
}


# ============================================================
# 2. CREATE 2009–2019 AND ALL-YEARS DATA
# ============================================================

country_pa_0919 <- df %>%
  filter(
    !is.na(STATUS_YR),
    STATUS_YR >= 2009,
    STATUS_YR <= 2019
  ) %>%
  build_country_pa()

country_pa_all <- build_country_pa(df)


# ============================================================
# 3. 2009–2019 SUMMARY TABLE
# ============================================================

pa_characteristic_0919 <- country_pa_0919 %>%
  
  group_by(
    Characteristic,
    Category
  ) %>%
  
  summarise(
    
    total_n_parks = sum(
      n_parks,
      na.rm = TRUE
    ),
    
    total_area_km2 = sum(
      area_km2,
      na.rm = TRUE
    ),
    
    n_countries = n_distinct(ISO3),
    
    top_country_share =
      max(n_parks, na.rm = TRUE) /
      sum(n_parks, na.rm = TRUE),
    
    top5_country_share =
      sum(
        head(
          sort(
            n_parks,
            decreasing = TRUE
          ),
          5
        )
      ) /
      sum(n_parks, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  
  rename(
    n_parks = total_n_parks,
    area_km2 = total_area_km2
  ) %>%
  
  group_by(
    Characteristic
  ) %>%
  
  mutate(
    pct_parks =
      n_parks / sum(n_parks),
    
    pct_area =
      area_km2 / sum(area_km2)
  ) %>%
  
  ungroup() %>%
  
  arrange(
    Characteristic,
    desc(area_km2)
  )


# ============================================================
# 4. NICE 2009–2019 TABLE
# ============================================================

pa_characteristic_0919_gt <- pa_characteristic_0919 %>%
  
  select(
    Characteristic,
    Category,
    n_parks,
    n_countries,
    top_country_share,
    top5_country_share,
    area_km2,
    pct_parks,
    pct_area
  ) %>%
  
  gt(
    groupname_col = "Characteristic",
    rowname_col = "Category"
  ) %>%
  
  cols_label(
    n_parks            = "Number of PAs",
    n_countries        = "Countries",
    top_country_share  = "Largest Country",
    top5_country_share = "Top 5 Countries",
    area_km2           = "GIS Area (km²)",
    pct_parks          = "% of PAs",
    pct_area           = "% of Area"
  ) %>%
  
  fmt_number(
    columns = c(
      n_parks,
      n_countries
    ),
    decimals = 0,
    sep_mark = ","
  ) %>%
  
  fmt_number(
    columns = area_km2,
    decimals = 0,
    sep_mark = ","
  ) %>%
  
  fmt_percent(
    columns = c(
      top_country_share,
      top5_country_share,
      pct_parks,
      pct_area
    ),
    decimals = 1
  ) %>%
  
  tab_header(
    title = md(
      "**Global Protected Area Characteristics, 2009–2019**"
    ),
    subtitle =
      "Protected areas receiving status from 2009 through 2019"
  ) %>%
  
  tab_source_note(
    source_note = paste0(
      "Area based on WDPA GIS-derived area (GIS_AREA). ",
      "Countries based on unique ISO3 codes. ",
      "Largest Country and Top 5 Countries are shares of PA records. ",
      "GIS areas are summed and therefore do not remove spatial overlap."
    )
  ) %>%
  
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 18,
    heading.subtitle.font.size = 12,
    row_group.font.weight = "bold",
    table.width = pct(100)
  )

pa_characteristic_0919_gt


# ============================================================
# 5. WORLD MAP
# ============================================================

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  
  select(
    iso_a3,
    name,
    geometry
  ) %>%
  
  filter(
    name != "Antarctica",
    iso_a3 != "-99"
  )


# ============================================================
# 6. COLOUR PALETTES
# ============================================================

# Governance = blue
gov_cols <- c(
  "#bdd7e7",
  "#3182bd",
  "#08519c"
)

# IUCN = red
iucn_cols <- c(
  "#fcbba1",
  "#ef3b2c",
  "#99000d"
)

# Ownership = darkish yellow / mustard
own_cols <- c(
  "#f6e8a6",
  "#d4a017",
  "#8c6d1f"
)

# Designation = darkish green
desig_cols <- c(
  "#c7e9c0",
  "#31a354",
  "#006d2c"
)


# ============================================================
# 7. FUNCTION: MAP SUMMED GIS AREA (km²)
# ============================================================

make_area_map <- function(
    pa_data,
    characteristic_name,
    plot_title,
    ncol = 3,
    exclude_not_reported = TRUE,
    colours = gov_cols
) {
  
  map_data <- pa_data %>%
    filter(
      Characteristic == characteristic_name
    )
  
  if (exclude_not_reported) {
    map_data <- map_data %>%
      filter(
        Category != "Not Reported"
      )
  }
  
  category_order <- map_data %>%
    group_by(Category) %>%
    summarise(
      n_entities = n_distinct(ISO3),
      .groups = "drop"
    ) %>%
    arrange(
      desc(n_entities)
    ) %>%
    pull(Category)
  
  map_data <- map_data %>%
    group_by(
      Category,
      ISO3
    ) %>%
    summarise(
      area_km2 = sum(
        area_km2,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      Category = factor(
        Category,
        levels = category_order
      )
    )
  
  map_full <- expand_grid(
    
    Category = factor(
      category_order,
      levels = category_order
    ),
    
    iso_a3 = world$iso_a3
    
  ) %>%
    left_join(
      map_data %>%
        rename(
          iso_a3 = ISO3
        ),
      by = c(
        "Category",
        "iso_a3"
      )
    ) %>%
    mutate(
      area_plot = if_else(
        is.na(area_km2) |
          area_km2 <= 0,
        NA_real_,
        area_km2
      )
    )
  
  map_sf <- world %>%
    left_join(
      map_full,
      by = "iso_a3"
    ) %>%
    st_as_sf()
  
  ggplot(map_sf) +
    
    geom_sf(
      aes(
        fill = area_plot
      ),
      color = "white",
      linewidth = 0.05
    ) +
    
    facet_wrap(
      ~Category,
      ncol = ncol
    ) +
    
    scale_fill_gradientn(
      
      colours = colours,
      
      na.value = "grey85",
      
      trans = "sqrt",
      
      breaks = scales::breaks_pretty(
        n = 3
      ),
      
      labels = scales::label_number(
        scale_cut = scales::cut_short_scale(),
        accuracy = 0.1
      ),
      
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        barwidth = grid::unit(
          10,
          "cm"
        ),
        barheight = grid::unit(
          0.55,
          "cm"
        )
      )
    ) +
    
    coord_sf(
      expand = FALSE
    ) +
    
    labs(
      title = plot_title,
      
      subtitle =
        "Shade intensity indicates summed GIS-derived protected area (km²) per ISO3 entity",
      
      fill = "GIS Area (km²)",
      
      caption =
        "Gray indicates no positive GIS-derived area in that category. Areas are summed and may include spatial overlap."
    ) +
    
    theme_bw() +
    
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      
      strip.text = element_text(
        face = "bold",
        size = 10
      ),
      
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      
      plot.subtitle = element_text(
        size = 11
      ),
      
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9)
    )
}


# ============================================================
# 8. 2009–2019 GIS AREA MAPS
# ============================================================

p_gov_0919 <- make_area_map(
  country_pa_0919,
  "Governance Type",
  "Protected Area Governance Types Established 2009–2019",
  ncol = 3,
  colours = gov_cols
)

p_gov_0919


p_iucn_0919 <- make_area_map(
  country_pa_0919,
  "IUCN Category",
  "IUCN Protected Area Categories Established 2009–2019",
  ncol = 3,
  colours = iucn_cols
)

p_iucn_0919


p_ownership_0919 <- make_area_map(
  country_pa_0919,
  "Ownership",
  "Protected Area Ownership Types Established 2009–2019",
  ncol = 3,
  colours = own_cols
)

p_ownership_0919


p_designation_0919 <- make_area_map(
  country_pa_0919,
  "Designation Type",
  "Protected Area Designation Types Established 2009–2019",
  ncol = 2,
  colours = desig_cols
)

p_designation_0919


# ============================================================
# 9. COUNTRY LAND AREA
#
# Derive country land area directly from Natural Earth polygons.
# EPSG:6933 is an equal-area projection, so polygon areas can
# be calculated in square metres and converted to km².
# ============================================================

country_land <- world %>%
  
  # Make geometries valid before calculating areas
  st_make_valid() %>%
  
  # Equal-area projection
  st_transform(6933) %>%
  
  # Calculate country land area in km²
  mutate(
    Ctry_Land_km2 =
      as.numeric(st_area(geometry)) / 1e6
  ) %>%
  
  # Return ordinary dataframe
  st_drop_geometry() %>%
  
  transmute(
    ISO3 = iso_a3,
    Ctry_Land_km2
  ) %>%
  
  filter(
    !is.na(ISO3),
    ISO3 != "",
    ISO3 != "-99",
    !is.na(Ctry_Land_km2),
    Ctry_Land_km2 > 0
  ) %>%
  
  distinct(
    ISO3,
    .keep_all = TRUE
  )


# Quick check
country_land %>%
  arrange(desc(Ctry_Land_km2)) %>%
  head(10)

# ============================================================
# 10. ALL YEARS:
# SUMMED TERRESTRIAL GIS PA AREA RELATIVE TO COUNTRY LAND
# ============================================================

country_pa_pct <- country_pa_all %>%
  
  left_join(
    country_land,
    by = "ISO3"
  ) %>%
  
  mutate(
    pct_country =
      100 *
      land_area_km2 /
      Ctry_Land_km2
  )


# ============================================================
# 11. DIAGNOSTIC: HIGHEST COUNTRY PERCENTAGES
# ============================================================

country_pa_pct %>%
  
  filter(
    !is.na(pct_country)
  ) %>%
  
  arrange(
    desc(pct_country)
  ) %>%
  
  select(
    ISO3,
    Characteristic,
    Category,
    land_area_km2,
    Ctry_Land_km2,
    pct_country
  ) %>%
  
  head(30)


# ============================================================
# 12. FUNCTION:
# MAP SUMMED TERRESTRIAL GIS AREA AS % OF COUNTRY LAND
#
# Colour scale capped at 95th percentile.
# ============================================================

make_pct_country_map <- function(
    pa_data,
    characteristic_name,
    plot_title,
    ncol = 3,
    exclude_not_reported = TRUE,
    cap_quantile = 0.95,
    colours = gov_cols
) {
  
  map_data <- pa_data %>%
    filter(
      Characteristic == characteristic_name
    )
  
  if (exclude_not_reported) {
    map_data <- map_data %>%
      filter(
        Category != "Not Reported"
      )
  }
  
  category_order <- map_data %>%
    filter(
      !is.na(pct_country),
      pct_country > 0
    ) %>%
    group_by(Category) %>%
    summarise(
      n_entities = n_distinct(ISO3),
      .groups = "drop"
    ) %>%
    arrange(
      desc(n_entities)
    ) %>%
    pull(Category)
  
  map_data <- map_data %>%
    mutate(
      Category = factor(
        Category,
        levels = category_order
      )
    )
  
  positive_values <- map_data$pct_country[
    is.finite(map_data$pct_country) &
      map_data$pct_country > 0
  ]
  
  cap_value <- as.numeric(
    quantile(
      positive_values,
      probs = cap_quantile,
      na.rm = TRUE
    )
  )
  
  map_data <- map_data %>%
    mutate(
      pct_plot = case_when(
        
        is.na(pct_country) |
          pct_country <= 0 ~
          NA_real_,
        
        pct_country > cap_value ~
          cap_value,
        
        TRUE ~ pct_country
      )
    )
  
  map_full <- expand_grid(
    
    Category = factor(
      category_order,
      levels = category_order
    ),
    
    iso_a3 = world$iso_a3
    
  ) %>%
    left_join(
      map_data %>%
        select(
          Category,
          ISO3,
          pct_country,
          pct_plot
        ) %>%
        rename(
          iso_a3 = ISO3
        ),
      
      by = c(
        "Category",
        "iso_a3"
      )
    )
  
  map_sf <- world %>%
    left_join(
      map_full,
      by = "iso_a3"
    ) %>%
    st_as_sf()
  
  ggplot(map_sf) +
    
    geom_sf(
      aes(
        fill = pct_plot
      ),
      color = "white",
      linewidth = 0.05
    ) +
    
    facet_wrap(
      ~Category,
      ncol = ncol
    ) +
    
    scale_fill_gradientn(
      
      colours = colours,
      
      na.value = "grey85",
      
      limits = c(
        0,
        cap_value
      ),
      
      breaks = scales::breaks_pretty(
        n = 4
      ),
      
      labels = function(x) {
        paste0(
          scales::number(
            x,
            accuracy = 0.1
          ),
          "%"
        )
      },
      
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        barwidth = grid::unit(
          10,
          "cm"
        ),
        barheight = grid::unit(
          0.55,
          "cm"
        )
      )
    ) +
    
    coord_sf(
      expand = FALSE
    ) +
    
    labs(
      
      title = plot_title,
      
      subtitle = paste0(
        "Summed GIS-derived terrestrial PA area relative to country land area; ",
        "colour scale capped at the ",
        cap_quantile * 100,
        "th percentile"
      ),
      
      fill = "% of Country",
      
      caption = paste0(
        "Gray indicates no positive mapped value. ",
        "Values above ",
        round(cap_value, 1),
        "% use the darkest shade. ",
        "PA overlaps are not dissolved, so values represent summed area rather than unique land coverage."
      )
    ) +
    
    theme_bw() +
    
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      
      strip.text = element_text(
        face = "bold",
        size = 10
      ),
      
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      
      plot.subtitle = element_text(
        size = 11
      ),
      
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9)
    )
}


# ============================================================
# 13. ALL-YEARS % COUNTRY LAND MAPS
# ============================================================

p_gov_pct <- make_pct_country_map(
  country_pa_pct,
  "Governance Type",
  "Protected Area Governance Types Relative to Country Land Area",
  ncol = 3,
  colours = gov_cols
)

p_gov_pct


p_iucn_pct <- make_pct_country_map(
  country_pa_pct,
  "IUCN Category",
  "IUCN Protected Area Categories Relative to Country Land Area",
  ncol = 3,
  colours = iucn_cols
)

p_iucn_pct


p_ownership_pct <- make_pct_country_map(
  country_pa_pct,
  "Ownership",
  "Protected Area Ownership Types Relative to Country Land Area",
  ncol = 3,
  colours = own_cols
)

p_ownership_pct


p_designation_pct <- make_pct_country_map(
  country_pa_pct,
  "Designation Type",
  "Protected Area Designation Types Relative to Country Land Area",
  ncol = 2,
  colours = desig_cols
)

p_designation_pct


# ============================================================
# 14. CHECK FOR VALUES > 100%
#
# These may still occur if multiple mapped PAs overlap.
# Summing GIS_AREA does NOT dissolve spatial overlap.
# ============================================================

country_pa_pct %>%
  
  filter(
    !is.na(pct_country),
    pct_country > 100
  ) %>%
  
  arrange(
    desc(pct_country)
  ) %>%
  
  select(
    ISO3,
    Characteristic,
    Category,
    land_area_km2,
    Ctry_Land_km2,
    pct_country
  )


##### Saving df ######

output_dir <- "/Users/alexrogers/Desktop/Global Anaysis /sample_design/outputs"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

saveRDS(
  df,
  file.path(output_dir, "df.rds")
)














df <- readRDS(
  "/Users/alexrogers/Desktop/Global Anaysis /sample_design/outputs/df.rds"
)


library(dplyr)
library(ggplot2)
library(scales)

#--------------------------------------------------
# PA-level terrestrial area diagnostic
#--------------------------------------------------

pa_size <- df %>%
  filter(
    REALM == "Terrestrial",
    STATUS_YR >= 2000,
    STATUS_YR <= 2024
  ) %>%
  mutate(
    # Prefer GIS area where available
    terrestrial_area_km2 = case_when(
      !is.na(GIS_AREA) & !is.na(GIS_M_AREA) ~
        pmax(GIS_AREA - GIS_M_AREA, 0),
      
      !is.na(REP_AREA) & !is.na(REP_M_AREA) ~
        pmax(REP_AREA - REP_M_AREA, 0),
      
      TRUE ~ NA_real_
    ),
    
    # VERY rough proxy only:
    approx_1km_cells = terrestrial_area_km2
  ) %>%
  filter(
    !is.na(terrestrial_area_km2),
    terrestrial_area_km2 > 0
  )


ggplot(pa_size, aes(x = terrestrial_area_km2)) +
  geom_histogram(bins = 80) +
  scale_x_log10(
    labels = label_number(big.mark = ",")
  ) +
  labs(
    title = "Global distribution of terrestrial protected-area size",
    subtitle = "Protected areas established 2000–2024",
    x = "Terrestrial PA area (km², log scale)",
    y = "Number of protected areas"
  ) +
  theme_minimal()



pa_concentration_small_to_large <- pa_size %>%
  arrange(terrestrial_area_km2) %>%
  mutate(
    pct_parks = row_number() / n(),
    pct_area = cumsum(terrestrial_area_km2) /
      sum(terrestrial_area_km2)
  )

ggplot(pa_concentration_small_to_large,
       aes(x = pct_parks, y = pct_area)) +
  geom_line() +
  geom_abline(linetype = "dashed") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "PA count versus protected area",
    x = "Cumulative share of protected areas",
    y = "Cumulative share of terrestrial protected area"
  ) +
  theme_minimal()


pa_size %>%
  mutate(
    size_class = case_when(
      terrestrial_area_km2 < 1       ~ "<1 km²",
      terrestrial_area_km2 < 10      ~ "1–10 km²",
      terrestrial_area_km2 < 100     ~ "10–100 km²",
      terrestrial_area_km2 < 1000    ~ "100–1,000 km²",
      terrestrial_area_km2 < 10000   ~ "1,000–10,000 km²",
      TRUE                            ~ "≥10,000 km²"
    ),
    size_class = factor(
      size_class,
      levels = c(
        "<1 km²",
        "1–10 km²",
        "10–100 km²",
        "100–1,000 km²",
        "1,000–10,000 km²",
        "≥10,000 km²"
      )
    )
  ) %>%
  group_by(size_class) %>%
  summarise(
    n_parks = n(),
    pct_parks = n() / nrow(pa_size),
    area_km2 = sum(terrestrial_area_km2),
    pct_area = area_km2 / sum(pa_size$terrestrial_area_km2),
    .groups = "drop"
  )


governance_size <- pa_size %>%
  group_by(GOV_TYPE) %>%
  summarise(
    n_parks = n(),
    total_area_km2 = sum(terrestrial_area_km2),
    median_area_km2 = median(terrestrial_area_km2),
    mean_area_km2 = mean(terrestrial_area_km2),
    p25_area_km2 = quantile(terrestrial_area_km2, 0.25),
    p75_area_km2 = quantile(terrestrial_area_km2, 0.75),
    p90_area_km2 = quantile(terrestrial_area_km2, 0.90),
    n_countries = n_distinct(ISO3),
    .groups = "drop"
  ) %>%
  arrange(desc(total_area_km2))

governance_size

ggplot(
  pa_size %>% filter(GOV_TYPE != "Not Reported"),
  aes(x = GOV_TYPE, y = terrestrial_area_km2)
) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  coord_flip() +
  labs(
    title = "Protected-area size by governance type",
    x = NULL,
    y = "Terrestrial PA area (km², log scale)"
  ) +
  theme_minimal()


country_size <- pa_size %>%
  group_by(ISO3) %>%
  summarise(
    n_parks = n(),
    total_area_km2 = sum(terrestrial_area_km2),
    median_pa_area_km2 = median(terrestrial_area_km2),
    p90_pa_area_km2 = quantile(terrestrial_area_km2, 0.90),
    max_pa_area_km2 = max(terrestrial_area_km2),
    n_governance_types = n_distinct(GOV_TYPE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_parks))

country_size


######### 2009 to 2020 cohort #########

# ============================================================
# GLOBAL PA SAMPLING DIAGNOSTICS
# Primary treatment cohorts: 2009–2020
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

#--------------------------------------------------------------
# 1. Construct primary analytic PA population
#--------------------------------------------------------------

pa_size <- df %>%
  
  # Primary analytic treatment cohorts
  filter(
    REALM == "Terrestrial",
    STATUS_YR >= 2009,
    STATUS_YR <= 2020
  ) %>%
  
  mutate(
    
    # Estimate terrestrial area.
    # Prefer GIS area when available; otherwise use reported area.
    terrestrial_area_km2 = case_when(
      
      !is.na(GIS_AREA) & !is.na(GIS_M_AREA) ~
        pmax(GIS_AREA - GIS_M_AREA, 0),
      
      !is.na(REP_AREA) & !is.na(REP_M_AREA) ~
        pmax(REP_AREA - REP_M_AREA, 0),
      
      TRUE ~ NA_real_
    ),
    
    # Very rough first-pass approximation:
    # one km² corresponds approximately to one potential 1-km cell.
    # Replace later with the actual number of ≥50%-protected cells.
    approx_1km_cells = terrestrial_area_km2
  ) %>%
  
  filter(
    !is.na(terrestrial_area_km2),
    terrestrial_area_km2 > 0
  )


# Basic sample check
pa_size %>%
  summarise(
    n_parks = n(),
    n_countries = n_distinct(ISO3),
    total_area_km2 = sum(terrestrial_area_km2),
    earliest_year = min(STATUS_YR),
    latest_year = max(STATUS_YR)
  )



#--------------------------------------------------------------
# 2. Global PA-size distribution
#--------------------------------------------------------------

pa_size_summary <- pa_size %>%
  summarise(
    n_parks = n(),
    n_countries = n_distinct(ISO3),
    
    min_km2 = min(terrestrial_area_km2),
    p01_km2 = quantile(terrestrial_area_km2, 0.01),
    p05_km2 = quantile(terrestrial_area_km2, 0.05),
    p10_km2 = quantile(terrestrial_area_km2, 0.10),
    p25_km2 = quantile(terrestrial_area_km2, 0.25),
    median_km2 = median(terrestrial_area_km2),
    mean_km2 = mean(terrestrial_area_km2),
    p75_km2 = quantile(terrestrial_area_km2, 0.75),
    p90_km2 = quantile(terrestrial_area_km2, 0.90),
    p95_km2 = quantile(terrestrial_area_km2, 0.95),
    p99_km2 = quantile(terrestrial_area_km2, 0.99),
    max_km2 = max(terrestrial_area_km2)
  )

pa_size_summary

ggplot(pa_size, aes(x = terrestrial_area_km2)) +
  geom_histogram(bins = 80) +
  scale_x_log10(
    labels = label_number(big.mark = ",")
  ) +
  labs(
    title = "Global distribution of protected-area size",
    subtitle = "Terrestrial PAs established 2009–2020",
    x = "Terrestrial PA area (km², log scale)",
    y = "Number of protected areas"
  ) +
  theme_minimal()


#--------------------------------------------------------------
# 3. Concentration of protected area across PAs
#--------------------------------------------------------------

pa_concentration <- pa_size %>%
  arrange(terrestrial_area_km2) %>%
  mutate(
    cumulative_n = row_number(),
    pct_parks = cumulative_n / n(),
    
    cumulative_area =
      cumsum(terrestrial_area_km2),
    
    pct_area =
      cumulative_area /
      sum(terrestrial_area_km2)
  )


ggplot(
  pa_concentration,
  aes(x = pct_parks, y = pct_area)
) +
  geom_line(linewidth = 1) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    labels = percent
  ) +
  scale_y_continuous(
    labels = percent
  ) +
  labs(
    title = "Concentration of protected area across PAs",
    subtitle = "Terrestrial PAs established 2009–2020",
    x = "Cumulative share of protected areas",
    y = "Cumulative share of terrestrial protected area"
  ) +
  theme_minimal()



#--------------------------------------------------------------
# 4. Share of total protected area in largest PAs
#--------------------------------------------------------------

large_pa_concentration <- pa_size %>%
  arrange(desc(terrestrial_area_km2)) %>%
  mutate(
    rank = row_number(),
    rank_share = rank / n()
  ) %>%
  summarise(
    
    top_1pct_area_share =
      sum(
        terrestrial_area_km2[
          rank_share <= 0.01
        ]
      ) /
      sum(terrestrial_area_km2),
    
    top_5pct_area_share =
      sum(
        terrestrial_area_km2[
          rank_share <= 0.05
        ]
      ) /
      sum(terrestrial_area_km2),
    
    top_10pct_area_share =
      sum(
        terrestrial_area_km2[
          rank_share <= 0.10
        ]
      ) /
      sum(terrestrial_area_km2)
  )

large_pa_concentration


#--------------------------------------------------------------
# 5. PA size classes
#--------------------------------------------------------------

pa_size_classes <- pa_size %>%
  mutate(
    
    size_class = case_when(
      terrestrial_area_km2 < 1       ~ "<1 km²",
      terrestrial_area_km2 < 10      ~ "1–10 km²",
      terrestrial_area_km2 < 100     ~ "10–100 km²",
      terrestrial_area_km2 < 1000    ~ "100–1,000 km²",
      terrestrial_area_km2 < 10000   ~ "1,000–10,000 km²",
      TRUE                            ~ "≥10,000 km²"
    ),
    
    size_class = factor(
      size_class,
      levels = c(
        "<1 km²",
        "1–10 km²",
        "10–100 km²",
        "100–1,000 km²",
        "1,000–10,000 km²",
        "≥10,000 km²"
      )
    )
  )


size_class_summary <- pa_size_classes %>%
  group_by(size_class) %>%
  summarise(
    n_parks = n(),
    area_km2 = sum(terrestrial_area_km2),
    .groups = "drop"
  ) %>%
  mutate(
    pct_parks =
      n_parks / sum(n_parks),
    
    pct_area =
      area_km2 / sum(area_km2)
  )

size_class_summary





#--------------------------------------------------------------
# 6. Function: summarize a PA characteristic
#--------------------------------------------------------------

summarize_pa_characteristic <- function(data, characteristic) {
  
  data %>%
    group_by({{ characteristic }}) %>%
    summarise(
      
      n_parks = n(),
      
      total_area_km2 =
        sum(
          terrestrial_area_km2,
          na.rm = TRUE
        ),
      
      median_area_km2 =
        median(
          terrestrial_area_km2,
          na.rm = TRUE
        ),
      
      mean_area_km2 =
        mean(
          terrestrial_area_km2,
          na.rm = TRUE
        ),
      
      p25_area_km2 =
        quantile(
          terrestrial_area_km2,
          0.25,
          na.rm = TRUE
        ),
      
      p75_area_km2 =
        quantile(
          terrestrial_area_km2,
          0.75,
          na.rm = TRUE
        ),
      
      p90_area_km2 =
        quantile(
          terrestrial_area_km2,
          0.90,
          na.rm = TRUE
        ),
      
      p99_area_km2 =
        quantile(
          terrestrial_area_km2,
          0.99,
          na.rm = TRUE
        ),
      
      max_area_km2 =
        max(
          terrestrial_area_km2,
          na.rm = TRUE
        ),
      
      n_countries =
        n_distinct(ISO3),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      pct_all_parks =
        n_parks / sum(n_parks),
      
      pct_total_area =
        total_area_km2 /
        sum(total_area_km2)
    ) %>%
    
    arrange(desc(total_area_km2))
}


#--------------------------------------------------------------
# 7. Governance diagnostics
#--------------------------------------------------------------

governance_size <- summarize_pa_characteristic(
  pa_size,
  GOV_TYPE
)

governance_size

#--------------------------------------------------------------
# 8. IUCN category diagnostics
#--------------------------------------------------------------

iucn_size <- summarize_pa_characteristic(
  pa_size,
  IUCN_CAT
)

iucn_size



#--------------------------------------------------------------
# 9. Function: plot PA size by characteristic
#--------------------------------------------------------------

plot_pa_size_by_characteristic <- function(
    data,
    characteristic,
    title
) {
  
  ggplot(
    data,
    aes(
      x = {{ characteristic }},
      y = terrestrial_area_km2
    )
  ) +
    geom_boxplot(
      outlier.shape = NA
    ) +
    scale_y_log10(
      labels =
        label_number(big.mark = ",")
    ) +
    coord_flip() +
    labs(
      title = title,
      subtitle = "Terrestrial PAs established 2009–2020",
      x = NULL,
      y = "Terrestrial PA area (km², log scale)"
    ) +
    theme_minimal()
}


plot_pa_size_by_characteristic(
  pa_size,
  GOV_TYPE,
  "Protected-area size by governance type"
)

plot_pa_size_by_characteristic(
  pa_size,
  IUCN_CAT,
  "Protected-area size by IUCN category"
)



#--------------------------------------------------------------
# 10. Size composition by governance type
#--------------------------------------------------------------

governance_size_classes <- pa_size_classes %>%
  group_by(
    GOV_TYPE,
    size_class
  ) %>%
  summarise(
    n_parks = n(),
    area_km2 =
      sum(
        terrestrial_area_km2,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) %>%
  group_by(GOV_TYPE) %>%
  mutate(
    pct_parks_within_type =
      n_parks /
      sum(n_parks),
    
    pct_area_within_type =
      area_km2 /
      sum(area_km2)
  ) %>%
  ungroup()

governance_size_classes


#--------------------------------------------------------------
# 11. Size composition by IUCN category
#--------------------------------------------------------------

iucn_size_classes <- pa_size_classes %>%
  group_by(
    IUCN_CAT,
    size_class
  ) %>%
  summarise(
    n_parks = n(),
    area_km2 =
      sum(
        terrestrial_area_km2,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) %>%
  group_by(IUCN_CAT) %>%
  mutate(
    pct_parks_within_category =
      n_parks /
      sum(n_parks),
    
    pct_area_within_category =
      area_km2 /
      sum(area_km2)
  ) %>%
  ungroup()

iucn_size_classes


#--------------------------------------------------------------
# 12. Governance × IUCN support
#--------------------------------------------------------------

gov_iucn_cross <- pa_size %>%
  group_by(
    GOV_TYPE,
    IUCN_CAT
  ) %>%
  summarise(
    
    n_parks = n(),
    
    area_km2 =
      sum(
        terrestrial_area_km2,
        na.rm = TRUE
      ),
    
    n_countries =
      n_distinct(ISO3),
    
    median_area_km2 =
      median(
        terrestrial_area_km2,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  arrange(desc(area_km2))

gov_iucn_cross


gov_iucn_n_matrix <- gov_iucn_cross %>%
  select(
    GOV_TYPE,
    IUCN_CAT,
    n_parks
  ) %>%
  pivot_wider(
    names_from = IUCN_CAT,
    values_from = n_parks,
    values_fill = 0
  )

gov_iucn_n_matrix


gov_iucn_area_matrix <- gov_iucn_cross %>%
  select(
    GOV_TYPE,
    IUCN_CAT,
    area_km2
  ) %>%
  pivot_wider(
    names_from = IUCN_CAT,
    values_from = area_km2,
    values_fill = 0
  )

gov_iucn_area_matrix



#--------------------------------------------------------------
# 13. Country-level PA sampling diagnostics
#--------------------------------------------------------------

country_size <- pa_size %>%
  group_by(ISO3) %>%
  summarise(
    
    n_parks = n(),
    
    total_area_km2 =
      sum(
        terrestrial_area_km2,
        na.rm = TRUE
      ),
    
    median_pa_area_km2 =
      median(
        terrestrial_area_km2,
        na.rm = TRUE
      ),
    
    p90_pa_area_km2 =
      quantile(
        terrestrial_area_km2,
        0.90,
        na.rm = TRUE
      ),
    
    max_pa_area_km2 =
      max(
        terrestrial_area_km2,
        na.rm = TRUE
      ),
    
    n_governance_types =
      n_distinct(GOV_TYPE),
    
    n_iucn_categories =
      n_distinct(IUCN_CAT),
    
    .groups = "drop"
  )

# Countries with most PAs
country_size %>%
  arrange(desc(n_parks))

# Countries with most PA area
country_size %>%
  arrange(desc(total_area_km2))


#--------------------------------------------------------------
# 14. Treatment-cohort diagnostics
#--------------------------------------------------------------

cohort_summary <- pa_size %>%
  group_by(STATUS_YR) %>%
  summarise(
    
    n_parks = n(),
    
    n_countries =
      n_distinct(ISO3),
    
    area_km2 =
      sum(
        terrestrial_area_km2,
        na.rm = TRUE
      ),
    
    median_pa_area_km2 =
      median(
        terrestrial_area_km2,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

cohort_summary


ggplot(
  cohort_summary,
  aes(
    x = STATUS_YR,
    y = n_parks
  )
) +
  geom_col() +
  scale_x_continuous(
    breaks = 2009:2020
  ) +
  labs(
    title = "Protected areas by treatment cohort",
    subtitle = "Primary analytic window: 2009–2020",
    x = "Year established",
    y = "Number of protected areas"
  ) +
  theme_minimal()


ggplot(
  cohort_summary,
  aes(
    x = STATUS_YR,
    y = area_km2
  )
) +
  geom_col() +
  scale_x_continuous(
    breaks = 2009:2020
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ","
    )
  ) +
  labs(
    title = "Protected area added by treatment cohort",
    subtitle = "Primary analytic window: 2009–2020",
    x = "Year established",
    y = "Terrestrial area (km²)"
  ) +
  theme_minimal()






######### SAMPLING STRATEGY SENSITIVITY ANALYSIS #########

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# ============================================================
# SETTINGS
# ============================================================

THIN_DISTANCES <- c(5, 10)

PA_CAPS <- c(
  25, 50, 100, 200, 500, 1000
)


# ============================================================
# 1. PREPARE PA DATA
# ============================================================

prepare_pa_simulation <- function(data) {
  
  data %>%
    mutate(
      
      # Rough proxy only:
      # 1 km² PA area ≈ 1 possible 1-km treated cell.
      #
      # pmax(1) means every PA gets at least one potential cell
      # in this diagnostic. The real >=50%-protected raster will
      # later determine actual eligibility.
      approx_cells = pmax(
        1L,
        as.integer(round(terrestrial_area_km2))
      ),
      
      size_class = case_when(
        terrestrial_area_km2 < 1     ~ "<1 km²",
        terrestrial_area_km2 < 10    ~ "1–10 km²",
        terrestrial_area_km2 < 100   ~ "10–100 km²",
        terrestrial_area_km2 < 1000  ~ "100–1,000 km²",
        terrestrial_area_km2 < 10000 ~ "1,000–10,000 km²",
        TRUE                          ~ "≥10,000 km²"
      ),
      
      size_class = factor(
        size_class,
        levels = c(
          "<1 km²",
          "1–10 km²",
          "10–100 km²",
          "100–1,000 km²",
          "1,000–10,000 km²",
          "≥10,000 km²"
        )
      )
    )
}


sim_pa <- prepare_pa_simulation(pa_size)


# ============================================================
# 2. SAMPLING RULES
# ============================================================

# Approximate number of points that can be separated by d km
# inside a compact PA.
#
# 0.866*d^2 is the approximate area per point under
# hexagonal/triangular packing.
approx_thinned_cells <- function(area_km2, approx_cells, distance_km) {
  
  area_per_point <- 0.866 * distance_km^2
  
  thinning_capacity <- pmax(
    1L,
    as.integer(floor(area_km2 / area_per_point))
  )
  
  pmin(
    approx_cells,
    thinning_capacity
  )
}


# Build all sampling scenarios in one long dataframe
build_sampling_scenarios <- function(
    data,
    thinning_distances = c(5, 10),
    pa_caps = c(25, 50, 100, 200, 500, 1000)
) {
  
  # No thinning
  baseline <- data %>%
    mutate(
      scenario_type = "No thinning",
      parameter = NA_real_,
      scenario = "No thinning",
      sample_cells = approx_cells
    )
  
  
  # Spatial thinning scenarios
  thinning <- map_dfr(
    thinning_distances,
    function(d) {
      
      data %>%
        mutate(
          scenario_type = "Spatial thinning",
          parameter = d,
          scenario = paste0(d, "-km thinning"),
          
          sample_cells = approx_thinned_cells(
            area_km2 = terrestrial_area_km2,
            approx_cells = approx_cells,
            distance_km = d
          )
        )
    }
  )
  
  
  # Asymmetric treated-cell strategies:
  # no within-PA thinning, but impose PA-level cap
  capped <- map_dfr(
    pa_caps,
    function(cap) {
      
      data %>%
        mutate(
          scenario_type = "PA cap",
          parameter = cap,
          scenario = paste0("PA cap: ", cap),
          sample_cells = pmin(
            approx_cells,
            cap
          )
        )
    }
  )
  
  
  scenario_order <- c(
    "No thinning",
    paste0(thinning_distances, "-km thinning"),
    paste0("PA cap: ", pa_caps)
  )
  
  
  bind_rows(
    baseline,
    thinning,
    capped
  ) %>%
    mutate(
      scenario = factor(
        scenario,
        levels = scenario_order
      )
    )
}


sim_long <- build_sampling_scenarios(
  sim_pa,
  thinning_distances = THIN_DISTANCES,
  pa_caps = PA_CAPS
)

# ============================================================
# 3. OVERALL SCENARIO SUMMARY
# ============================================================

top_share <- function(x, proportion) {
  
  x <- sort(
    x,
    decreasing = TRUE
  )
  
  n_top <- max(
    1,
    ceiling(length(x) * proportion)
  )
  
  sum(head(x, n_top)) / sum(x)
}


summarise_scenarios <- function(data) {
  
  baseline_cells <- data %>%
    filter(scenario == "No thinning") %>%
    summarise(
      n = sum(sample_cells)
    ) %>%
    pull(n)
  
  
  data %>%
    group_by(
      scenario_type,
      parameter,
      scenario
    ) %>%
    summarise(
      
      sampled_cells =
        sum(sample_cells),
      
      median_cells_per_pa =
        median(sample_cells),
      
      p90_cells_per_pa =
        quantile(sample_cells, 0.90),
      
      max_cells_from_one_pa =
        max(sample_cells),
      
      pct_pas_one_cell =
        mean(sample_cells <= 1),
      
      top_1pct_pa_share =
        top_share(sample_cells, 0.01),
      
      top_5pct_pa_share =
        top_share(sample_cells, 0.05),
      
      top_10pct_pa_share =
        top_share(sample_cells, 0.10),
      
      .groups = "drop"
    ) %>%
    mutate(
      retention =
        sampled_cells / baseline_cells
    )
}


overall_summary <- summarise_scenarios(sim_long)

overall_display <- overall_summary %>%
  transmute(
    Scenario = scenario,
    
    `Sampled cells` =
      comma(sampled_cells),
    
    `Retention` =
      percent(
        retention,
        accuracy = 0.1
      ),
    
    `Median cells / PA` =
      comma(
        median_cells_per_pa,
        accuracy = 1
      ),
    
    `90th percentile` =
      comma(
        p90_cells_per_pa,
        accuracy = 1
      ),
    
    `Maximum from one PA` =
      comma(max_cells_from_one_pa),
    
    `% PAs with one cell` =
      percent(
        pct_pas_one_cell,
        accuracy = 0.1
      ),
    
    `Top 1% PA share` =
      percent(
        top_1pct_pa_share,
        accuracy = 0.1
      ),
    
    `Top 5% PA share` =
      percent(
        top_5pct_pa_share,
        accuracy = 0.1
      )
  )

overall_display


# ============================================================
# 4. GENERIC SUPPORT SUMMARY
# ============================================================

summarise_support <- function(data, group_var) {
  
  data %>%
    group_by(
      scenario_type,
      parameter,
      scenario,
      {{ group_var }}
    ) %>%
    summarise(
      
      n_parks = n(),
      
      n_countries =
        n_distinct(
          ISO3,
          na.rm = TRUE
        ),
      
      available_cells =
        sum(approx_cells),
      
      sampled_cells =
        sum(sample_cells),
      
      median_cells_per_pa =
        median(sample_cells),
      
      pct_pas_one_cell =
        mean(sample_cells <= 1),
      
      .groups = "drop"
    ) %>%
    
    group_by(scenario) %>%
    
    mutate(
      
      retention =
        sampled_cells /
        available_cells,
      
      pct_of_scenario_sample =
        sampled_cells /
        sum(sampled_cells)
    ) %>%
    
    ungroup()
}

size_support <- summarise_support(
  sim_long,
  size_class
)

governance_support <- summarise_support(
  sim_long,
  GOV_TYPE
)

iucn_support <- summarise_support(
  sim_long,
  IUCN_CAT
)

SELECTED_SCENARIOS <- c(
  "No thinning",
  "5-km thinning",
  "PA cap: 100",
  "PA cap: 200",
  "PA cap: 500"
)

governance_display <- governance_support %>%
  filter(
    scenario %in% SELECTED_SCENARIOS
  ) %>%
  transmute(
    Scenario = scenario,
    Governance = GOV_TYPE,
    Parks = comma(n_parks),
    Countries = n_countries,
    `Sampled cells` = comma(sampled_cells),
    `Retention` = percent(
      retention,
      accuracy = 0.1
    ),
    `Share of sample` = percent(
      pct_of_scenario_sample,
      accuracy = 0.1
    ),
    `% PAs with one cell` = percent(
      pct_pas_one_cell,
      accuracy = 0.1
    )
  )

governance_display

iucn_display <- iucn_support %>%
  filter(
    scenario %in% SELECTED_SCENARIOS
  ) %>%
  transmute(
    Scenario = scenario,
    `IUCN category` = IUCN_CAT,
    Parks = comma(n_parks),
    Countries = n_countries,
    `Sampled cells` = comma(sampled_cells),
    `Retention` = percent(
      retention,
      accuracy = 0.1
    ),
    `Share of sample` = percent(
      pct_of_scenario_sample,
      accuracy = 0.1
    ),
    `% PAs with one cell` = percent(
      pct_pas_one_cell,
      accuracy = 0.1
    )
  )

iucn_display

size_display <- size_support %>%
  filter(
    scenario %in% SELECTED_SCENARIOS
  ) %>%
  transmute(
    Scenario = scenario,
    `PA size` = size_class,
    Parks = comma(n_parks),
    `Sampled cells` = comma(sampled_cells),
    `Median cells / PA` = comma(
      median_cells_per_pa,
      accuracy = 1
    ),
    `Retention` = percent(
      retention,
      accuracy = 0.1
    ),
    `% PAs with one cell` = percent(
      pct_pas_one_cell,
      accuracy = 0.1
    )
  )

size_display

# ============================================================
# 5. CAP SENSITIVITY
# ============================================================

cap_sensitivity <- overall_summary %>%
  filter(
    scenario_type == "PA cap"
  ) %>%
  arrange(parameter) %>%
  transmute(
    
    `PA cap` =
      parameter,
    
    `Sampled cells` =
      sampled_cells,
    
    `Retention` =
      retention,
    
    `Median cells / PA` =
      median_cells_per_pa,
    
    `90th percentile` =
      p90_cells_per_pa,
    
    `Top 1% PA share` =
      top_1pct_pa_share,
    
    `Top 5% PA share` =
      top_5pct_pa_share
  )

cap_sensitivity

cap_sensitivity_display <- cap_sensitivity %>%
  transmute(
    `PA cap` = `PA cap`,
    
    `Sampled cells` =
      comma(`Sampled cells`),
    
    `Retention` =
      percent(
        Retention,
        accuracy = 0.1
      ),
    
    `Median cells / PA` =
      comma(
        `Median cells / PA`,
        accuracy = 1
      ),
    
    `90th percentile` =
      comma(
        `90th percentile`,
        accuracy = 1
      ),
    
    `Top 1% PA share` =
      percent(
        `Top 1% PA share`,
        accuracy = 0.1
      ),
    
    `Top 5% PA share` =
      percent(
        `Top 5% PA share`,
        accuracy = 0.1
      )
  )

cap_sensitivity_display

ggplot(
  cap_sensitivity,
  aes(
    x = `PA cap`,
    y = `Sampled cells`
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_log10(
    breaks = PA_CAPS
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Treated-cell sample size under alternative PA caps",
    subtitle = "Non-spatial approximation using terrestrial PA area",
    x = "Maximum sampled cells per PA",
    y = "Approximate treated cells retained"
  ) +
  theme_minimal()

ggplot(
  cap_sensitivity,
  aes(
    x = `PA cap`,
    y = `Top 1% PA share`
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_log10(
    breaks = PA_CAPS
  ) +
  scale_y_continuous(
    labels = percent
  ) +
  labs(
    title = "Sample concentration under alternative PA caps",
    subtitle = "Share of treated sample contributed by largest 1% of PAs",
    x = "Maximum sampled cells per PA",
    y = "Share of treated sample"
  ) +
  theme_minimal()



saveRDS(
  sim_long,
  "/Users/alexrogers/Desktop/Global Anaysis /sample_design/outputs//sim_long.rds"
)










######################### 3 ################################







######### 100 VS 200 PA-CAP SUPPORT DIAGNOSTICS #########

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(purrr)

# ============================================================
# SETTINGS
# ============================================================

COMPARE_CAPS <- c(100, 200)

COMPARE_SCENARIOS <- paste0(
  "PA cap: ",
  COMPARE_CAPS
)

# Diagnostic thresholds only.
# These are not inferential requirements.
CELL_THRESHOLDS <- c(50, 100, 200, 500)


cap_compare <- sim_long %>%
  filter(
    scenario %in% COMPARE_SCENARIOS
  ) %>%
  mutate(
    cap = parameter
  )

overall_cap_compare <- cap_compare %>%
  group_by(cap) %>%
  summarise(
    sampled_cells = sum(sample_cells),
    n_parks = sum(sample_cells > 0),
    n_countries = n_distinct(ISO3[sample_cells > 0]),
    median_cells_per_pa = median(sample_cells),
    p90_cells_per_pa = quantile(sample_cells, 0.90),
    max_cells_per_pa = max(sample_cells),
    .groups = "drop"
  ) %>%
  arrange(cap) %>%
  mutate(
    extra_cells_vs_100 =
      sampled_cells - first(sampled_cells),
    
    pct_more_cells_vs_100 =
      sampled_cells / first(sampled_cells) - 1
  )

overall_cap_compare

overall_cap_display <- overall_cap_compare %>%
  transmute(
    `PA cap` = cap,
    `Candidate treated cells` = comma(sampled_cells),
    `PAs represented` = comma(n_parks),
    `Countries represented` = n_countries,
    `Median cells / PA` = comma(median_cells_per_pa),
    `90th percentile` = comma(p90_cells_per_pa),
    `Maximum cells / PA` = comma(max_cells_per_pa),
    `Extra cells vs. cap 100` = comma(extra_cells_vs_100),
    `% increase vs. cap 100` =
      percent(pct_more_cells_vs_100, accuracy = 0.1)
  )

overall_cap_display


country_cohort_support <- cap_compare %>%
  group_by(
    cap,
    ISO3,
    STATUS_YR
  ) %>%
  summarise(
    sampled_cells = sum(sample_cells),
    n_parks = sum(sample_cells > 0),
    .groups = "drop"
  )


summarise_country_cohort <- function(
    data,
    thresholds = c(50, 100, 200, 500)
) {
  
  base <- data %>%
    group_by(cap) %>%
    summarise(
      n_country_cohort_strata = n(),
      
      median_cells =
        median(sampled_cells),
      
      p10_cells =
        quantile(sampled_cells, 0.10),
      
      p25_cells =
        quantile(sampled_cells, 0.25),
      
      p75_cells =
        quantile(sampled_cells, 0.75),
      
      p90_cells =
        quantile(sampled_cells, 0.90),
      
      median_parks =
        median(n_parks),
      
      .groups = "drop"
    )
  
  threshold_results <- map_dfr(
    thresholds,
    function(x) {
      
      data %>%
        group_by(cap) %>%
        summarise(
          threshold = x,
          
          n_strata_at_least =
            sum(sampled_cells >= x),
          
          pct_strata_at_least =
            mean(sampled_cells >= x),
          
          .groups = "drop"
        )
    }
  )
  
  list(
    distribution = base,
    thresholds = threshold_results
  )
}


country_cohort_summary <-
  summarise_country_cohort(
    country_cohort_support,
    CELL_THRESHOLDS
  )


country_cohort_summary$distribution

country_cohort_summary$thresholds %>%
  mutate(
    pct_strata_at_least =
      percent(
        pct_strata_at_least,
        accuracy = 0.1
      )
  )




country_cohort_gain <- country_cohort_support %>%
  select(
    cap,
    ISO3,
    STATUS_YR,
    sampled_cells
  ) %>%
  pivot_wider(
    names_from = cap,
    values_from = sampled_cells,
    names_prefix = "cap_"
  ) %>%
  mutate(
    additional_cells =
      cap_200 - cap_100,
    
    pct_gain =
      if_else(
        cap_100 > 0,
        cap_200 / cap_100 - 1,
        NA_real_
      )
  ) %>%
  arrange(
    desc(additional_cells)
  )

country_cohort_gain



gain_concentration <- country_cohort_gain %>%
  arrange(
    desc(additional_cells)
  ) %>%
  mutate(
    rank = row_number(),
    pct_strata = rank / n(),
    
    cumulative_gain_share =
      cumsum(additional_cells) /
      sum(additional_cells)
  ) %>%
  summarise(
    top_1pct_strata_gain =
      sum(
        additional_cells[
          pct_strata <= 0.01
        ]
      ) /
      sum(additional_cells),
    
    top_5pct_strata_gain =
      sum(
        additional_cells[
          pct_strata <= 0.05
        ]
      ) /
      sum(additional_cells),
    
    top_10pct_strata_gain =
      sum(
        additional_cells[
          pct_strata <= 0.10
        ]
      ) /
      sum(additional_cells)
  )

gain_concentration %>%
  mutate(
    across(
      everything(),
      ~ percent(.x, accuracy = 0.1)
    )
  )


###### TREATMENT COHORT SUPPORT ####

cohort_support <- cap_compare %>%
  group_by(
    cap,
    STATUS_YR
  ) %>%
  summarise(
    sampled_cells = sum(sample_cells),
    n_parks = sum(sample_cells > 0),
    n_countries = n_distinct(ISO3[sample_cells > 0]),
    .groups = "drop"
  )

ggplot(
  cohort_support,
  aes(
    x = STATUS_YR,
    y = sampled_cells,
    group = factor(cap),
    linetype = factor(cap)
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = comma
  ) +
  scale_x_continuous(
    breaks = 2009:2020
  ) +
  labs(
    title = "Treatment-cohort support under alternative PA caps",
    subtitle = "Candidate treated cells, 2009–2020",
    x = "PA establishment year",
    y = "Candidate treated cells",
    linetype = "PA cap"
  ) +
  theme_minimal()


##### Generic governance / IUCN support function

summarise_design_support <- function(
    data,
    group_var
) {
  
  data %>%
    group_by(
      cap,
      {{ group_var }}
    ) %>%
    summarise(
      sampled_cells =
        sum(sample_cells),
      
      n_parks =
        sum(sample_cells > 0),
      
      n_countries =
        n_distinct(
          ISO3[sample_cells > 0]
        ),
      
      median_cells_per_pa =
        median(sample_cells),
      
      p90_cells_per_pa =
        quantile(
          sample_cells,
          0.90
        ),
      
      max_cells_from_one_pa =
        max(sample_cells),
      
      .groups = "drop"
    ) %>%
    
    group_by(cap) %>%
    
    mutate(
      sample_share =
        sampled_cells /
        sum(sampled_cells)
    ) %>%
    
    ungroup()
}

governance_support <-
  summarise_design_support(
    cap_compare,
    GOV_TYPE
  )

iucn_support <-
  summarise_design_support(
    cap_compare,
    IUCN_CAT
  )


compare_category_caps <- function(
    support_data,
    group_var
) {
  
  support_data %>%
    select(
      cap,
      {{ group_var }},
      sampled_cells,
      n_parks,
      n_countries,
      sample_share
    ) %>%
    
    pivot_wider(
      names_from = cap,
      values_from = c(
        sampled_cells,
        n_parks,
        n_countries,
        sample_share
      ),
      names_sep = "_cap"
    ) %>%
    
    mutate(
      extra_cells =
        sampled_cells_cap200 -
        sampled_cells_cap100,
      
      pct_more_cells =
        sampled_cells_cap200 /
        sampled_cells_cap100 - 1,
      
      change_sample_share_pp =
        100 *
        (
          sample_share_cap200 -
            sample_share_cap100
        )
    ) %>%
    
    arrange(
      desc(extra_cells)
    )
}

governance_cap_compare <-
  compare_category_caps(
    governance_support,
    GOV_TYPE
  )

iucn_cap_compare <-
  compare_category_caps(
    iucn_support,
    IUCN_CAT
  )

governance_display <- governance_cap_compare %>%
  transmute(
    Governance = GOV_TYPE,
    
    `PAs` =
      comma(n_parks_cap100),
    
    `Countries` =
      n_countries_cap100,
    
    `Cells: cap 100` =
      comma(sampled_cells_cap100),
    
    `Cells: cap 200` =
      comma(sampled_cells_cap200),
    
    `Additional cells` =
      comma(extra_cells),
    
    `% cell increase` =
      percent(
        pct_more_cells,
        accuracy = 0.1
      ),
    
    `Sample share: cap 100` =
      percent(
        sample_share_cap100,
        accuracy = 0.1
      ),
    
    `Sample share: cap 200` =
      percent(
        sample_share_cap200,
        accuracy = 0.1
      ),
    
    `Change in share (pp)` =
      round(
        change_sample_share_pp,
        1
      )
  )

governance_display


iucn_display <- iucn_cap_compare %>%
  transmute(
    `IUCN category` = IUCN_CAT,
    
    `PAs` =
      comma(n_parks_cap100),
    
    `Countries` =
      n_countries_cap100,
    
    `Cells: cap 100` =
      comma(sampled_cells_cap100),
    
    `Cells: cap 200` =
      comma(sampled_cells_cap200),
    
    `Additional cells` =
      comma(extra_cells),
    
    `% cell increase` =
      percent(
        pct_more_cells,
        accuracy = 0.1
      ),
    
    `Sample share: cap 100` =
      percent(
        sample_share_cap100,
        accuracy = 0.1
      ),
    
    `Sample share: cap 200` =
      percent(
        sample_share_cap200,
        accuracy = 0.1
      ),
    
    `Change in share (pp)` =
      round(
        change_sample_share_pp,
        1
      )
  )

iucn_display

ggplot(
  governance_cap_compare,
  aes(
    x = reorder(
      GOV_TYPE,
      extra_cells
    ),
    y = extra_cells
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Where does increasing the PA cap from 100 to 200 add cells?",
    subtitle = "Additional candidate treated cells by governance type",
    x = NULL,
    y = "Additional candidate treated cells"
  ) +
  theme_minimal()

ggplot(
  iucn_cap_compare,
  aes(
    x = reorder(
      IUCN_CAT,
      extra_cells
    ),
    y = extra_cells
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Additional treated-cell capacity from a 200-cell PA cap",
    subtitle = "Relative to a 100-cell cap, by IUCN category",
    x = NULL,
    y = "Additional candidate treated cells"
  ) +
  theme_minimal()



gov_iucn_support <- cap_compare %>%
  group_by(
    cap,
    GOV_TYPE,
    IUCN_CAT
  ) %>%
  summarise(
    sampled_cells =
      sum(sample_cells),
    
    n_parks =
      sum(sample_cells > 0),
    
    n_countries =
      n_distinct(
        ISO3[sample_cells > 0]
      ),
    
    .groups = "drop"
  )

gov_iucn_replication <- gov_iucn_support %>%
  filter(
    cap == 100
  ) %>%
  select(
    GOV_TYPE,
    IUCN_CAT,
    n_parks,
    n_countries,
    sampled_cells
  ) %>%
  arrange(
    n_countries,
    n_parks
  )

gov_iucn_replication

gov_iucn_replication <- gov_iucn_replication %>%
  mutate(
    support_flag = case_when(
      n_countries < 3 ~
        "Very limited geographic replication",
      
      n_parks < 20 ~
        "Limited PA replication",
      
      TRUE ~
        "Broader replication"
    )
  )

scoping_summary <- tibble(
  Metric = c(
    "Candidate treated cells: cap 100",
    "Candidate treated cells: cap 200",
    "Additional cells from cap 200",
    "Percent increase in candidate cells",
    "Country × cohort strata",
    "Share of strata ≥100 cells: cap 100",
    "Share of strata ≥100 cells: cap 200",
    "Share of additional cells from top 10% of strata"
  ),
  
  Value = c(
    comma(
      overall_cap_compare$sampled_cells[
        overall_cap_compare$cap == 100
      ]
    ),
    
    comma(
      overall_cap_compare$sampled_cells[
        overall_cap_compare$cap == 200
      ]
    ),
    
    comma(
      overall_cap_compare$extra_cells_vs_100[
        overall_cap_compare$cap == 200
      ]
    ),
    
    percent(
      overall_cap_compare$pct_more_cells_vs_100[
        overall_cap_compare$cap == 200
      ],
      accuracy = 0.1
    ),
    
    comma(
      unique(
        country_cohort_summary$distribution$
          n_country_cohort_strata
      )[1]
    ),
    
    percent(
      country_cohort_summary$thresholds %>%
        filter(
          cap == 100,
          threshold == 100
        ) %>%
        pull(pct_strata_at_least),
      accuracy = 0.1
    ),
    
    percent(
      country_cohort_summary$thresholds %>%
        filter(
          cap == 200,
          threshold == 100
        ) %>%
        pull(pct_strata_at_least),
      accuracy = 0.1
    ),
    
    percent(
      gain_concentration$top_10pct_strata_gain,
      accuracy = 0.1
    )
  )
)

scoping_summary








################### 4 ########################









######## COUNTRY × COHORT ALLOCATION DIAGNOSTICS ########

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# ============================================================
# SETTINGS
# ============================================================

TARGET_SIZES <- c(
  200000,
  250000,
  300000
)

MIN_PER_STRATUM <- c(
  0,      # Purely proportional
  25,
  50,
  100
)

# We are carrying forward the provisional 100-cell PA cap
PA_CAP <- 100


# ============================================================
# 1. COUNTRY × COHORT CANDIDATE CAPACITY
# ============================================================

candidate_pa <- sim_long %>%
  filter(
    scenario == paste0("PA cap: ", PA_CAP)
  )


stratum_capacity <- candidate_pa %>%
  group_by(
    ISO3,
    STATUS_YR
  ) %>%
  summarise(
    capacity = sum(sample_cells),
    
    n_parks = n(),
    
    .groups = "drop"
  )


# Quick feasibility check
stratum_capacity %>%
  summarise(
    n_strata = n(),
    n_countries = n_distinct(ISO3),
    total_candidate_cells = sum(capacity),
    
    median_capacity = median(capacity),
    p10_capacity = quantile(capacity, 0.10),
    p90_capacity = quantile(capacity, 0.90)
  )


# ============================================================
# 2. CAPACITY-CONSTRAINED PROPORTIONAL ALLOCATION
# ============================================================

allocate_with_capacity <- function(
    capacity,
    weights,
    target
) {
  
  target <- min(
    target,
    sum(capacity)
  )
  
  n <- length(capacity)
  
  allocation <- integer(n)
  
  capacity_left <- capacity
  
  remaining <- target
  
  
  while (
    remaining > 0 &&
    any(capacity_left > 0)
  ) {
    
    eligible <- capacity_left > 0
    
    w <- weights
    w[!eligible] <- 0
    
    # Fall back to equal weighting if necessary
    if (sum(w) <= 0) {
      w <- as.numeric(eligible)
    }
    
    raw <-
      remaining *
      w /
      sum(w)
    
    add <- pmin(
      floor(raw),
      capacity_left
    )
    
    # If flooring allocates nothing,
    # assign one cell according to largest remainder
    if (sum(add) == 0) {
      
      fractional <-
        raw - floor(raw)
      
      candidates <-
        which(eligible)
      
      candidates <-
        candidates[
          order(
            fractional[candidates],
            decreasing = TRUE
          )
        ]
      
      n_take <- min(
        remaining,
        length(candidates)
      )
      
      add[
        candidates[
          seq_len(n_take)
        ]
      ] <- 1L
    }
    
    allocation <-
      allocation + add
    
    capacity_left <-
      capacity - allocation
    
    remaining <-
      target - sum(allocation)
  }
  
  
  allocation
}


# ============================================================
# 3. HYBRID STRATIFIED ALLOCATION
# ============================================================

allocate_hybrid <- function(
    capacity,
    target,
    minimum = 0
) {
  
  # Baseline allocation
  base <- pmin(
    capacity,
    minimum
  )
  
  
  # If target is smaller than total baseline requirement,
  # allocate proportionally within that feasible base.
  if (sum(base) >= target) {
    
    return(
      allocate_with_capacity(
        capacity = base,
        weights = capacity,
        target = target
      )
    )
  }
  
  
  remaining_target <-
    target - sum(base)
  
  remaining_capacity <-
    capacity - base
  
  
  additional <-
    allocate_with_capacity(
      capacity = remaining_capacity,
      weights = remaining_capacity,
      target = remaining_target
    )
  
  
  base + additional
}


# ============================================================
# 4. RUN ALLOCATION SENSITIVITY GRID
# ============================================================

run_allocation <- function(
    data,
    target,
    minimum
) {
  
  data %>%
    mutate(
      target_sample = target,
      
      minimum_per_stratum = minimum,
      
      allocation_rule = if_else(
        minimum == 0,
        "Proportional",
        paste0(
          "Hybrid: min ",
          minimum
        )
      ),
      
      sampled_cells =
        allocate_hybrid(
          capacity = capacity,
          target = target,
          minimum = minimum
        )
    )
}


allocation_results <- crossing(
  target_sample = TARGET_SIZES,
  minimum_per_stratum = MIN_PER_STRATUM
) %>%
  pmap_dfr(
    function(
    target_sample,
    minimum_per_stratum
    ) {
      
      run_allocation(
        data = stratum_capacity,
        target = target_sample,
        minimum = minimum_per_stratum
      )
    }
  )


# ============================================================
# 5. DIAGNOSTIC HELPERS
# ============================================================

top_share <- function(
    x,
    proportion
) {
  
  x <- sort(
    x,
    decreasing = TRUE
  )
  
  n_top <- max(
    1,
    ceiling(
      length(x) * proportion
    )
  )
  
  sum(
    head(x, n_top)
  ) /
    sum(x)
}

# ============================================================
# 6. COUNTRY × COHORT SUPPORT
# ============================================================

stratum_diagnostics <- allocation_results %>%
  group_by(
    target_sample,
    minimum_per_stratum,
    allocation_rule
  ) %>%
  
  summarise(
    realized_sample =
      sum(sampled_cells),
    
    pct_candidate_pool_used =
      realized_sample /
      sum(capacity),
    
    n_strata_represented =
      sum(sampled_cells > 0),
    
    pct_strata_represented =
      mean(sampled_cells > 0),
    
    median_cells_per_stratum =
      median(sampled_cells),
    
    p10_cells_per_stratum =
      quantile(
        sampled_cells,
        0.10
      ),
    
    p90_cells_per_stratum =
      quantile(
        sampled_cells,
        0.90
      ),
    
    pct_strata_25plus =
      mean(
        sampled_cells >= 25
      ),
    
    pct_strata_50plus =
      mean(
        sampled_cells >= 50
      ),
    
    pct_strata_100plus =
      mean(
        sampled_cells >= 100
      ),
    
    pct_strata_200plus =
      mean(
        sampled_cells >= 200
      ),
    
    top_1pct_strata_share =
      top_share(
        sampled_cells,
        0.01
      ),
    
    top_5pct_strata_share =
      top_share(
        sampled_cells,
        0.05
      ),
    
    top_10pct_strata_share =
      top_share(
        sampled_cells,
        0.10
      ),
    
    .groups = "drop"
  )


# ============================================================
# 7. COUNTRY-LEVEL CONCENTRATION
# ============================================================

country_diagnostics <- allocation_results %>%
  
  group_by(
    target_sample,
    minimum_per_stratum,
    allocation_rule,
    ISO3
  ) %>%
  
  summarise(
    sampled_cells =
      sum(sampled_cells),
    
    .groups = "drop"
  ) %>%
  
  group_by(
    target_sample,
    minimum_per_stratum,
    allocation_rule
  ) %>%
  
  summarise(
    countries_represented =
      sum(sampled_cells > 0),
    
    largest_country_share =
      max(sampled_cells) /
      sum(sampled_cells),
    
    top5_country_share =
      sum(
        head(
          sort(
            sampled_cells,
            decreasing = TRUE
          ),
          5
        )
      ) /
      sum(sampled_cells),
    
    top10_country_share =
      sum(
        head(
          sort(
            sampled_cells,
            decreasing = TRUE
          ),
          10
        )
      ) /
      sum(sampled_cells),
    
    .groups = "drop"
  )


# ============================================================
# 8. TREATMENT-COHORT BALANCE
# ============================================================

cohort_diagnostics <- allocation_results %>%
  
  group_by(
    target_sample,
    minimum_per_stratum,
    allocation_rule,
    STATUS_YR
  ) %>%
  
  summarise(
    sampled_cells =
      sum(sampled_cells),
    
    .groups = "drop"
  ) %>%
  
  group_by(
    target_sample,
    minimum_per_stratum,
    allocation_rule
  ) %>%
  
  summarise(
    smallest_cohort_share =
      min(sampled_cells) /
      sum(sampled_cells),
    
    largest_cohort_share =
      max(sampled_cells) /
      sum(sampled_cells),
    
    cohort_cv =
      sd(sampled_cells) /
      mean(sampled_cells),
    
    .groups = "drop"
  )

# ============================================================
# 9. MASTER DIAGNOSTIC TABLE
# ============================================================

allocation_summary <- stratum_diagnostics %>%
  
  left_join(
    country_diagnostics,
    by = c(
      "target_sample",
      "minimum_per_stratum",
      "allocation_rule"
    )
  ) %>%
  
  left_join(
    cohort_diagnostics,
    by = c(
      "target_sample",
      "minimum_per_stratum",
      "allocation_rule"
    )
  )


allocation_summary


allocation_display <- allocation_summary %>%
  transmute(
    
    `Target treated sample` =
      comma(target_sample),
    
    `Allocation rule` =
      allocation_rule,
    
    `Strata represented` =
      percent(
        pct_strata_represented,
        accuracy = 0.1
      ),
    
    `Strata ≥50 cells` =
      percent(
        pct_strata_50plus,
        accuracy = 0.1
      ),
    
    `Strata ≥100 cells` =
      percent(
        pct_strata_100plus,
        accuracy = 0.1
      ),
    
    `Median cells / stratum` =
      comma(
        median_cells_per_stratum,
        accuracy = 1
      ),
    
    `Top 5% stratum share` =
      percent(
        top_5pct_strata_share,
        accuracy = 0.1
      ),
    
    `Largest country share` =
      percent(
        largest_country_share,
        accuracy = 0.1
      ),
    
    `Top 5 countries` =
      percent(
        top5_country_share,
        accuracy = 0.1
      ),
    
    `Cohort CV` =
      round(
        cohort_cv,
        2
      )
  )

allocation_display


# ============================================================
# 10. SUPPORT VS. MINIMUM ALLOCATION
# ============================================================

ggplot(
  allocation_summary,
  aes(
    x = minimum_per_stratum,
    y = pct_strata_100plus,
    group = factor(target_sample),
    linetype = factor(target_sample)
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = percent
  ) +
  scale_x_continuous(
    breaks = MIN_PER_STRATUM
  ) +
  labs(
    title =
      "Country × cohort support under alternative allocation rules",
    
    subtitle =
      "Share of strata receiving at least 100 treated cells",
    
    x =
      "Minimum allocation per country × cohort stratum",
    
    y =
      "Share of strata with ≥100 cells",
    
    linetype =
      "Target treated sample"
  ) +
  theme_minimal()


ggplot(
  allocation_summary,
  aes(
    x = minimum_per_stratum,
    y = top5_country_share,
    group = factor(target_sample),
    linetype = factor(target_sample)
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = percent
  ) +
  scale_x_continuous(
    breaks = MIN_PER_STRATUM
  ) +
  labs(
    title =
      "Country concentration under alternative allocation rules",
    
    subtitle =
      "Share of treated sample contributed by five largest countries",
    
    x =
      "Minimum allocation per country × cohort stratum",
    
    y =
      "Top-five-country share",
    
    linetype =
      "Target treated sample"
  ) +
  theme_minimal()








###### Design Characteristic Support #######

######## EXPECTED GOVERNANCE / IUCN SUPPORT ########

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# ============================================================
# SETTINGS
# ============================================================

PA_CAP <- 100
TARGET_TREATED <- 250000
MIN_STRATUM <- 50


# ============================================================
# 1. SELECT PROVISIONAL DESIGN
# ============================================================

# One row per PA under the 100-cell cap
candidate_pa <- sim_long %>%
  filter(
    scenario == paste0("PA cap: ", PA_CAP)
  ) %>%
  transmute(
    across(everything()),
    pa_candidate_cells = sample_cells
  )


# Country × cohort allocation selected from previous simulation
chosen_allocation <- allocation_results %>%
  filter(
    target_sample == TARGET_TREATED,
    minimum_per_stratum == MIN_STRATUM
  ) %>%
  select(
    ISO3,
    STATUS_YR,
    stratum_capacity = capacity,
    stratum_sample_n = sampled_cells
  )

# ============================================================
# 2. ATTACH COUNTRY × COHORT SAMPLE SIZE TO EACH PA
# ============================================================

pa_expected <- candidate_pa %>%
  left_join(
    chosen_allocation,
    by = c(
      "ISO3",
      "STATUS_YR"
    )
  ) %>%
  filter(
    !is.na(stratum_sample_n),
    pa_candidate_cells > 0
  )

# ============================================================
# 3. EXPECTED NUMBER OF SAMPLED CELLS PER PA
# ============================================================

pa_expected <- pa_expected %>%
  mutate(
    expected_sampled_cells =
      stratum_sample_n *
      pa_candidate_cells /
      stratum_capacity
  )


# ============================================================
# 4. PROBABILITY THAT EACH PA IS REPRESENTED
# ============================================================

prob_pa_represented <- function(
    pa_cells,
    stratum_cells,
    sample_n
) {
  
  # No sample drawn
  if (sample_n <= 0) {
    return(0)
  }
  
  # Entire stratum sampled
  if (sample_n >= stratum_cells) {
    return(1)
  }
  
  # If there are not enough non-PA cells to draw the sample
  # without touching this PA, representation is certain.
  if (sample_n > (stratum_cells - pa_cells)) {
    return(1)
  }
  
  log_p_zero <-
    lchoose(
      stratum_cells - pa_cells,
      sample_n
    ) -
    lchoose(
      stratum_cells,
      sample_n
    )
  
  1 - exp(log_p_zero)
}


pa_expected <- pa_expected %>%
  mutate(
    prob_pa_represented = pmap_dbl(
      list(
        pa_candidate_cells,
        stratum_capacity,
        stratum_sample_n
      ),
      prob_pa_represented
    )
  )

pa_expected %>%
  summarise(
    expected_cells =
      sum(expected_sampled_cells),
    
    expected_pas =
      sum(prob_pa_represented),
    
    total_candidate_pas =
      n(),
    
    mean_pa_representation_probability =
      mean(prob_pa_represented)
  )



# ============================================================
# 5. GENERIC EXPECTED SUPPORT FUNCTION
# ============================================================

summarise_expected_support <- function(
    data,
    ...
) {
  
  data %>%
    group_by(...) %>%
    summarise(
      candidate_cells =
        sum(pa_candidate_cells),
      
      expected_sampled_cells =
        sum(expected_sampled_cells),
      
      candidate_pas =
        n(),
      
      expected_sampled_pas =
        sum(prob_pa_represented),
      
      observed_countries =
        n_distinct(ISO3),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      cell_retention =
        expected_sampled_cells /
        candidate_cells,
      
      pa_retention =
        expected_sampled_pas /
        candidate_pas,
      
      expected_sample_share =
        expected_sampled_cells /
        sum(expected_sampled_cells)
    ) %>%
    
    arrange(
      desc(expected_sampled_cells)
    )
}

governance_expected <- summarise_expected_support(
  pa_expected,
  GOV_TYPE
)

governance_expected

iucn_expected <- summarise_expected_support(
  pa_expected,
  IUCN_CAT
)

iucn_expected

gov_iucn_expected <- summarise_expected_support(
  pa_expected,
  GOV_TYPE,
  IUCN_CAT
)

gov_iucn_expected




# ============================================================
# 6. CATEGORY REPRESENTATION WITHIN COUNTRY × COHORT STRATA
# ============================================================

category_stratum_probability <- function(
    data,
    group_var
) {
  
  data %>%
    group_by(
      ISO3,
      STATUS_YR,
      {{ group_var }}
    ) %>%
    
    summarise(
      category_cells =
        sum(pa_candidate_cells),
      
      stratum_capacity =
        first(stratum_capacity),
      
      stratum_sample_n =
        first(stratum_sample_n),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      prob_category_in_stratum =
        pmap_dbl(
          list(
            category_cells,
            stratum_capacity,
            stratum_sample_n
          ),
          prob_pa_represented
        )
    )
}


expected_category_countries <- function(
    data,
    group_var
) {
  
  stratum_probs <-
    category_stratum_probability(
      data,
      {{ group_var }}
    )
  
  stratum_probs %>%
    group_by(
      ISO3,
      {{ group_var }}
    ) %>%
    
    summarise(
      prob_category_in_country =
        1 -
        prod(
          1 - prob_category_in_stratum
        ),
      
      .groups = "drop"
    ) %>%
    
    group_by(
      {{ group_var }}
    ) %>%
    
    summarise(
      expected_countries =
        sum(prob_category_in_country),
      
      candidate_countries =
        n(),
      
      .groups = "drop"
    )
}

governance_country_support <-
  expected_category_countries(
    pa_expected,
    GOV_TYPE
  )


iucn_country_support <-
  expected_category_countries(
    pa_expected,
    IUCN_CAT
  )


governance_expected <- governance_expected %>%
  left_join(
    governance_country_support,
    by = "GOV_TYPE"
  )


iucn_expected <- iucn_expected %>%
  left_join(
    iucn_country_support,
    by = "IUCN_CAT"
  )


# ============================================================
# 7. PRESENTABLE GOVERNANCE TABLE
# ============================================================

governance_display <- governance_expected %>%
  transmute(
    Governance = GOV_TYPE,
    
    `Candidate PAs` =
      comma(candidate_pas),
    
    `Expected sampled PAs` =
      comma(
        expected_sampled_pas,
        accuracy = 1
      ),
    
    `Candidate countries` =
      candidate_countries,
    
    `Expected sampled countries` =
      round(
        expected_countries,
        1
      ),
    
    `Expected sampled cells` =
      comma(
        expected_sampled_cells,
        accuracy = 1
      ),
    
    `Share of treated sample` =
      percent(
        expected_sample_share,
        accuracy = 0.1
      ),
    
    `PA retention` =
      percent(
        pa_retention,
        accuracy = 0.1
      )
  )

governance_display



# ============================================================
# 8. PRESENTABLE IUCN TABLE
# ============================================================

iucn_display <- iucn_expected %>%
  transmute(
    `IUCN category` = IUCN_CAT,
    
    `Candidate PAs` =
      comma(candidate_pas),
    
    `Expected sampled PAs` =
      comma(
        expected_sampled_pas,
        accuracy = 1
      ),
    
    `Candidate countries` =
      candidate_countries,
    
    `Expected sampled countries` =
      round(
        expected_countries,
        1
      ),
    
    `Expected sampled cells` =
      comma(
        expected_sampled_cells,
        accuracy = 1
      ),
    
    `Share of treated sample` =
      percent(
        expected_sample_share,
        accuracy = 0.1
      ),
    
    `PA retention` =
      percent(
        pa_retention,
        accuracy = 0.1
      )
  )

iucn_display


# ============================================================
# 9. GOVERNANCE × IUCN REPLICATION
# ============================================================

gov_iucn_replication <- gov_iucn_expected %>%
  transmute(
    GOV_TYPE,
    IUCN_CAT,
    
    candidate_pas,
    
    expected_sampled_pas,
    
    observed_countries,
    
    expected_sampled_cells,
    
    expected_sample_share
  ) %>%
  
  arrange(
    expected_sampled_pas
  )

gov_iucn_replication


gov_iucn_replication <- gov_iucn_replication %>%
  mutate(
    support_flag = case_when(
      
      observed_countries < 3 ~
        "Very limited geographic support",
      
      expected_sampled_pas < 10 ~
        "Very limited PA replication",
      
      expected_sampled_pas < 30 ~
        "Limited PA replication",
      
      TRUE ~
        "Broader support"
    )
  )



ggplot(
  governance_expected,
  aes(
    x = reorder(
      GOV_TYPE,
      expected_sampled_cells
    ),
    y = expected_sampled_cells
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title =
      "Expected treated-cell sample by governance type",
    
    subtitle =
      "PA cap = 100; treated target = 250,000; country × cohort minimum = 50",
    
    x = NULL,
    y = "Expected sampled treated cells"
  ) +
  theme_minimal()


ggplot(
  governance_expected,
  aes(
    x = expected_sampled_pas,
    y = expected_sampled_cells,
    label = GOV_TYPE
  )
) +
  geom_point(size = 2.5) +
  geom_text(
    check_overlap = TRUE,
    nudge_y = 1500
  ) +
  scale_x_continuous(
    labels = comma
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title =
      "Cell support versus PA replication by governance type",
    
    subtitle =
      "Expected support under provisional treated-sampling design",
    
    x =
      "Expected number of sampled PAs",
    
    y =
      "Expected number of sampled treated cells"
  ) +
  theme_minimal()




weak_governance_support <- governance_expected %>%
  arrange(
    expected_sampled_pas,
    expected_countries
  ) %>%
  select(
    GOV_TYPE,
    expected_sampled_cells,
    expected_sampled_pas,
    expected_countries,
    expected_sample_share
  )

weak_governance_support


weak_iucn_support <- iucn_expected %>%
  arrange(
    expected_sampled_pas,
    expected_countries
  ) %>%
  select(
    IUCN_CAT,
    expected_sampled_cells,
    expected_sampled_pas,
    expected_countries,
    expected_sample_share
  )

weak_iucn_support









##################### 5 ########################








######## WITHIN-COUNTRY PA DESIGN OVERLAP ########

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# ============================================================
# SETTINGS
# ============================================================

GOV_EXCLUDE <- c(
  "Not Reported"
)

IUCN_EXCLUDE <- c(
  "Not Reported",
  "Not Assigned",
  "Not Applicable"
)

# Diagnostic thresholds only — not inferential cutoffs
ALT_PA_THRESHOLDS <- c(
  1, 5, 10, 25
)


######## WITHIN-COUNTRY PA DESIGN OVERLAP ########

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# ============================================================
# SETTINGS
# ============================================================

GOV_EXCLUDE <- c(
  "Not Reported"
)

IUCN_EXCLUDE <- c(
  "Not Reported",
  "Not Assigned",
  "Not Applicable"
)

# Diagnostic thresholds only — not inferential cutoffs
ALT_PA_THRESHOLDS <- c(
  1, 5, 10, 25
)


# ============================================================
# 1. BUILD CATEGORY × COUNTRY SUPPORT TABLE
# ============================================================

build_country_overlap <- function(
    data,
    category_var,
    exclude = character()
) {
  
  category_name <- rlang::as_name(
    rlang::ensym(category_var)
  )
  
  d <- data %>%
    filter(
      !is.na({{ category_var }}),
      !{{ category_var }} %in% exclude
    )
  
  category_country <- d %>%
    group_by(
      ISO3,
      {{ category_var }}
    ) %>%
    summarise(
      candidate_pas = n(),
      
      expected_sampled_pas =
        sum(prob_pa_represented),
      
      expected_sampled_cells =
        sum(expected_sampled_cells),
      
      .groups = "drop"
    )
  
  country_totals <- category_country %>%
    group_by(ISO3) %>%
    summarise(
      n_categories =
        n_distinct({{ category_var }}),
      
      country_expected_pas =
        sum(expected_sampled_pas),
      
      country_expected_cells =
        sum(expected_sampled_cells),
      
      .groups = "drop"
    )
  
  category_country %>%
    left_join(
      country_totals,
      by = "ISO3"
    ) %>%
    mutate(
      n_alternative_categories =
        n_categories - 1,
      
      alternative_expected_pas =
        country_expected_pas -
        expected_sampled_pas,
      
      alternative_expected_cells =
        country_expected_cells -
        expected_sampled_cells,
      
      has_within_country_alternative =
        n_alternative_categories > 0
    )
}

governance_country_overlap <-
  build_country_overlap(
    pa_expected,
    GOV_TYPE,
    exclude = GOV_EXCLUDE
  )


iucn_country_overlap <-
  build_country_overlap(
    pa_expected,
    IUCN_CAT,
    exclude = IUCN_EXCLUDE
  )


# ============================================================
# 2. CATEGORY-LEVEL OVERLAP SUMMARY
# ============================================================

summarise_country_overlap <- function(
    data,
    category_var
) {
  
  data %>%
    group_by(
      {{ category_var }}
    ) %>%
    summarise(
      countries_present =
        n(),
      
      countries_with_alternative =
        sum(
          has_within_country_alternative
        ),
      
      pct_countries_with_alternative =
        mean(
          has_within_country_alternative
        ),
      
      expected_sampled_pas =
        sum(expected_sampled_pas),
      
      expected_pas_in_overlap_countries =
        sum(
          expected_sampled_pas[
            has_within_country_alternative
          ]
        ),
      
      expected_sampled_cells =
        sum(expected_sampled_cells),
      
      expected_cells_in_overlap_countries =
        sum(
          expected_sampled_cells[
            has_within_country_alternative
          ]
        ),
      
      median_alt_expected_pas =
        median(
          alternative_expected_pas
        ),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      pct_pas_in_overlap_countries =
        expected_pas_in_overlap_countries /
        expected_sampled_pas,
      
      pct_cells_in_overlap_countries =
        expected_cells_in_overlap_countries /
        expected_sampled_cells
    ) %>%
    
    arrange(
      pct_countries_with_alternative,
      expected_sampled_pas
    )
}

governance_overlap_summary <-
  summarise_country_overlap(
    governance_country_overlap,
    GOV_TYPE
  )


iucn_overlap_summary <-
  summarise_country_overlap(
    iucn_country_overlap,
    IUCN_CAT
  )

governance_overlap_display <-
  governance_overlap_summary %>%
  
  transmute(
    Governance = GOV_TYPE,
    
    `Countries present` =
      countries_present,
    
    `Countries with alternative governance` =
      countries_with_alternative,
    
    `% countries with alternative` =
      percent(
        pct_countries_with_alternative,
        accuracy = 0.1
      ),
    
    `Expected sampled PAs` =
      comma(
        expected_sampled_pas,
        accuracy = 1
      ),
    
    `% sampled PAs in overlap countries` =
      percent(
        pct_pas_in_overlap_countries,
        accuracy = 0.1
      ),
    
    `Expected sampled cells` =
      comma(
        expected_sampled_cells,
        accuracy = 1
      ),
    
    `Median alternative PAs in same country` =
      round(
        median_alt_expected_pas,
        1
      )
  )

governance_overlap_display



iucn_overlap_display <-
  iucn_overlap_summary %>%
  
  transmute(
    `IUCN category` = IUCN_CAT,
    
    `Countries present` =
      countries_present,
    
    `Countries with alternative IUCN categories` =
      countries_with_alternative,
    
    `% countries with alternative` =
      percent(
        pct_countries_with_alternative,
        accuracy = 0.1
      ),
    
    `Expected sampled PAs` =
      comma(
        expected_sampled_pas,
        accuracy = 1
      ),
    
    `% sampled PAs in overlap countries` =
      percent(
        pct_pas_in_overlap_countries,
        accuracy = 0.1
      ),
    
    `Expected sampled cells` =
      comma(
        expected_sampled_cells,
        accuracy = 1
      ),
    
    `Median alternative PAs in same country` =
      round(
        median_alt_expected_pas,
        1
      )
  )

iucn_overlap_display


# ============================================================
# 3. ALTERNATIVE-PA SUPPORT SENSITIVITY
# ============================================================

summarise_alt_thresholds <- function(
    data,
    category_var,
    thresholds = c(1, 5, 10, 25)
) {
  
  map_dfr(
    thresholds,
    function(threshold) {
      
      data %>%
        group_by(
          {{ category_var }}
        ) %>%
        summarise(
          threshold =
            threshold,
          
          countries_meeting_threshold =
            sum(
              alternative_expected_pas >=
                threshold
            ),
          
          pct_countries_meeting_threshold =
            mean(
              alternative_expected_pas >=
                threshold
            ),
          
          .groups = "drop"
        )
    }
  )
}

governance_threshold_support <-
  summarise_alt_thresholds(
    governance_country_overlap,
    GOV_TYPE,
    ALT_PA_THRESHOLDS
  )


iucn_threshold_support <-
  summarise_alt_thresholds(
    iucn_country_overlap,
    IUCN_CAT,
    ALT_PA_THRESHOLDS
  )

governance_threshold_display <-
  governance_threshold_support %>%
  
  mutate(
    pct_countries_meeting_threshold =
      percent(
        pct_countries_meeting_threshold,
        accuracy = 0.1
      )
  )

governance_threshold_display




# ============================================================
# PAIRWISE WITHIN-COUNTRY OVERLAP
# ============================================================

pairwise_country_overlap <- function(
    country_data,
    category_var
) {
  
  category_sym <- rlang::ensym(category_var)
  
  left <- country_data %>%
    select(
      ISO3,
      !!category_sym,
      expected_sampled_pas,
      expected_sampled_cells
    ) %>%
    rename(
      category_A = !!category_sym,
      expected_pas_A = expected_sampled_pas,
      expected_cells_A = expected_sampled_cells
    )
  
  right <- country_data %>%
    select(
      ISO3,
      !!category_sym,
      expected_sampled_pas,
      expected_sampled_cells
    ) %>%
    rename(
      category_B = !!category_sym,
      expected_pas_B = expected_sampled_pas,
      expected_cells_B = expected_sampled_cells
    )
  
  left %>%
    inner_join(
      right,
      by = "ISO3",
      relationship = "many-to-many"
    ) %>%
    
    # Keep each unordered pair once
    filter(
      category_A < category_B
    ) %>%
    
    group_by(
      category_A,
      category_B
    ) %>%
    
    summarise(
      overlap_countries =
        n_distinct(ISO3),
      
      expected_pas_A =
        sum(expected_pas_A),
      
      expected_pas_B =
        sum(expected_pas_B),
      
      expected_cells_A =
        sum(expected_cells_A),
      
      expected_cells_B =
        sum(expected_cells_B),
      
      .groups = "drop"
    ) %>%
    
    arrange(
      desc(overlap_countries)
    )
}

governance_pair_overlap <-
  pairwise_country_overlap(
    governance_country_overlap,
    GOV_TYPE
  )


iucn_pair_overlap <-
  pairwise_country_overlap(
    iucn_country_overlap,
    IUCN_CAT
  )

governance_pair_overlap



ggplot(
  governance_pair_overlap,
  aes(
    x = category_A,
    y = category_B,
    fill = overlap_countries
  )
) +
  geom_tile() +
  geom_text(
    aes(
      label = overlap_countries
    ),
    size = 3
  ) +
  coord_fixed() +
  labs(
    title =
      "Within-country overlap among PA governance types",
    
    subtitle =
      "Number of countries containing both governance regimes",
    
    x = NULL,
    y = NULL,
    fill = "Countries"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )



ggplot(
  iucn_pair_overlap,
  aes(
    x = category_A,
    y = category_B,
    fill = overlap_countries
  )
) +
  geom_tile() +
  geom_text(
    aes(
      label = overlap_countries
    ),
    size = 3
  ) +
  coord_fixed() +
  labs(
    title =
      "Within-country overlap among IUCN categories",
    
    subtitle =
      "Number of countries containing both categories",
    
    x = NULL,
    y = NULL,
    fill = "Countries"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )



# ============================================================
# 5. COUNTRY × COHORT DESIGN DIVERSITY
# ============================================================

summarise_country_cohort_overlap <- function(
    data,
    category_var,
    exclude = character()
) {
  
  data %>%
    filter(
      !is.na({{ category_var }}),
      !{{ category_var }} %in% exclude
    ) %>%
    
    group_by(
      ISO3,
      STATUS_YR
    ) %>%
    
    summarise(
      n_categories =
        n_distinct(
          {{ category_var }}
        ),
      
      expected_pas =
        sum(
          prob_pa_represented
        ),
      
      expected_cells =
        sum(
          expected_sampled_cells
        ),
      
      .groups = "drop"
    ) %>%
    
    summarise(
      n_country_cohort_strata =
        n(),
      
      pct_strata_multiple_designs =
        mean(
          n_categories >= 2
        ),
      
      pct_strata_3plus_designs =
        mean(
          n_categories >= 3
        ),
      
      median_designs_per_stratum =
        median(n_categories)
    )
}


governance_country_cohort_overlap <-
  summarise_country_cohort_overlap(
    pa_expected,
    GOV_TYPE,
    GOV_EXCLUDE
  )


iucn_country_cohort_overlap <-
  summarise_country_cohort_overlap(
    pa_expected,
    IUCN_CAT,
    IUCN_EXCLUDE
  )


governance_country_cohort_overlap
iucn_country_cohort_overlap

