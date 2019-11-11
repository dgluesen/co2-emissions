# ------------------------------------------------------------------------------------- #
# co2-emissions                                                                         #
# DATA STUDY ABOUT CO2 EMISSIONS AND AREA BELOW 5M OF COUNTRIES AROUND THE WORLD        #
# Author: Dennis Gluesenkamp                                                            #
# ------------------------------------------------------------------------------------- #


# ---- Libraries/packages used in this script ----
library(dplyr)                            # data manipulation focused on data frames
library(tidyr)                            # create clean data; changing shape and hierarchy of datasets
library(ggplot2)                          # general plotting engine
library(showtext)                         # usage of Google fonts

library(gganimate)                        # animations in ggplot
library(plyr)                             # splitting, applying and combining data
library(gdata)                            # data manipulation, including reading XLS(X)-files


# ---- Style configuration ----
font_add_google(name = 'Exo', family = 'Exo')
font_add_google(name = 'PT Sans', family = 'PT Sans')
showtext_auto()
fontTitle = 'Exo'
fontText  = 'PT Sans'
fontSize  = 20

cBackground = '#191816'
cTitle      = '#dabaa4'
cText       = '#e4d1d1'
cBar        = c('#1c5e7d', '#ac2d2d')

# Note: The following specifications are aligned to the later used ggsave statement which
# is optimized for a squared output.
themeCO2 <- theme(
  plot.background    = element_rect(fill       = cBackground),
  panel.background   = element_rect(fill       = cBackground),
  plot.title         = element_text(size       = 1.75 * fontSize,
                                    face       = 'plain',
                                    family     = fontTitle,
                                    color      = cTitle,
                                    hjust      = 1.0,
                                    margin     = unit(c(1.0, 0.0, 2.0, 0.0), 'pt')),
  plot.subtitle      = element_text(size       = 1.25 * fontSize,
                                    face       = 'italic',
                                    family     = fontText,
                                    color      = cText,
                                    hjust      = 1.0,
                                    lineheight = 0.275,
                                    margin     = unit(c(0.0, 0.0, 0.0, 0.0), 'pt')),
  plot.caption       = element_text(size       = 0.70 * fontSize,
                                    face       = 'plain',
                                    family     = fontText,
                                    color      = cText,
                                    hjust      = 1.0,
                                    lineheight = 0.3,
                                    margin     = unit(c(-0.85, 0.0, -0.75, 0.0), 'pt')),
  axis.title         = element_blank(),
  axis.text.x        = element_blank(),
  axis.text.y        = element_text(size       = 1.05 * fontSize,
                                    face       = 'plain',
                                    family     = fontText,
                                    color      = cText,
                                    hjust      = 1.0,
                                    margin     = unit(c(0.0, 1.5, 0.0, 0.0), 'pt')),
  panel.border       = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color    = cTitle,
                                    size     = 0.1,
                                    linetype = 'dotted'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.ticks.x       = element_blank(),
  axis.ticks.y       = element_blank(),
  axis.line.x.top    = element_blank(),
  axis.line.x.bottom = element_blank(),
  axis.line.y.left   = element_blank(),
  axis.line.y.right  = element_blank(),
  
  legend.position   = 'none',
  
  complete = FALSE
)


# ---- Country ISO-code ----
iso <- ISOcodes::ISO_3166_1 %>%
  dplyr::add_row(Alpha_2 = 'XK',
                 Alpha_3 = 'XKX',
                 Name = 'Kosovo') %>%
  dplyr::mutate(Name = case_when(
    Name == 'Czechia'                ~ 'Czech Republic',
    Name == 'Macedonia, Republic of' ~ 'Macedonia',
    TRUE                             ~ Name
  )) %>%
  dplyr::left_join(dplyr::mutate(ggplot2::map_data('world'),
                                 region = case_when(
                                   region == 'UK'   ~ 'United Kingdom',
                                   TRUE             ~ region
                                 )),
                   by = c('Name' = 'region')) %>%
  dplyr::select(Alpha_2, Alpha_3, Name, long, lat, group) %>%
  dplyr::rename(code2 = Alpha_2,
                code3 = Alpha_3,
                country = Name,
                mapgroup = group)


# ---- CO2 emissions per capita ----
# Carbon dioxide emissions are those stemming from the burning of fossil fuels and the
# manufacture of cement. They include carbon dioxide produced during consumption of
# solid, liquid, and gas fuels and gas flaring.
# Source: Carbon Dioxide Information Analysis Center, Environmental Sciences Division,
# Oak Ridge National Laboratory, Tennessee, United States.
# doi: 10.3334/CDIAC/00001_V2017
co2 <- utils::read.csv('dat/co2emissionspercapita.csv', sep = ',', skip = 4, stringsAsFactors = FALSE) %>%
  dplyr::select(-Country.Name, -Indicator.Name, -Indicator.Code, -X) %>%
  dplyr::rename(code3 = Country.Code,
                co2 = X2014) %>%
  dplyr::select(code3, co2) %>%
  dplyr::filter(!is.na(co2))
# %>%
#   dplyr::left_join(iso, by = 'code3') %>%
#   dplyr::filter(!is.na(code2)) %>%
#   dplyr::select(20:22)


# ---- Land area where elevation is below 5 meters (% of total land area) ----
# Land area below 5m is the percentage of total land where the elevation is 5 meters or
# less.
# Source: Center for International Earth Science Information Network (CIESIN)/Columbia
# University. 2013. Urban-Rural Population and Land Area Estimates Version 2. Palisades,
# NY: NASA Socioeconomic Data and Applications Center ( SEDAC ).
# sedac.ciesin.columbia.edu/data/set/lecz-urban-rural-population-land-area-estimates-v2.
# doi: 10.7927/H4MW2F2J
b5m <- utils::read.csv('dat/landareabelow5m.csv', sep = ',', skip = 4, stringsAsFactors = FALSE) %>%
  dplyr::select(-Country.Name, -Indicator.Name, -Indicator.Code, -X) %>%
  dplyr::rename(code3 = Country.Code,
                area = X2010) %>%
  dplyr::select(code3, area) %>%
  dplyr::filter(!is.na(area))
level_order <- dplyr::arrange(b5m, area)$code3

# ---- Creation of pooling data frame and annotation text ----
df <- iso %>%
  dplyr::left_join(co2, by = 'code3') %>%          # join CO2 data
  dplyr::left_join(b5m, by = 'code3') %>%          # join area data
  dplyr::filter(!is.na(country)) %>%               # exclude country groups
  dplyr::select(code3, co2, area) %>%
  base::unique() %>%
  dplyr::filter(!is.na(co2) & !is.na(area)) %>%    # use only countries with full data 
  dplyr::mutate(area = -area) %>%                  # switch sign to draw bars top-down
  tidyr::gather('indicator', 'value', -code3)      # transpose for visualization purposes
df$code3 <- factor(df$code3, levels = level_order)

df_annotation1 <- data.frame('text' = c('Annual CO2 emissions\nin tons per capita',
                                        'Percentage of land area\nwith elevation below 5m'),
                             'posX' = c('FIN', 'ISL'),
                             'posY' = c(37.5, -37.5))

df_annotation2 <- data.frame('text' = c('Each vertical, red-blue pair of bars\nrepresents a country, sorted by area below 5m'),
                             'posX' = c('LUX'),
                             'posY' = c(-6.0))

# ---- Creation of viz for CO2 emission per capita and land area below 5m ----
# Drawing chart
plotCO2Area <- ggplot() +
  geom_bar(data  = df,
           stat  = 'identity',
           width = 1.0,
           aes(x    = code3,
               y    = value,
               fill = indicator)) +
  geom_text(data       = df_annotation1,
            color      = c(cBar[2], cBar[1]),
            family     = fontTitle,
            fontface   = 'bold',
            lineheight = 0.3,
            size       = 0.45 * fontSize,
            aes(x     = posX,
                y     = posY,
                label = text)) +
  geom_text(data       = df_annotation2,
            color      = cText,
            family     = fontTitle,
            fontface   = 'plain',
            lineheight = 0.3,
            size       = 0.30 * fontSize,
            hjust      = 0.0,
            aes(x     = posX,
                y     = posY,
                label = text)) +
  scale_y_continuous(limits = c(-57.5, +50),
                     breaks = c(-50, -25, 0, 25, 50),
                     labels = c('50%', '25%', '0', '25t/c', '50t/c')) +
  scale_fill_manual(values = c(cBar[1], cBar[2])) +
  labs(title    = 'National CO2 emissions and area below 5m',
       subtitle = 'Few perceive the effects of climate change on sea levels\nbut all bear the responsibility',
       x        = NULL,
       y        = NULL,
       caption  = 'Data sources: doi: 10.3334/CDIAC/00001_V2017 (CO2 emissions per capita, 2014)\ndoi: 10.7927/H4MW2F2J (Land area below 5m in percentage, 2010',
       tag      = NULL) +
  themeCO2

# Export chart
ggsave(
  filename = 'out/co2area.png',
  plot = plotCO2Area,
  type = 'cairo',
  width = 91.5,
  height = 91.5,
  units = 'mm'
)
