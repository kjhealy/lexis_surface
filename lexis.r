library(tidyverse)
library(janitor)

library(showtext)
showtext_auto()


### ---------------------------------------
### Skip if you don't have these fonts
###
library(myriad)
import_myriad_semi()

theme_set(theme_myriad_semi())
### ---------------------------------------

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(
  fig.path='figures',
  cache.path='cache',
  dev=c("png","pdf"),
  fig.width=14,
  fig.height=7,
  dpi=300,
  fig.show='hold',
  fig.lp="fig:",
  cache=TRUE,
  par=TRUE,
  echo=FALSE,
  warning = FALSE,
  results="hide",
  message=FALSE)

###--------------------------------------------------
### Data needs to be downloaded from mortality.org
### After registering with them, it's at 
### https://www.mortality.org/hmd/zip/by_statistic/death_rates.zip
###--------------------------------------------------

path <- "data/Mx_1x1/"
path5 <- "data/Mx_1x5/"

## Make a "figures" subdirectory in the working directory if one
## doesn't already exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)


###--------------------------------------------------
### Countries
###--------------------------------------------------

britain <- read_table(paste0(path, "GBRTENW.Mx_1x1.txt"), skip = 2, na = ".") %>%
  clean_names()

britain$age <- as.integer(recode(britain$age, "110+" = "110"))

britain <- britain %>% mutate(ratio = male / female,
                              deciles = cut(ratio, 
                                            breaks = quantile(ratio, probs = seq(0, 1, 0.1), na.rm = TRUE)),
                              pct_diff = ((male - female) / (male + female))*100,
                              bin_ratio = ntile(ratio, 100))



france <- read_table(paste0(path, "FRATNP.Mx_1x1.txt"), skip = 2, na = ".") %>% clean_names()
france$age <- as.integer(recode(france$age, "110+" = "110"))

france <- france %>% mutate(ratio = male / female,
                            deciles = cut(ratio, breaks = quantile(ratio, probs = seq(0, 1, 0.1), na.rm = TRUE)),
                            pct_diff = ((male - female) / (male + female))*100,
                            bin_ratio = ntile(ratio, 100))



###--------------------------------------------------
### Britain
###--------------------------------------------------

p <- ggplot(britain, aes(x = year, y = age, fill = bin_ratio))
p_out <- p + geom_tile(color = "white", size = 0.01) +
    scale_fill_viridis_c(option = "A") +
    guides(fill = guide_legend(label.position = "bottom", title.position = "top")) +
    labs(x = "Year", y = "Age", fill = "Percentile of Difference between\nMale and Female Death Rates",
         title = "Ratio of Male to Female Mortality Rates in England and Wales, 1841-2016",
         subtitle = "After Schöley & Willekens (2017), 'Visualizing compositional data on the Lexis surface'",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out

ggsave("figures/britain_ratio2.png", p_out, height = 8, width = 10)
ggsave("figures/britain_ratio2.pdf", p_out, height = 8, width = 10)



p <- ggplot(subset(britain, age < 101), aes(x = year, y = age, fill = ntile(female, 100)))
p_out_w <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1845, 2015, by = 15)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    ylim(c(0, 100)) +
    labs(x = "Year", y = "Age", fill = "Female Death Rate Percentile",
         title = "Female Mortality 1841-2016",
         subtitle = "Binned by percentile",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out_w <- p_out_w + theme(axis.text = element_text(colour="white", size = rel(1.2)), 
                axis.title = element_text(colour="white", size = rel(1.2)),
                panel.background = element_rect(color = "black", fill = "black"), 
                plot.background = element_rect(color = "black", fill = "black")) 
ggsave("figures/britain_women.png", p_out_w, height = 9, width = 10)
ggsave("figures/britain_women.pdf", p_out_w, height = 9, width = 10)



p <- ggplot(subset(britain, age < 101), aes(x = year, y = age, fill = ntile(male, 100)))
p_out <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1845, 2015, by = 15)) +
    ylim(c(0, 100)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    labs(x = "Year", y = "Age", fill = "Male Death Rate Percentile",
         title = "Male Mortality Rates in England and Wales, 1841-2016",
         subtitle = "Binned by percentile",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out <- p_out + theme(axis.text = element_text(colour="white", size = rel(1.2)), 
              axis.title = element_text(colour="white", size = rel(1.2)),
              panel.background = element_rect(color = "black", fill = "black"), 
              plot.background = element_rect(color = "black", fill = "black")) 
ggsave("figures/britain_men.png", p_out, height = 9, width = 10)
ggsave("figures/britain_men.pdf", p_out, height = 9, width = 10)



p <- ggplot(subset(usa, age < 101), aes(x = year, y = age, fill = ntile(male, 100)))
p_out <- p + geom_raster() +
  scale_fill_viridis_c(option = "A", direction = -1) +
  scale_x_continuous(breaks = seq(1845, 2015, by = 15)) +
  ylim(c(0, 100)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  labs(x = "Year", y = "Age") +
  theme(legend.position = "top",
        legend.title = element_text(size = 8))

p_out <- p_out + theme(axis.text = element_text(colour="white", size = rel(1.2)), 
                       axis.title = element_text(colour="white", size = rel(1.2)),
                       panel.background = element_rect(color = "black", fill = "black"), 
                       plot.background = element_rect(color = "black", fill = "black")) 
ggsave("figures/usa_men.png", p_out, height = 9, width = 10)
ggsave("figures/usa_men.pdf", p_out, height = 9, width = 10)


p_out + theme(axis.text = element_text(colour="white", size = rel(1.2)), 
              axis.title = element_text(colour="white", size = rel(1.2)),
              panel.background = element_rect(color = "black", fill = "black"), 
              plot.background = element_rect(color = "black", fill = "black")) 


###--------------------------------------------------
### France
###--------------------------------------------------


p <- ggplot(subset(france, age < 101), aes(x = year, y = age, fill = ntile(female, 100)))
p_out <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1820, 2015, by = 25)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    ylim(c(0, 100)) +
    labs(x = "Year", y = "Age", fill = "Female Death Rate Percentile",
         title = "Female Mortality Rates in France, 1816-2016",
         subtitle = "Binned by percentile",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out
ggsave("figures/france_women.png", p_out, height = 8, width = 12)
ggsave("figures/france_women.pdf", p_out, height = 8, width = 12)


### Poster

p <- ggplot(data = subset(france, age < 101), 
            mapping = aes(x = year, 
                          y = age, fill = ntile(male, 100)))
p_out_male <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1820, 2015, by = 15)) +
    ylim(c(0, 100)) +
    guides(fill = FALSE) +
    labs(x = NULL, y = NULL)

p_out_male
ggsave("figures/france_men_poster.pdf", p_out_male, height = 6, width = 15)


p <- ggplot(subset(france, age < 101), aes(x = year, y = age, fill = ntile(female, 100)))
p_out_female <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1820, 2015, by = 25)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    ylim(c(0, 100)) +
    guides(fill = FALSE) +
    labs(x = NULL, y = NULL)

p_out_female
ggsave("figures/france_women_poster.pdf", p_out_female, height = 6, width = 15)





###--------------------------------------------------
### Relative Mortality
###--------------------------------------------------


relpal_m <- RColorBrewer::brewer.pal(7, "Blues")
relpal_f <- RColorBrewer::brewer.pal(4, "Oranges")

relmort_pal <- c(rev(relpal_f)[1:3], "#FFFFFF", relpal_m)


## Extremely skewed by war deaths, so the key in this figure is skewed also, 
## probably beyond good practice.
france2 <- france %>% filter(ratio != Inf, ratio != 0) %>%
    mutate(ratio = male / female,
           deciles = Hmisc::cut2(ratio, g = 10, digits = 2),
           manual_cut = Hmisc::cut2(ratio, cuts = c(0.25, 0.5, 0.75, 0.98, 1.02, 
                                                    1.25, 1.5, 1.75, 2, 5, 10, 30), digits = 2),
           pct_diff = ((male - female) / (male + female))*100,
           bin_ratio = ntile(ratio, 100))


levs <- levels(france2$manual_cut)
levs <- str_replace(levs, ", ", "-")
levs <- str_replace(levs, ",", "-")
levs <- str_replace(levs, "\\[ ", "")
levs <- str_replace(levs, "\\[", "")
levs <- str_replace(levs, "\\]", "")
levs <- str_replace(levs, "\\)", "")
levs <- str_replace_all(levs, "\\.00", "")
levs <- str_replace(levs, "50", "5")
#levs <- paste0(levs, "x")


p <- ggplot(subset(france2, age < 101), aes(x = year, y = age, fill = deciles))
p_out <- p + geom_tile() +
    scale_fill_manual(values = relmort_pal) +
    scale_x_continuous(breaks = seq(1800, 2016, by = 20)) +
    coord_equal() +
    ylim(c(0, 100)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top",
                               label.position = "bottom", keywidth = 2)) +
    labs(x = "Year", y = "Age", fill = "Ratio of Male to Female Death Rates",
         title = "Relative Mortality Rates in France, 1816-2016",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 18))

p_out

ggsave("figures/france_relmort.pdf", p_out, height = 8, width = 12)



britain2 <- britain %>% filter(ratio != Inf, ratio != 0) %>%
    mutate(ratio = male / female,
           deciles = Hmisc::cut2(ratio, g = 10, digits = 2),
           ## manual_cut = Hmisc::cut2(ratio, cuts = c(0.25, 0.5, 0.75, 0.98, 1.02, 1.25, 1.5, 1.75, 2, 5, 10, 30), digits = 2),
           pct_diff = ((male - female) / (male + female))*100,
           bin_ratio = ntile(ratio, 100))

p <- ggplot(subset(britain2, age < 101), aes(x = year, y = age, fill = ratio))
p_out <- p + geom_tile() +
    scale_fill_gradient2() + 
#    scale_fill_manual(values = relmort_pal) +
    scale_x_continuous(breaks = seq(1844, 2016, by = 20)) +
    coord_equal() +
    ylim(c(0, 100)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top",
                               label.position = "bottom", keywidth = 2)) +
    labs(x = "Year", y = "Age", fill = "Ratio of Male to Female Death Rates",
         title = "Relative Mortality Rates in England and Wales, 1841-2016",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 18))

p_out

ggsave("figures/france_relmort.pdf", p_out, height = 8, width = 12)



###--------------------------------------------------
### 3D surface
###--------------------------------------------------

library(plotly)

## Axis labels

axx <- list(
    tickmode = "array",
    tickvals = (seq(0, 100, 10)),
    ticktext = as.character(seq(0, 100, 10)),
  title = "Age"
)

axy <- list(
    tickmode = "array",
    tickvals = (seq(0, 200, 25)),
    ticktext = as.character(seq(1816, 2016, 25)),
    title = "Year"
)

axz <- list(
    title = "Mortality (%ile)"
)


###--------------------------------------------------
### Women
###--------------------------------------------------

## Reshape the data

fra_surf <- france2 %>% select(year, age, female)
fra_surf <- fra_surf %>% mutate(ptile = ntile(female, 100)) %>% select(-one_of("female"))
fra_surf <- data.frame(spread(fra_surf, age, ptile))

rownames(fra_surf) <- fra_surf$year
colnames(fra_surf) <- as.character(c(1:ncol(fra_surf)))

fra_surf <- fra_surf[,-1]
fra_surf <- fra_surf[,c(1:100)]

p <- plot_ly(z = ~ as.matrix(fra_surf)) %>%
    add_surface() %>%
    layout(title = "Female Mortality Rates in France",
           xaxis = axx,
           yaxis = axy,
           scene = list(
               xaxis = axx,
               yaxis = axy,
               zaxis = axz,
               aspectmode='manual',
               aspectratio = list(x=2, y=2, z=1)))

## Save
htmlwidgets::saveWidget(as_widget(p), "/Users/kjhealy/Documents/data/misc/lexis_surface/html/females.html")


###--------------------------------------------------
### Same for men
fra_surf <- france2 %>% select(year, age, male)
fra_surf <- fra_surf %>% mutate(ptile = ntile(male, 100)) %>% select(-one_of("male"))
fra_surf <- data.frame(spread(fra_surf, age, ptile))

rownames(fra_surf) <- fra_surf$year
colnames(fra_surf) <- as.character(c(1:ncol(fra_surf)))

fra_surf <- fra_surf[,-1]
fra_surf <- fra_surf[,c(1:100)]

p <- plot_ly(z = ~ as.matrix(fra_surf)) %>%
    add_surface() %>%
    layout(title = "Male Mortality Rates in France",
           xaxis = axx,
           yaxis = axy,
           scene = list(
               xaxis = axx,
               yaxis = axy,
               zaxis = axz,
               aspectmode='manual',
               aspectratio = list(x=2, y=2, z=1)))

htmlwidgets::saveWidget(as_widget(p), "/Users/kjhealy/Documents/data/misc/lexis_surface/html/males.html")

###--------------------------------------------------

### Try a mesh instead of a surface, for the relative mortality plot
p <-  plot_ly(subset(france2, age<100)) %>%
    add_mesh(x = ~age,
        y = ~year,
        z = ~total,
        intensity=~bin_ratio,
        colors = colorRamp(c("white", scales::muted("blue")))) %>%
    layout(
        xaxis = axx,
        yaxis = axy,
        scene = list(
        aspectmode='manual',
        aspectratio = list(x=2, y=2, z=1.2)))

p

### 
### Flu
### 

## Everyone
library(here)
library(janitor)
library(tidyverse)

## Where the data is 
path <- "data/Mx_1x1/"

## Colors for later
my_colors <- c("#0072B2", "#E69F00")

## Some utility functions for cleaning
get_country_name <- function(x){
  read_lines(x, n_max = 1) %>%
    str_extract(".+?,") %>%
    str_remove(",")
}

shorten_name <- function(x){
  str_replace_all(x, " -- ", " ") %>%
    str_replace("The United States of America", "USA") %>%
    snakecase::to_any_case()
}

make_ccode <- function(x){
  str_extract(x, "[:upper:]+((?=\\.))")
}

## Tibble of country naes, codes, and file paths
filenames <- dir(path = here(path),
                 pattern = "*.txt",
                 full.names = TRUE)

countries <- tibble(country = map_chr(filenames, get_country_name),
                    cname = map_chr(country, shorten_name),
                    ccode = map_chr(filenames, make_ccode),
                    path = filenames)

countries

## Ingest the data
mortality <- countries %>%
  mutate(data = map(path,
                    ~ read_table(., skip = 2, na = "."))) %>%
  unnest(cols = c(data)) %>%
  clean_names() %>%
  mutate(age = as.integer(recode(age, "110+" = "110"))) %>%
  select(-path) %>%
  nest(data = c(year:total))

## Subset to flu years / countries
flu <- mortality %>% 
  unnest(cols = c(data)) %>%
  group_by(country) %>%
  filter(min(year) < 1918)

## Dummy dataset sor a "1918" label in one facet panel only
dat_text <- data.frame(
  label = c("1918", rep(NA, 5)),
  agegrp = factor(paste("Age", seq(10, 60, 10))),
  year     = c(1920, rep(NA, 5)),
  female     = c(0.04, rep(NA, 5)), 
  flag = rep(NA, 6)
)

## Make the plot
p0 <- flu %>%
  group_by(country, year) %>%
  filter(year > 1899 & year < 1930, age %in% seq(10, 60, by = 10)) %>%
  mutate(flag = country %in% "Spain", 
         agegrp = paste("Age", age)) %>%
  ggplot(mapping = aes(x = year, y = female, color = flag)) + 
  geom_vline(xintercept = 1918, color = "gray80") + 
  geom_line(mapping = aes(group = country)) 

p1 <- p0 +  geom_text(data = dat_text, 
                mapping = aes(x = year, y = female, label = label), 
                color = "black", 
                show.legend = FALSE, 
                group = 1, 
                size = 3) + 
  scale_color_manual(values = my_colors, 
                     labels = c("Other Countries", "Spain")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
  labs(title = "Female Mortality, Selected Ages and Countries 1900-1929", 
       x = "Year", y = "Female Mortality Rate", color = NULL,
       caption = "@kjhealy / Data: mortality.org") + 
  facet_wrap(~ agegrp, nrow = 2) + 
  theme(legend.position = "top")

p1
  
ggsave("figures/flu.png", p1, width = 12, height = 9)

flu %>%
  group_by(country, year) %>%
  filter(year > 1899 & year < 1930, age == 60, female > 0.07)

mortality %>%
  unnest(cols = c(data)) %>%
  group_by(country) %>%
  summarize(min_yr = min(year), 
            max_yr = max(year)) %>%
  ggplot(mapping = aes(x = reorder(country, min_yr, min), 
                       ymin = min_yr, ymax = max_yr, 
                       y = (max_yr + min_yr)/2)) + 
  geom_errorbar(size = 2) + coord_flip()


