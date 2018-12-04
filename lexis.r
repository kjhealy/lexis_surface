library(tidyverse)
library(janitor)

library(showtext)
showtext_auto()

library(myriad)
import_myriad()

theme_set(theme_myriad_semi())

path <- "data/Mx_1x1/"

## Make a "figures" subdirectory in the working directory if one
## doesn't already exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)


###--------------------------------------------------
### Data from mortality.org
###--------------------------------------------------

britain <- read_table(paste0(path, "GBRTENW.Mx_1x1.txt"), skip = 2, na = ".") %>% clean_names()

britain$age <- as.integer(recode(britain$age, "110+" = "110"))

britain <- britain %>% mutate(ratio = male / female,
                              pct_diff = ((male - female) / (male + female))*100,
                              bin_ratio = ntile(ratio, 100))



france <- read_table(paste0(path, "FRATNP.Mx_1x1.txt"), skip = 2, na = ".") %>% clean_names()
france$age <- as.integer(recode(france$age, "110+" = "110"))

france <- france %>% mutate(ratio = male / female,
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



p <- ggplot(subset(britain, age < 101), aes(x = year, y = age, fill = ntile(female)))
p_out <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1845, 2015, by = 15)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    ylim(c(0, 100)) +
    labs(x = "Year", y = "Age", fill = "Female Death Rate Percentile",
         title = "Female Mortality 1841-2016",
         subtitle = "Binned by percentile",
#         subtitle = "After Schöley & Willekens (2017), 'Visualizing compositional data on the Lexis surface'",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out
ggsave("figures/britain_women.png", p_out, height = 9, width = 10)
ggsave("figures/britain_women.pdf", p_out, height = 9, width = 10)



p <- ggplot(subset(britain, age < 101), aes(x = year, y = age, fill = ntile(male, 100)))
p_out <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1845, 2015, by = 15)) +
    ylim(c(0, 100)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    labs(x = "Year", y = "Age", fill = "Male Death Rate Percentile",
         title = "Male Mortality Rates in England and Wales, 1841-2016",
         subtitle = "Binned by percentile",
         ## subtitle = "After Schöley & Willekens (2017), 'Visualizing compositional data on the Lexis surface'",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out
ggsave("figures/britain_men.png", p_out, height = 9, width = 10)
ggsave("figures/britain_men.pdf", p_out, height = 9, width = 10)


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
#         subtitle = "After Schöley & Willekens (2017), 'Visualizing compositional data on the Lexis surface'",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out
ggsave("figures/france_women.png", p_out, height = 8, width = 12)
ggsave("figures/france_women.pdf", p_out, height = 8, width = 12)



p <- ggplot(subset(france, age < 101), aes(x = year, y = age, fill = ntile(male, 100)))
p_out <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1820, 2015, by = 15)) +
    ylim(c(0, 100)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    labs(x = "Year", y = "Age", fill = "Male Death Rate Percentile",
         title = "Male Mortality Rates in France, 1816-2016",
         subtitle = "Binned by percentile",
         ## subtitle = "After Schöley & Willekens (2017), 'Visualizing compositional data on the Lexis surface'",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out
ggsave("figures/france_men.png", p_out, height = 8, width = 12)
ggsave("figures/france_men.pdf", p_out, height = 8, width = 12)
