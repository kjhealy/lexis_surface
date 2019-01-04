library(tidyverse)
library(janitor)

library(showtext)
showtext_auto()

library(myriad)
import_myriad_semi()

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
                              deciles = cut(ratio, breaks = quantile(ratio, probs = seq(0, 1, 0.1), na.rm = TRUE)),
                              pct_diff = ((male - female) / (male + female))*100,
                              bin_ratio = ntile(ratio, 100))



france <- read_table(paste0(path, "FRATNP.Mx_1x1.txt"), skip = 2, na = ".") %>% clean_names()
france$age <- as.integer(recode(france$age, "110+" = "110"))

france <- france %>% mutate(ratio = male / female,
                            deciles = cut(ratio, breaks = quantile(ratio, probs = seq(0, 1, 0.1), na.rm = TRUE)),
                              pct_diff = ((male - female) / (male + female))*100,
                              bin_ratio = ntile(ratio, 100))

ireland <- read_table(paste0(path, "IRL.Mx_1x1.txt"), skip = 2, na = ".") %>% clean_names()
ireland$age <- as.integer(recode(ireland$age, "110+" = "110"))

ireland <- ireland %>% mutate(ratio = male / female,
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


p <- ggplot(subset(france, age < 101), aes(x = year, y = age, fill = ntile(female, 100)))
p_out <- p + geom_tile() +
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
ggsave("figures/france_women_tile.png", p_out, height = 8, width = 12)
ggsave("figures/france_women_tile.pdf", p_out, height = 8, width = 12)




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


###--------------------------------------------------
### Ireland
###--------------------------------------------------


p <- ggplot(subset(ireland, age < 101), aes(x = year, y = age, fill = ntile(female, 100)))
p_out <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1950, 2015, by = 10)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    ylim(c(0, 100)) +
    labs(x = "Year", y = "Age", fill = "Female Death Rate Percentile",
         title = "Female Mortality Rates in Ireland, 1950-2016",
         subtitle = "Binned by percentile",
#         subtitle = "After Schöley & Willekens (2017), 'Visualizing compositional data on the Lexis surface'",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out
ggsave("figures/ireland_women.png", p_out, height = 8, width = 9)
ggsave("figures/ireland_women.pdf", p_out, height = 8, width = 9)


p <- ggplot(subset(ireland, age < 101), aes(x = year, y = age, fill = ntile(female, 100)))
p_out <- p + geom_tile() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1950, 2015, by = 10)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    ylim(c(0, 100)) +
    labs(x = "Year", y = "Age", fill = "Female Death Rate Percentile",
         title = "Female Mortality Rates in Ireland, 1950-2016",
         subtitle = "Binned by percentile",
#         subtitle = "After Schöley & Willekens (2017), 'Visualizing compositional data on the Lexis surface'",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out
ggsave("figures/ireland_women_tile.png", p_out, height = 8, width = 12)
ggsave("figures/ireland_women_tile.pdf", p_out, height = 8, width = 12)




p <- ggplot(subset(ireland, age < 101), aes(x = year, y = age, fill = ntile(male, 100)))
p_out <- p + geom_raster() +
    scale_fill_viridis_c(option = "A", direction = -1) +
    scale_x_continuous(breaks = seq(1950, 2015, by = 10)) +
    ylim(c(0, 100)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    labs(x = "Year", y = "Age", fill = "Male Death Rate Percentile",
         title = "Male Mortality Rates in Ireland, 1950-2016",
         subtitle = "Binned by percentile",
         ## subtitle = "After Schöley & Willekens (2017), 'Visualizing compositional data on the Lexis surface'",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out
ggsave("figures/ireland_men.png", p_out, height = 8, width = 12)
ggsave("figures/ireland_men.pdf", p_out, height = 8, width = 12)





p <- ggplot(subset(ireland, age < 101), aes(x = year, y = age, fill = pct_diff))

p_out <- p + geom_raster() +
    scale_fill_gradient2() +
    scale_x_continuous(breaks = seq(1950, 2015, by = 10)) +
    ylim(c(0, 100)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
    labs(x = "Year", y = "Age", fill = "Male Death Rate Percentile",
         title = "Male Mortality Rates in Ireland, 1950-2016",
         subtitle = "Binned by percentile",
         ## subtitle = "After Schöley & Willekens (2017), 'Visualizing compositional data on the Lexis surface'",
         caption = "@kjhealy / http://socviz.co. Data: Human Mortality Database.") +
    theme(legend.position = "top",
          legend.title = element_text(size = 8))

p_out
ggsave("figures/ireland_comp.png", p_out, height = 8, width = 12)
ggsave("figures/ireland_comp.pdf", p_out, height = 8, width = 12)




###--------------------------------------------------
### Relative Mortality
###--------------------------------------------------


relpal_m <- RColorBrewer::brewer.pal(7, "Blues")
relpal_f <- RColorBrewer::brewer.pal(4, "Oranges")

relmort_pal <- c(rev(relpal_f)[1:3], "#FFFFFF", relpal_m)



france2 <- france %>% filter(ratio != Inf, ratio != 0) %>%
    mutate(ratio = male / female,
           deciles = Hmisc::cut2(ratio, g = 10, digits = 2),
           manual_cut = Hmisc::cut2(ratio, cuts = c(0.25, 0.5, 0.75, 0.98, 1.02, 1.25, 1.5, 1.75, 2, 5, 10, 30), digits = 2),
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


p <- ggplot(subset(france2, age < 101), aes(x = year, y = age, fill = manual_cut))
p_out <- p + geom_tile() +
    scale_fill_manual(values = relmort_pal, breaks = levels(france2$manual_cut), labels = levs) +
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

htmlwidgets::saveWidget(as_widget(p), "html/males.html")

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
