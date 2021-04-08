# Pixelated Geological Map
# Data from Rob Sansom
# 08/04/2021

library(tidyverse)
library(ggplot2)
library(extrafont)

# Read in csv of x y coordinates with geology as id
data <- read.csv("data/xy_geology_dat.csv")

# Clean up the data a bit
data_xy <- data %>%
  filter(!is.na(id)) %>%
  mutate(id = paste0("unit_",id)) %>%
  mutate(id = recode(id,
                     "unit_k" = "unit_K",
                     "unit_d" = "unit_D",
                     "unit_m1" = "unit_M1",
                     "unit_m2" = "unit_M2",
                     "unit_i1" = "unit_I1",
                     "unit_i2" = "unit_I2")) %>%
  mutate(id = factor(id, levels = c("unit_Z","unit_K","unit_J","unit_T","unit_P","unit_C","unit_D","unit_S","unit_O","unit_A","unit_N" ,"unit_M1" ,"unit_M2" ,"unit_I1" ,"unit_I2")))

# Set some plotting factors
plot_title <- data.frame(x=-5,y=54,label = "Pixelated Geological \nMap of Great Britain\nand Ireland")
plot_caption <- data.frame(x=65,y=-3,label = "Visualisation: @HannahMBuckland\nData: @Sansom_Rob")

# Geological unit names
scale_labs <- c(unit_Z="Quaternary, Neogene \n and Paleogene",
                unit_K ="Cretaceous",
                unit_J ="Jurassic",
                unit_T ="Triassic",
                unit_P ="Permian",
                unit_C ="Carboniferous",
                unit_D ="Devonian",
                unit_S ="Silurian",
                unit_O ="Ordovician",
                unit_A ="Cambrian",
                unit_N ="Neoproterozoic",
                unit_M1 ="Metamorphic \n(Lower Palaeozoic and Upper Proterozoic)",
                unit_M2 ="Metamorphic \n(Lower Proterozoic and Archaen",
                unit_I1 ="Igneous Intrusive",
                unit_I2 ="Igneous Volcanic")

# Geological unit colours (grabbed using colour dropper from BGS map)
colourscale <- c(unit_Z="#fef99d",
                 unit_K = "#a3c24b",
                 unit_J = "#6fb7d0",
                 unit_T = "#e189a3",
                 unit_P = "#fae6ba",
                 unit_C = "#cecece",
                 unit_D = "#d8863a",
                 unit_S = "#97c7ca",
                 unit_O = "#9cc8ad",
                 unit_A = "#c1cbd3",
                 unit_N = "#f5d251",
                 unit_M1 = "#d3af40",
                 unit_M2 = "#9c74a9",
                 unit_I1 = "#df3f2f",
                 unit_I2 = "#a00053")

# Plot as x-y plot

pixplot <- ggplot() +
  geom_point(data = data_xy,
             mapping = aes(x=x,
                           y=y,
                           fill=id), shape = 22, size = 4.5) +
  scale_fill_manual(values = colourscale, labels = scale_labs) +
  scale_y_continuous(limits = c(-5,55))+
  scale_x_continuous(limits = c(-10,65))+
  geom_text(data = plot_title,
            mapping = aes(x=x,y=y,label=label), 
            colour = "white",
            hjust = 0, vjust = 1, size = 12, family = "Oswald") +
  geom_text(data = plot_caption,
            mapping = aes(x=x,y=y,label=label), 
            colour = "white",
            hjust = 1, vjust = 1, size = 4, family = "Courier") +
  coord_fixed(ratio=1) +
  theme(
    panel.background = element_rect(
      fill = "black", 
      colour = "black"),
    text = element_text(colour = "white"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "white", size = 12, family = "Oswald"),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      colour = NA,
      fill = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.75,0.45),
    plot.margin = margin(0,0,0,0,"cm"))


final_plot <- pixplot + guides(fill=guide_legend(
  keywidth=2,
  keyheight=2,
  default.unit="lines")
) 


ggsave("plots/geology_pixels.png", final_plot, width = 12, height = 12)

