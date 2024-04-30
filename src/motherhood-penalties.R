pacman::p_load(gganimate, tidyverse, here)

# Motherhood Penalties | Uruguay

uruguay <- read_csv(here("data", "estimates_.csv")) |>
  filter(name == "Uruguay")

(p <- uruguay |>
  mutate(pause = if_else(t_es == 0, 2, 1)) |> # set pause for 10 at zero
  uncount(pause) |> # repeat pause rows
  mutate(reveal = row_number(), .by = c(name, gender)) |> # set the reveal sequence
  ggplot(aes(t_es, estimate, group = gender, color = gender)) +
  geom_vline(xintercept = -.5, linetype = "solid", color = "black", linewidth = .2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("#D90D0D", "#6A6654")) +
  theme_minimal(base_family = "Verdana",
                base_size = 12) +
  labs(x = "",
       y = "",
       color = "",
       title = "Cambio en el empleo después de tener el primer hijo/a, %",
       subtitle = "Respecto a dos años antes del nacimiento",
       caption = "Fuente: elaboración propia en base a The Child Penalty Atlas, por H. Kleven et al., 2023.\nPaula Pereda Suárez | @paubgood") +
  geom_text(aes(x = -4, y = 0.0100, label = "Varones"), fontface = "bold", color = "#6A6654", vjust = -.5, family = "Verdana") +
  geom_text(aes(x = -4, y = -.03, label = "Mujeres"), fontface = "bold", color = "#D90D0D", vjust = -.5, family = "Verdana") +
  geom_text(aes(x = -.5, y = -.31, label = "-31%"), fontface = "bold", color = "#D90D0D", vjust = -.5, family = "Verdana") +
  geom_text(aes(x = 10, y = -.38, label = "-39%"), fontface = "bold", color = "#D90D0D", vjust = -.5, family = "Verdana") +
  annotate("text", x = -4, y = -.39,  label = "5 años antes", fontface = "bold", color = "black",   vjust = -.5) +
  annotate("text", x = 3.5, y = -.39,  label = "10 años después", fontface = "bold", color = "black",   vjust = -.5) +
  theme(text = element_text(family = "Verdana"),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        plot.background  = element_rect(fill  = "#F5F4EE",
                                        color = "#F5F4EE")))

# Save static plot
ggsave(here("plots", "uruguay.png"), dpi = 300, width = 7, height = 12)

comparison <- read_csv(here("data", "estimates_.csv")) |>
  filter(name %in% c("Africa", "Asia", "Europe", "Latin America", "North America", "Uruguay") & gender == "female") |>
  mutate(name = case_when(
    name == "Africa"        ~ "África",
    name == "Asia"          ~ "Asia",
    name == "Europe"        ~ "Europa",
    name == "Latin America" ~ "Latinoamérica",
    name == "North America" ~ "Norteamérica",
    name == "Uruguay"       ~ "Uruguay"))

comparison |>
  ggplot(aes(t_es, estimate, group = name, color = name)) +
  geom_vline(xintercept = -.5, linetype = "solid", color = "black", linewidth = .2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("#ad0a0a","#D90D0D", "#e03d3d", "#e86d6d", "#ef9e9e", "#f7cece")) +
  theme_minimal(base_family = "Verdana",
                base_size = 16) +
  labs(x = "",
       y = "",
       color = "",
       title = "Cambio en el empleo después de tener el primer hijo/a, %",
       subtitle = "Respecto a dos años antes del nacimiento",
       caption = "Fuente: elaboración propia en base a The Child Penalty Atlas, por H. Kleven et al., 2023.\nPaula Pereda Suárez | @paubgood") +
  geom_text(aes(x = 8, y = -.41,  label = "Uruguay"), fontface = "bold", color = "#f7cece", vjust = -.5, family = "Verdana") +
  geom_text(aes(x = 8, y = -.22, label = "Norteamérica"), fontface = "bold", color = "#ef9e9e", vjust = -.5, family = "Verdana") +
  geom_text(aes(x = 8, y = -.37, label = "Latinoamérica"), fontface = "bold", color = "#e86d6d", vjust = -.5, family = "Verdana") +
  geom_text(aes(x = 8, y = -.28, label = "Europa"), fontface = "bold", color = "#e03d3d", vjust = -.5, family = "Verdana") +
  geom_text(aes(x = 8, y = -.13, label = "Asia"), fontface = "bold", color = "#D90D0D", vjust = -.5, family = "Verdana") +
  geom_text(aes(x = 8, y = -.09, label = "África"), fontface = "bold", color = "#ad0a0a", vjust = -.5, family = "Verdana") +
  annotate("text", x = -4, y = -.39,  label = "5 años antes", fontface = "bold", color = "black",   vjust = -.5) +
  annotate("text", x = 3.5, y = -.39,  label = "10 años después", fontface = "bold", color = "black",   vjust = -.5) +
  theme(text = element_text(family = "Verdana"),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        plot.background  = element_rect(fill  = "#F5F4EE",
                                        color = "#F5F4EE"))

ggsave(here("plots", "comparison.png"), dpi = 300, width = 12, height = 7)
