pacman::p_load(gganimate, tidyverse, here)

uruguay <- read_csv(here("data", "estimates_.csv")) |>
  filter(name == "Uruguay")

(p <- uruguay |>
  mutate(pause = if_else(t_es == 0, 2, 1)) |> # set pause for 10 at zero
  uncount(pause) |> # repeat pause rows
  mutate(reveal = row_number(), .by = c(name, gender)) |> # set the reveal sequence
  ggplot(aes(t_es, estimate, group = gender, color = gender)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_line(linewidth = 1.5) + # changed from size (deprecated)
  scale_color_manual(values = c("#D90D0D", "#6A6654")) +
  theme_void() +
  theme(legend.position = "none") +
  labs(x = "",
       y = "",
       color = "") +
  annotate("text", x = -4,  y = .1,   label = "Varones", fontface = "bold",  color = "#6A6654", vjust = -.5) +
  annotate("text", x = -4,  y = -.3,  label = "Mujeres", fontface = "bold",  color = "#D90D0D", vjust = -.5) +
  annotate("text", x = -.5, y = -.31,  label = "-31%",    fontface = "bold", color = "#D90D0D", vjust = -.5) +
  annotate("text", x = 10,  y = -.38,  label = "-39%",    fontface = "bold", color = "#D90D0D", vjust = -.5) +
  theme(panel.background = element_rect(fill  = "#F5F4EE"),
        plot.background  = element_rect(fill  = "#F5F4EE",
                                        color = "#F5F4EE")))

p +
  transition_reveal(reveal) + # use the reveal sequence
  enter_fade() +
  exit_fade() +
  ease_aes('linear') +
  shadow_mark()

# Save the animation with the simulated pause
anim_save("animated_plot.gif", animation = anim, nframes = 200, fps = 10, width = 480, height = 480)
