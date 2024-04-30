pacman::p_load(gganimate, tidyverse, here)

uruguay <- read_csv(here("data", "estimates_.csv")) %>%
  filter(name == "Uruguay")

# Add duplicated rows for the t_es == 0 to create a pause effect
pause_length <- 10  # Number of duplicates to create the pause

uruguay_pause <- uruguay %>%
  slice(rep(which(t_es == 0), each = pause_length)) %>%
  bind_rows(uruguay) %>%
  arrange(t_es, desc(gender))  # Ensure the data is in the correct order

p <- ggplot(uruguay_pause, aes(t_es, estimate, group = gender, color = gender)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_line(size = 1.5) +  # Adjust size as needed for line thickness
  scale_color_manual(values = c("#D90D0D", "#6A6654")) +
  theme_void() +
  theme(legend.position = "none") +
  labs(x = "",
       y = "",
       color = "") +
  annotate("text", x = -4,  y = .01,   label = "Varones", fontface = "bold", color = "#6A6654", vjust = -0.5) +
  annotate("text", x = -4,  y = -.03,  label = "Mujeres", fontface = "bold", color = "#D90D0D", vjust = -0.5) +
  annotate("text", x = -.5, y = -.31,  label = "-31%",    fontface = "bold", color = "#D90D0D", vjust = -0.5) +
  annotate("text", x = 10,  y = -.38,  label = "-39%",    fontface = "bold", color = "#D90D0D", vjust = -0.5) +
  theme(panel.background = element_rect(fill = "#F5F4EE"),
        plot.background  = element_rect(fill = "#F5F4EE", color = NA))

# Animate the plot
(anim <- p +
  transition_reveal(along = t_es) +
  enter_fade() + exit_fade() +
  ease_aes('linear') +
  shadow_mark())

# Save the animation with the simulated pause
anim_save("animated_plot.gif", animation = anim, end_pause = 10, nframes = 150, fps = 25, width = 480, height = 480)
