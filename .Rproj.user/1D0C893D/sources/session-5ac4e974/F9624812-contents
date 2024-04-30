pacman::p_load(gganimate, tidyverse, ggtext, here)

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
         caption = "Fuente: elaboración propia en base a The Child Penalty Atlas, por H. Kleven et al., 2023.
                    \nPaula Pereda Suárez | @paubgood") +
    geom_text(aes(x = -4, y = .0100, label = "Varones"), fontface = "bold", color = "#6A6654", vjust = -.5, family = "Verdana") +
    geom_text(aes(x = -4, y = -.05, label = "Mujeres"), fontface = "bold", color = "#D90D0D", vjust = -.5, family = "Verdana") +
    geom_text(aes(x = -.65, y = -.31, label = "-31%"), fontface = "bold", color = "#D90D0D", vjust = -.5, family = "Verdana") +
    geom_text(aes(x = 10, y = -.38, label = "-39%"), fontface = "bold", color = "#D90D0D", vjust = -.5, family = "Verdana") +
    annotate("text", x = -4, y = -.39,  label = "5 años antes", fontface = "bold", color = "black",   vjust = -.5) +
    annotate("text", x = 3.2, y = -.39,  label = "10 años después", fontface = "bold", color = "black",   vjust = -.5) +
    theme(text = element_text(family = "Verdana"),
          plot.title = element_text(face = "bold"),
          plot.caption = element_markdown(hjust = 0),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          plot.background  = element_rect(fill  = "#F5F4EE",
                                          color = "#F5F4EE")))

# Save static plot
ggsave(here("plots", "uruguay.png"), dpi = 300, width = 7, height = 12)

# Create and save the animated plot
anim <- p +
  transition_reveal(along = t_es) +
  enter_fade() + exit_fade() +
  ease_aes('linear') +
  shadow_mark()

# # Calculate the dimensions for the GIF that maintain the same aspect ratio and improve resolution
# gif_width <- 960  # Twice the pixel width for better detail
# gif_height <- 560  # Calculated to maintain the aspect ratio

# Calculate the dimensions for the GIF that maintain the same aspect ratio and improve resolution
gif_width <- 1152  # Twice the pixel width for better detail
gif_height <- 672  # Calculated to maintain the aspect ratio

anim_save(here("plots", "motherhood_penalties_uy.gif"), animation = anim, end_pause = 10, nframes = 150, fps = 25, width = gif_width, height = gif_height)
