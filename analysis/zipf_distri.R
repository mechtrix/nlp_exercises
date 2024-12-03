library(ggh4x)
library(zipfR)
library(purrr)
library(tidyverse)
library(here)


N <- 20

zipf_sim <- expand_grid(
  s = c(0.5,1,1.5,2),
  max_Rank = N
)

zipf_sim <- zipf_sim |> 
  mutate(
    probs = 
      # list(
      map2(
        .x = s, 
        .y = max_Rank,
        \(x,y) {
        zipf_probs = (1 / (1:y)^x) / sum(1 / (1:y)^x) 
        out <- data.frame(Rank = 1:y, Probability = zipf_probs)
        }
        )
      # )
  ) |> 
  unnest(
    cols = "probs"
  )


zipf_sim |> 
  ggplot(
    aes(
      x = Rank,
      y = Probability,
      color = as.factor(s)
    )
  )+
  geom_point()+
  geom_line(
    size = 1
  )+
  labs(
    title = "",
    x = "Rang",
    y = "Wahrscheinlichkeit",
    color = "Formparameter"
  ) +
  scale_x_continuous(
    breaks = seq(1,N)
  )+
  scale_y_continuous(
    breaks = seq(0,1,0.1),
    labels = scales::percent
  )+
  scale_color_brewer(
    palette = "Dark2"
  )+
  theme_minimal(
    base_size = 15
  )+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  )

ggsave(filename = here("img","zipf_shape.png"),width = 8, height = 6)


N <- 20   # Total number of elements
s <- 1.5   # Shape parameter
# 
# # Generate Zipf distribution probabilities
zipf_probs <- (1 / (1:N)^s) / sum(1 / (1:N)^s)
zipf_data <- data.frame(Rank = 1:N, Probability = zipf_probs)

zipf_data |> 
  ggplot(
    aes(
      x = Rank, 
      y = Probability)
    ) +
  geom_line(
    color = "steelblue", 
    size = 1) +
  labs(
    title = "",
    x = "Rang",
    y = "Wahrscheinlichkeit"
    ) +
  scale_x_continuous(
    breaks = seq(1,N)
  )+
  scale_y_continuous(
    breaks = seq(0,0.5,0.05),
    labels = scales::percent
  )+
  theme_minimal(
    base_size = 15
  )+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(filename = here("img","zipf_single.png"),width = 8, height = 6)





