library(tibble)
library(readr)
library(grid)
library(ggplot2)

##
## Configuration
##

rootIndi <- 1100L

numGenerations <- 7L

##
##
##

surname <- read_csv(
  "gen_surnames.csv",
  col_types = cols(
    OXG_ID_PATRONYME = col_double(),
    OXG_SENTENCE = col_character()
  )
)

indi <- read_csv(
  "gen_individuals.csv",
  col_types = cols(
    OXG_ID_INDIVIDU = col_double(),
    OXG_ID_FATHER = col_double(),
    OXG_ID_MOTHER = col_double(),
    OXG_ID_PATRONYME = col_double(),
    OXG_SOSA = col_double(),
    OXG_GIVNAME = col_character(),
    OXG_Sex = col_double(),
    OXG_BIRTH_DATE = col_character(),
    OXG_BIRTH_PLACE = col_character(),
    OXG_ID_BIRTH_CITY = col_character(),
    OXG_ID_BIRTH_ZONE = col_character(),
    OXG_ID_BIRTH_COUNTRY = col_character(),
    OXG_DEATH_DATE = col_character(),
    OXG_DEATH_PLACE = col_character(),
    OXG_ID_DEATH_CITY = col_character(),
    OXG_ID_DEATH_ZONE = col_character(),
    OXG_ID_DEATH_COUNTRY = col_character()
  )
)

indi <- merge(indi, surname)

numIndis <- sum(2^(seq(numGenerations) - 1))

plot <- tibble(
  ID = NA,
  Name = NA,
  DateOfBirth = NA,
  CountryOfBirth = NA,
  DateOfDeath = NA,
  CountryOfDeath = NA,
  Generation = rep(seq(numGenerations), times = 2^(seq(numGenerations) - 1)),
  IndiPerGeneration = rep(2^(seq(numGenerations) - 1), times = 2^(seq(numGenerations) - 1)),
  x = unlist(
    lapply(
      seq(numGenerations),
      function(gen) {
        return(seq(from = 2^(numGenerations - gen), to = 2^numGenerations, by = 2^(numGenerations - gen + 1)))
      }
    )
  ),
  width = 2^numGenerations / IndiPerGeneration
)

for (idx in seq(numIndis)) {
  if (idx == 1) {
    plot[idx, "ID"] <- rootIndi
  }
  
  if(is.na(plot[idx, ]$ID)) {
    stop("plot[idx, ]$ID is NA. idx: ", idx)
  }
  
  if(plot[idx, ]$ID == -1) {
    next
  }
  
  this <- indi[indi$OXG_ID_INDIVIDU == plot[idx, ]$ID, ]
  
  if (nrow(this) == 0) {
    stop("nriw(this) == 0. idx: ", idx)
  }
  
  plot[idx, "Name"] <- paste(this[, c("OXG_GIVNAME","OXG_SENTENCE")], collapse = "\n")
  plot[idx, "DateOfBirth"] <- this$OXG_BIRTH_DATE
  plot[idx, "CountryOfBirth"] <- ifelse(this$OXG_ID_BIRTH_COUNTRY == "?", "Unknown", this$OXG_ID_BIRTH_COUNTRY)
  plot[idx, "DateOfDeath"] <- this$OXG_DEATH_DATE
  plot[idx, "CountryOfDeath"] <- this$OXG_ID_DEATH_COUNTRY
  plot[idx, "Label"] <- trimws(paste(
    plot[idx, "Name"],
    ifelse(is.na(this$OXG_BIRTH_DATE), "", paste("b.", this$OXG_BIRTH_DATE)),
    ifelse(is.na(this$OXG_DEATH_DATE), "", paste("d.", this$OXG_DEATH_DATE)),
    sep = "\n"
  ))
  
  if (idx < (numIndis + 1) / 2) {
    plot[2*idx, "ID"] <- this$OXG_ID_FATHER
    plot[(2*idx)+1, "ID"] <- this$OXG_ID_MOTHER
  }
}

plot[is.na(plot$CountryOfBirth), "CountryOfBirth"] <- "Unknown"

# Define the tree structure that connects the members of each generation

tree <- tibble(
  x = c(
    plot[seq(from = 4, to = numIndis, by = 2), ]$x,
    plot[seq(from = 2L, to = as.integer(numIndis/2)), ]$x,
    plot[seq(from = 4L, to = numIndis), ]$x
  ),
  xend = c(
    plot[seq(from = 5, to = numIndis, by = 2), ]$x,
    plot[seq(from = 2L, to = as.integer(numIndis/2)), ]$x,
    plot[seq(from = 4L, to = numIndis), ]$x
  ),
  y = c(
    plot[seq(from = 4, to = numIndis, by = 2), ]$Generation - 0.5,
    plot[seq(from = 2L, to = as.integer(numIndis/2)), ]$Generation + 0.4,
    plot[seq(from = 4L, to = numIndis), ]$Generation - 0.5
  ),
  yend = c(
    plot[seq(from = 4, to = numIndis, by = 2), ]$Generation -0.5,
    plot[seq(from = 2L, to = as.integer(numIndis/2)), ]$Generation + 0.5,
    plot[seq(from = 4L, to = numIndis), ]$Generation - 0.4
  )
)

pedigree <- ggplot(plot, aes(x = x, y = Generation)) +
  geom_segment(data = tree, aes(x = x, xend = xend, y = y, yend = yend), size = 1, colour = "black", lineend = "round", inherit.aes = FALSE) +
  geom_tile(aes(width = width, fill = CountryOfBirth), colour = "white", size = 1, alpha = 0.6, height = 0.8) +
  geom_tile(data = plot[1L,], aes(width = width), fill = "white", alpha = 1, height = 0.8) +
  geom_tile(data = plot[1L,], aes(width = width, fill = CountryOfBirth), alpha = 0.6, height = 0.8) +
  geom_text(aes(label = Label)) +
  coord_polar() +
  scale_y_discrete(expand = c(0L,0L)) +
  scale_fill_manual(
    name = "Country of Birth",
    values = c(
      Unknown = "grey90",
      Australia = "#cc3300",
      Belgium = "gold",
      China = "red3",
      Ireland = "springgreen3",
      `United Kingdom` = "steelblue4"
    )
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(1,1,1,1), "cm"),
    text = element_text(size = 6),
    plot.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    legend.justification = c("left","bottom"),
    legend.position = c(0,0),
    legend.direction = "vertical",
    legend.key.size = unit(2, "cm"),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 36)
  )

ggsave(filename = "Pedigree.pdf", plot = pedigree, width = 841, height = 1189, units = "mm")




