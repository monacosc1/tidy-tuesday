### Tidy Tuesday: Taylor Albums ###
### 10/17/2023 ###

# load libraries -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggplot2)
library(viridis)
library(showtext)
library(patchwork)
library(camcorder)
library(nrBrand)
library(glue)
library(statebins)
library(ggimage)
library(tayloRswift)
library(ggnewscale)
library(Ra11y)

# load data -----------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-17")
taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

# replicates -----------------------------------------------------------

# first replicate 

keys <- c(1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6, 7)
names(keys) <- c("C", "C", "D", "D", "E", "F", "F", "G", "G", "A", "A", "B")

taylor_keys <- taylor_album_songs |> 
  select(album_release, album_name, track_name, track_number, key, key_name, duration_ms) |> 
  drop_na() |> 
  arrange(album_release) |> 
  mutate(album_number = consecutive_id(album_name)) |> 
  mutate(duration_z = (duration_ms-mean(duration_ms))/sd(duration_ms)) |> 
  mutate(value = case_when(
    duration_z < -.5 ~ "eighth",
    duration_z > 2 ~ "half",
    .default = "quarter"
  )) |> 
  mutate(sh = case_when(
    duration_z < -.5 ~ 19,
    duration_z > 2 ~ 21,
    .default = 19
  )) |> 
  mutate(key_letter = str_sub(key_name, 1, 1),
         key_tag = str_sub(key_name, 2, 2)) |> 
  rowwise() |> 
  mutate(key = keys[key_letter]) |> 
  ungroup() |> 
  mutate(c_bar = case_when(
    n() > 18 ~ .3,
    .default = .2
  ), .by = album_name) |> 
  mutate(word = gsub(" .*", "", track_name)) |> 
  mutate(word = case_when(
    track_number!=1 & word!="I" ~ tolower(word),
    .default = word
  )) |> 
  mutate(word = case_when(
    nchar(word)>8 ~ paste0(str_sub(word, 1, 4), "-\n", str_sub(word, 5, -1)),
    .default = word
  )) |> 
  mutate(stem_y = case_when(
    key > 6 ~ key-7,
    .default = key+4
  )) |> 
  mutate(stem_x = case_when(
    key > 6 ~ -.1,
    .default = .1
  ))

# Create flags and beams
flags_beams <- taylor_keys |> 
  mutate(beam = (value==lead(value) & stem_x==lead(stem_x)) | (value==lag(value) & stem_x==lag(stem_x)))

flags <- flags_beams |> 
  filter(!beam & value=="eighth") |> 
  mutate(yend = case_when(
    stem_x < 0 ~ stem_y-stem_x*30,
    .default = stem_y
  )) |> 
  mutate(stem_y = case_when(
    stem_x < 0 ~ stem_y,
    .default = stem_y-stem_x*30
  )) |> 
  mutate(x = case_when(
    stem_x > 0 ~ track_number+stem_x+.1,
    .default = track_number+stem_x
  )) |> 
  mutate(xend = case_when(
    stem_x < 0 ~ track_number+stem_x+.1,
    .default = track_number+stem_x
  ))

beams <- flags_beams |> 
  mutate(grouping = consecutive_id(beam), .by = album_name) |> 
  filter(all(stem_x>0)|all(stem_x<0), .by = c(album_name, grouping)) |> 
  filter(beam & value=="eighth") |> 
  mutate(no = row_number(), .by = c(album_name, grouping)) |> 
  select(album_name, album_number, track_number, key, stem_x, stem_y, value, no, grouping) |> 
  mutate(xmin = min(track_number)+stem_x,
         xmax = max(track_number)+stem_x,
         ymin = first(stem_y),
         ymax = last(stem_y), .by = c(album_name, grouping)) |> 
  slice(1, .by = c(album_name, grouping))

# Time signature (most are 4/4)
time <- tibble(x=0, y=c(3.5, 7), label=c("4", "4"))

# Correct a flag stem
taylor_keys[3, ]$stem_y <- taylor_keys[3, ]$stem_y-1.5
taylor_keys[11, ]$stem_y <- taylor_keys[11, ]$stem_y+1
taylor_keys[177, ]$stem_y <- taylor_keys[177, ]$stem_y+2.5
taylor_keys[178, ]$stem_y <- taylor_keys[178, ]$stem_y-2
taylor_keys[179, ]$stem_y <- taylor_keys[179, ]$stem_y+1
taylor_keys[183, ]$stem_y <- taylor_keys[183, ]$stem_y+3.5



# Plot data ---------------------------------------------------------------

t <- ggplot() +
  geom_hline(yintercept=seq(1, 10, by=2), color="grey30") +
  geom_text(data=time, aes(x, y, label=label), family="Trattatello", size=5) +
  geom_point(data=taylor_keys, aes(x=track_number, y=key-2, shape=sh)) +
  geom_segment(data=taylor_keys, aes(x=track_number+stem_x, xend=track_number+stem_x, y=key-2, yend=stem_y)) +
  geom_text(data=taylor_keys, aes(x=track_number, y=-4, label=word), size=1.2, family="Trattatello") +
  geom_segment(data=beams, aes(x=xmin-.02, xend=xmax+.02, y=ymin, yend=ymax), linewidth=1.5) +
  geom_curve(data=flags, aes(x=x, xend=xend, y=stem_y, yend=yend), curvature=.2, linewidth=.7) +
  geom_text(data=taylor_keys, aes(x=track_number-.3, y=key-1, label=key_tag), family="Trattatello") +
  geom_segment(data=filter(taylor_keys, key_letter=="C"), aes(x=track_number-c_bar, xend=track_number+c_bar, y=key-2, yend=key-2)) +
  scale_shape_identity() +
  ylim(-5, 12) +
  labs(x="", y="") +
  facet_wrap(~fct_reorder(gsub(" \\(Taylor's Version\\)", "", album_name), album_number), scales="free", ncol=1) +
  theme_void(base_family="Trattatello") +
  theme(plot.background = element_rect(color="transparent", fill="#FAFAEE"),
        plot.margin = margin(5, 5, 5, 5, unit="mm"),
        strip.clip = "off")

ggsave("./2023/2023-10-17/taylor_albums_sheet.jpg", t, width=5.5, height=8, units="in")

sheet <- ggplotGrob(t)

ggplot() +
  ggtext::geom_textbox(aes(x=2.4, y=5, label="Each album as a single line of sheet music.<br><br>The key of each track as a single note.<br><br>Each note's duration as the relative track duration (z scored) across all albums."), 
                       fill="transparent", 
                       box.color="transparent",
                       family="Trattatello", 
                       width = .37,
                       size=12,
                       color="grey10") +
  labs(title="Taylor's Albums (melodized)",
       subtitle="Data: {taylor} via TidyTuesday | Packages: {tidyverse,ggtext} | Visualization: @c_borstell") +
  annotation_custom(grob=sheet, xmin=5, xmax=10, ymin=0, ymax=10) +
  xlim(0, 10) +
  ylim(0, 10) +
  theme_void(base_family="Trattatello") +
  theme(plot.background = element_rect(color="transparent", fill="#FAFAEE"),
        plot.margin = margin(15, 5, 5, 5, unit="mm"),
        plot.title = element_text(hjust=.5, size=rel(6)),
        plot.subtitle = element_text(hjust=.62, vjust=4, size=rel(1.6), color="grey45"))
ggsave("./2023/2023-10-17/taylor_albums.jpg", width=11, height=10, units="in")

# second replicate 

# fonts & colors ----
font1 <- "VCR OSD Mono"
font2 <- "Optima"
palette1 <- c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff")

# load data ----
taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
taylor_all_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
taylor_albums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv')


# data edit ----
## duration to m:ss ----
taylor_album_songs$duration_sec <- round(taylor_album_songs$duration_ms/1000,3)
taylor_album_songs$m <- floor(taylor_album_songs$duration_sec/60)
taylor_album_songs$s <- floor(taylor_album_songs$duration_sec - 60*taylor_album_songs$m)
taylor_album_songs$minsec <- "0:00"
# loop seconds with leading zero
for(i in 1:dim(taylor_album_songs)[1]){
  if(taylor_album_songs$s[i]>=10 & !is.na(taylor_album_songs$s[i])){
    taylor_album_songs$minsec[i] <- paste0(taylor_album_songs$m[i], ":", taylor_album_songs$s[i])
  }
  if(taylor_album_songs$s[i] <10 & !is.na(taylor_album_songs$s[i])){
    taylor_album_songs$minsec[i] <- paste0(taylor_album_songs$m[i], ":0", taylor_album_songs$s[i])
  }
}
# date & album name to help sort by date in y-axis
taylor_album_songs$label <- paste0("*<span style=\"color:#8F3A84;font-size:9px\">",  taylor_album_songs$album_release, 
                                   "</span>*<br><span style=\"color:#000000;font-size:18px\">", taylor_album_songs$album_name, "</span>")

# plot ----
TIDY <- ggplot(subset(taylor_album_songs, bonus_track==FALSE), aes(y=label, x=track_number, fill=duration_sec)) +
  # TILES
  geom_tile(color=palette1[3], linewidth=0.75) +
  scale_fill_viridis_b(breaks = seq(3*60, 5*60, 60), option="plasma", labels =c("3:00", "4:00", "5:00")) +
  # TEXT (min:sec value that is legible on the background)
  geom_text(aes(label = minsec, !!!autocontrast), size = 3.4, family=font1) +
  # LABS
  labs(title = "*\"I mean, you only want two and a half minutes if you can get it, you know, three minutes maximum\"* (Someday by Shirley Ann Lee)",
       subtitle = "**TAYLOR SWIFT'S SONG LENGTHS BY AN ALBUM**",
       y = NULL, 
       x = "Track", 
       fill="Length",
       caption="Graph: **PYYXXO** | Source: **taylor R package** | #TidyTuesday **2023/42**") + 
  # SETTINGS
  scale_x_continuous(breaks=1:max(taylor_album_songs$track_number), expand=c(0.01,-0.01)) +
  coord_equal() +
  theme_void(base_family=font2) +
  theme(axis.text.x     = element_text(size = 10, family = font1, angle = 0, hjust = .5, margin = unit(c(0.5, 0, 0, 0),"mm")),
        axis.title.x    = element_text(size = 10, family = font1, hjust = 0.01, vjust = 1,  margin = unit(c(0.5, 0, 3, 0),"mm")),
        axis.text.y     = element_markdown(size = 12, hjust = 1, angle = 0) ,
        axis.title.y    = element_markdown(size = 12, hjust = 1, angle = 90) ,
        plot.title      = element_markdown(size = 19, hjust = 1,  margin = unit(c(0, 0, 1, 0),"mm")) ,
        plot.subtitle   = element_markdown(size = 18, hjust = 1),
        plot.caption    = element_markdown(colour = "gray20",  size = 9, hjust = 1, vjust = .5, family = font1), 
        plot.background = element_rect(color = palette1[3], fill = palette1[3]),
        legend.text     = element_text(size = 10, hjust = 0, vjust = .5, family=font1), 
        legend.position       = "right", 
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.margin     = unit(c(0, 10, 0, 10),"mm"))

ggsave(TIDY, file="./2023/2023-10-17/TIDY_2023_42.png", width = 24*(2/3), height = 9*(2/3), units = "in") 

# replicate 3

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-17")
taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums


# Load fonts --------------------------------------------------------------

font_add_google("Caveat", "caveat")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "grey10"
text_col <- "grey95"
highlight_col <- tayloRswift::swift_palettes$lover[1]


# Data wrangling ----------------------------------------------------------

album_order <- taylor_album_songs |> 
  select(album_name, album_release) |> 
  distinct() |> 
  arrange(album_release) |> 
  pull(album_name)

plot_data <- taylor_album_songs |>
  group_by(album_name) |>
  summarise(energy = round(100 * mean(energy, na.rm = TRUE) / 5)) |>
  mutate(album_name = factor(album_name, levels = album_order)) |>
  uncount(energy, .remove = FALSE) |>
  group_by(album_name) |>
  mutate(y = row_number()) |>
  ungroup()

bg_data <- expand.grid(
  album_name = album_order,
  y = 1:20
) |>
  mutate(album_name = factor(album_name, levels = album_order)) |>
  as_tibble()

image_data <- plot_data |>
  select(album_name) |>
  distinct() |>
  mutate(
    img = str_replace(album_name, " \\s*\\([^\\)]+\\)", ""),
    img = str_to_lower(str_replace(img, " ", "_")),
    img = paste0(img, ".png"),
    img = file.path("2023", "2023-10-17", img)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-10-17", "recording"),
  device = "png",
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

body_font <- "caveat"

# social <- nrBrand::social_caption(
#   bg_colour = bg_col,
#   icon_colour = highlight_col,
#   font_colour = text_col,
#   font_family = body_font
# )
st <- "<span style='font-size: 44pt;'>The average energy, as defined by Spotify, for each of Taylor Swift's albums.</span>"
cap <- paste0(
  st, "<br>**Data**: {taylor}<br>"
)


# Plot --------------------------------------------------------------------

# Adapted from: https://stackoverflow.com/questions/64355877/round-corners-in-ggplots-geom-tile-possible
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

GeomRtile <- ggproto("GeomRtile",
                     statebins:::GeomRrect,
                     extra_params = c("na.rm"),
                     setup_data = function(data, params) {
                       data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                       data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)
                       
                       transform(data,
                                 xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                 ymin = y - height / 2, ymax = y + height / 2, height = NULL
                       )
                     },
                     default_aes = aes(
                       fill = "grey20", colour = NA, size = 0.1, linetype = 1,
                       alpha = NA, width = NA, height = NA
                     ),
                     required_aes = c("x", "y"),
                     non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                     draw_key = draw_key_polygon
)

geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"),
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      ...
    )
  )
}

ggplot() +
  geom_rtile(
    data = bg_data,
    mapping = aes(x = album_name, y = y, fill = y),
    alpha = 0.2,
    width = 0.95,
    height = 0.85
  ) +
  scale_fill_gradientn(colours = tayloRswift::swift_palettes$lover[4:1]) +
  new_scale_fill() +
  geom_rtile(
    data = plot_data,
    mapping = aes(x = album_name, y = y, fill = y),
    width = 0.95,
    height = 0.85
  ) +
  geom_rtile(
    data = plot_data,
    mapping = aes(x = album_name, y = -y, fill = y),
    alpha = 0.4,
    width = 0.95,
    height = 0.85
  ) +
  scale_fill_gradientn(colours = tayloRswift::swift_palettes$lover[4:1],
                       limits = c(1, 14)) +
  geom_image(
    data = image_data,
    mapping = aes(x = album_name, y = 24, image = img),
    asp = 1,
    size = 0.11
  ) +
  labs(caption = cap) +
  theme_void(base_size = 38) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-10-17", paste0("20231017", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

# fourth replicate

tuesdata <- tidytuesdayR::tt_load("2023-10-17")
taylor_album_songs <- tuesdata$taylor_album_songs

# Frequency table
freq_table <- taylor %>% 
  group_by(genre1) %>%           # group by the genre1
  summarize(Abs_freq=n()) %>%   # absolute frequencies
  mutate(Rel_freq=round(Abs_freq/sum(Abs_freq),digits=4), # relative frequencies
         Perc_freq=Rel_freq*100) %>% # percentages 
  mutate(Lab_text=paste0(Perc_freq,"%")) # paste0 collates strings, thus Lab_text is a new chr variable

# Custom palette of colors
my_palette = c("#FBD35D", "#75DBB2", "#FF6BAE")

# Donut chart
library(RColorBrewer)
freq_table %>% 
  ggplot(aes(x=1, y=Perc_freq,fill=as.factor(genre1))) +
  geom_bar(stat="identity") + # stacked percentage bar chart using stat="identity" so geom_bar does not compute frequencies but uses the data table value as they are
  geom_text(aes(label = Lab_text),position = position_stack(vjust = 0.5)) +
  xlim(c(-0.5,1.5))+ # donut chart
  coord_polar(theta="y") + 
  labs(fill = "Genre", title = "Taylor Swift's discography", subtitle = "by genres") + 
  scale_fill_manual(values = my_palette) +
  theme_void()