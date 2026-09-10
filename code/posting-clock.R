# ---------------------------------------------------------------------------
# "Who is awake at 3am?"  --  EC242, Week 1
#
# Inspired by the Wall Street Journal's analysis of Elon Musk's posting habits:
#   https://graphics.wsj.com/elon-musk-twitter-habit-analysis/
#
# The WSJ used Twitter data. That door is now shut -- X's API returns 401
# without a paid plan -- so this uses Bluesky, whose AT Protocol serves public
# posts to anyone who asks, no key and no account required.
#
# Nothing here is on the exam. It is here because you asked (or will ask) how
# the figures in the news get made, and the answer is: like this, in about a
# hundred lines, by someone who fussed over the colors for an hour.
#
# You need two packages beyond the tidyverse:
#   install.packages(c("patchwork", "ragg"))
# ---------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)   # stitches several ggplots into one composition
library(ragg)        # a better PNG device: real antialiasing, real fonts

# --- 1. The data -----------------------------------------------------------
# Collected from https://public.api.bsky.app/xrpc/app.bsky.feed.getAuthorFeed
# by paging with a cursor, 100 posts at a time, dropping reposts. The saved
# copy lives with the course data so this figure is reproducible even after
# these accounts keep posting:

posts_raw <- read_csv("https://ec242.netlify.app/data/bluesky_posting_2026.csv")

# --- 2. Whose clock is whose -----------------------------------------------
# A timestamp is meaningless until you know where the person was standing.
# Every post arrives in UTC; "3am" only means something in local time.

tz_of   <- c("apnews.com"              = "America/New_York",
             "nytimes.com"             = "America/New_York",
             "georgetakei.bsky.social" = "America/Los_Angeles")

name_of <- c("apnews.com"              = "THE ASSOCIATED PRESS",
             "nytimes.com"             = "THE NEW YORK TIMES",
             "georgetakei.bsky.social" = "GEORGE TAKEI")

# A POSIXct column carries exactly one time zone for the whole vector, so you
# cannot convert three accounts at once. Split, convert, stack back up.
posts <- posts_raw %>%
  mutate(utc = ymd_hms(created_at, tz = "UTC")) %>%
  group_split(account) %>%
  map_dfr(~ mutate(.x, local = with_tz(utc, tz_of[[.x$account[1]]]))) %>%
  mutate(day = as.Date(local),
         tod = hour(local) + minute(local) / 60,   # time of day, in hours
         who = factor(name_of[account], levels = unname(name_of))) %>%
  filter(day >= as.Date("2026-04-01"))   # the window all three accounts cover

X0 <- min(posts$day); X1 <- max(posts$day)

# --- 3. The one summary that stays on the chart -----------------------------
# The little bar profile at the right edge: the same posts, counted by hour.
# Everything else the chart used to say in words is said in the surrounding
# text instead. A figure that needs three sentences of caption is a figure
# that has not finished being a figure.

prof <- posts %>%
  mutate(h = floor(tod)) %>%
  count(who, h) %>%
  group_by(who) %>%
  mutate(share = n / sum(n), night = h < 6) %>%
  ungroup()

# --- 4. A palette, chosen on purpose ---------------------------------------
# Warm paper, one deep ink, one accent. Three colors is a design; six is a
# ransom note. The accent does exactly one job -- marking the small hours.

PAPER <- "#FAF7F1"   # paper
INK   <- "#15181C"   # headline
DOT   <- "#123C4A"   # one post
NIGHT <- "#EFE9DD"   # the midnight-to-6am band
RUST  <- "#A8431F"   # the accent, used once
DECK  <- "#5C574C"; MUTE <- "#8C8677"; RULE <- "#DCD5C7"
FONT  <- "Avenir"    # swap for one you have: "Helvetica Neue", "Georgia", ...
LEFT  <- 23.5        # inset that lines the headline up with the panels

# --- 5. The panels ---------------------------------------------------------
# One row per account: a scatter of every post, plus its hourly profile.

scatter <- function(w, last) {
  ggplot(filter(posts, who == w)) +
    annotate("rect", xmin = X0 - 1, xmax = X1 + 1, ymin = 0, ymax = 6,
             fill = NIGHT, colour = NA) +
    geom_point(aes(day, tod), colour = DOT, alpha = .34, size = .72, stroke = 0) +
    scale_y_continuous(limits = c(0, 24), breaks = c(0, 6, 12, 18, 24),
                       labels = c("12am", "6am", "noon", "6pm", "12am"),
                       expand = c(0, 0)) +
    scale_x_date(limits = c(X0 - 1, X1 + 1), date_breaks = "1 month",
                 date_labels = "%B", expand = c(0, 0)) +
    labs(title = as.character(w)) +
    theme_minimal(base_family = FONT) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = RULE, linewidth = .25),
          axis.title       = element_blank(),
          axis.text.y      = element_text(size = 7.5, colour = MUTE),
          axis.text.x      = element_blank(),
          plot.title    = element_text(face = "bold", size = 11, colour = INK,
                                       margin = margin(b = 7)),
          plot.margin   = margin(0, 0, if (last) 0 else 26, 0),
          plot.background  = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA))
}

PMAX <- max(prof$share) * 1.10   # one shared scale, so the three are comparable

profile <- function(w, last) {
  ggplot(filter(prof, who == w)) +
    annotate("segment", x = 0, xend = 0, y = 0, yend = 24,
             colour = RULE, linewidth = .4) +
    geom_col(aes(share, h + .5, fill = night), orientation = "y", width = .68) +
    scale_fill_manual(values = c(`FALSE` = DOT, `TRUE` = RUST), guide = "none") +
    scale_y_continuous(limits = c(0, 24), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, PMAX), expand = c(0, 0)) +
    labs(title = " ") +                     # blank, to keep the rows aligned
    theme_minimal(base_family = FONT) +
    theme(panel.grid  = element_blank(),
          axis.title  = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          plot.title  = element_text(face = "bold", size = 11, margin = margin(b = 7)),
          plot.margin = margin(0, 0, if (last) 0 else 26, 10),
          plot.background = element_rect(fill = NA, colour = NA))
}

# --- 6. Assemble -----------------------------------------------------------
accounts <- levels(posts$who)
rows <- map(seq_along(accounts), function(i) {
  last <- i == length(accounts)
  (scatter(accounts[i], last) | profile(accounts[i], last)) +
    plot_layout(widths = c(1, .10))
})

figure <- (rows[[1]] / rows[[2]] / rows[[3]]) +
  plot_annotation(
    title = "Who is awake at 3am?",
    subtitle = "Every Bluesky post from three accounts between April and September 2026.",
    theme = theme(
      plot.background = element_rect(fill = PAPER, colour = NA),
      plot.title    = element_text(family = FONT, face = "bold", size = 30,
                                   colour = INK, margin = margin(b = 8, l = LEFT)),
      plot.subtitle = element_text(family = FONT, size = 11.5, colour = DECK,
                                   margin = margin(b = 26, l = LEFT)),
      plot.margin   = margin(30, 30, 26, 30)))

agg_png("posting-clock.png", width = 1650, height = 950, res = 150)
print(figure)
dev.off()
