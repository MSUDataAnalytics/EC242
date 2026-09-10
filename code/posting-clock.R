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
# Two steps: pull the posts (Python, below in comments), then draw the clock (R).
# ---------------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# --- 1. The data -----------------------------------------------------------
# Collected from https://public.api.bsky.app/xrpc/app.bsky.feed.getAuthorFeed
# by paging with a cursor, 100 posts at a time, dropping reposts. The saved
# copy lives with the course data so this figure is reproducible even after
# these accounts keep posting:

posts <- read_csv("https://ec242.netlify.app/data/bluesky_posting_2026.csv")

# --- 2. Whose clock is whose -----------------------------------------------
# A timestamp is meaningless until you know where the person was standing.
# Every post arrives in UTC; "3am" only means something in local time.

tz_of  <- c("apnews.com"              = "America/New_York",
            "nytimes.com"             = "America/New_York",
            "georgetakei.bsky.social" = "America/Los_Angeles")

name_of <- c("apnews.com"              = "The Associated Press",
             "nytimes.com"             = "The New York Times",
             "georgetakei.bsky.social" = "George Takei")

clock <- posts %>%
  mutate(utc = ymd_hms(created_at, tz = "UTC")) %>%
  filter(year(utc) == 2026) %>%
  rowwise() %>%
  mutate(local = with_tz(utc, tz_of[[account]])) %>%   # <- the whole trick
  ungroup() %>%
  mutate(hour = hour(local),
         dow  = wday(local, label = TRUE, week_start = 1),
         who  = factor(name_of[account], levels = unname(name_of))) %>%
  count(who, dow, hour)

# --- 3. The figure ---------------------------------------------------------
# viridis, not a rainbow -- see Week 3 for why.

ggplot(clock, aes(hour, fct_rev(dow), fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~ who, ncol = 1) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 23),
                     labels = c("midnight", "6am", "noon", "6pm", "11pm"),
                     expand = c(0, 0)) +
  scale_fill_continuous(type = "viridis") +
  labs(title    = "Who is awake at 3am?",
       subtitle = "Bluesky posts in 2026, by local time of day",
       x = NULL, y = NULL, fill = "posts") +
  theme_minimal(base_size = 10) +
  theme(panel.grid  = element_blank(),
        legend.position = "bottom",
        strip.text  = element_text(face = "bold", hjust = 0),
        plot.title  = element_text(face = "bold", size = 15))

# --- 4. The number in the caption ------------------------------------------
posts %>%
  mutate(utc = ymd_hms(created_at, tz = "UTC")) %>%
  filter(year(utc) == 2026) %>%
  rowwise() %>% mutate(local = with_tz(utc, tz_of[[account]])) %>% ungroup() %>%
  group_by(who = name_of[account]) %>%
  summarise(posts = n(),
            overnight_pct = round(100 * mean(hour(local) >= 1 & hour(local) < 5), 1))
