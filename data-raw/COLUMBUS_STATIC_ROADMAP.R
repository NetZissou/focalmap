## code to prepare `COLUMBUS_STATIC_ROADMAP` dataset goes here
library(ggmap)

# ggmap::register_google(
#   key = "",
#   write = TRUE
# )

COLUMBUS_STATIC_ROADMAP <-
  ggmap::get_googlemap("Columbus, United States", zoom = 10, maptype = "roadmap")

usethis::use_data(COLUMBUS_STATIC_ROADMAP, overwrite = TRUE)
