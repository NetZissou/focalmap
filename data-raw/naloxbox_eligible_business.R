## code to prepare `naloxbox_eligible_business` dataset goes here
ELIGIBLE_BUSINESS_TYPE <-
  c(
    # Library
    "LIBRARIES-PUBLIC",
    # Convenience/liquor stores
    "CONVENIENCE STORES", "VARIETY STORES",
    # Fast food places/Restaurants
    "FOOD BANKS", "RESTAURANTS",
    # Check cashing places
    # Gas station
    "SERVICE STATIONS-GASOLINE & OIL",
    # Motels
    # Pawn shops
    "PAWNBROKERS",
    # Hardware stores
    # Hair Salon
    "CHILDREN'S HAIR SALON",
    "MEN'S HAIRSTYLING",
    "BARBERS",
    "BEAUTY SALONS"
  )
usethis::use_data(ELIGIBLE_BUSINESS_TYPE, overwrite = TRUE)
