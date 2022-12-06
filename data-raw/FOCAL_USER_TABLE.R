## code to prepare `FOCAL_USER_TABLE` dataset goes here

# ====================== #
# ---- Agency: FCPH ----
# ====================== #
user_FCPH <-
  tibble::tibble(
    agency = "FCPH",
    name = c(
      "Abigail Boeckman",
      "Alex Woroncow",
      "Emily Vander Werf",
      "Han-Tian Guo",
      "Hazem Gammoh",
      "Jessica Williams",
      "Kristin Peters",
      "Lindsey Rodenhauser",
      "Nicholas Black",
      "Rebecca Ajibola",
      "Samantha Batdorf",
      "Shalomel Achi",
      "Sierra MacEachron",
      "Theresa Seagraves"
    ),
    email = c(
      "AbigailBoeckman@franklincountyohio.gov",
      "AlexanderWoroncow@franklincountyohio.gov",
      "EmilyVanderwerf@franklincountyohio.gov",
      "hantian.guo@franklincountyohio.gov",
      "HazemGammoh@franklincountyohio.gov",
      "JessicaWilliams@franklincountyohio.gov",
      "Kristinpeters@franklincountyohio.gov",
      "lindseyrodenhauser@franklincountyohio.gov",
      "NicholasBlack@franklincountyohio.gov",
      "RebeccaAjibola@franklincountyohio.gov",
      "samanthabatdorf@franklincountyohio.gov",
      "ShalomelAchi@franklincountyohio.gov",
      "SierraMacEachron@franklincountyohio.gov",
      "Theresaseagraves@franklincountyohio.gov"
    ),
    username = c(
      "abigailboeckman",
      "alexworoncow",
      "emilyvanderwerf",
      "hantianguo",
      "hgammoh",
      "williams4148",
      "kristinnicole",
      "lmrodenh",
      "nickblack",
      "lajibola",
      "samajean",
      "shalomel",
      "smaceachron",
      "seagravest01"

    ),

    duo = c(
      rep(TRUE, 14)
    ),
    onboard = c(
      rep(TRUE, 14)
    )
  )

# ===================== #
# ---- Agency: CPH ----
# ===================== #

user_CPH <-
  tibble::tibble(
    agency = "CPH",
    name = c(
      "Tami Langen",
      "Matthew Parrish",
      "Andrew Murphy",
      "David Norris",
      "Becky Zwickl",
      "Austin Maloney",
      "Wodarcyk Olivia",
      "Elizabeth Wilson",
      "Tiffany Krauss",
      "Emily Alexy",
      "Michelle Groux",
      "Isaac Toliver",
      "Andrea Boxill",
      "Leigh Nelson",
      "Ben DeJesus",
      "Kathleen Cowen",
      "Leigh Rousseau"
    ),
    email = c(
      "tllangen@columbus.gov",
      "mparrish@columbus.gov",
      "ajmurphy@columbus.gov",
      "dlnorris@columbus.gov",
      "bczwickl@columbus.gov",
      "jamaloney@columbus.gov",
      "ocwodarcyk@columbus.gov",
      "emwilson@columbus.gov",
      "tskrauss@columbus.gov",
      "eralexy@columbus.gov",
      "mgroux@columbus.gov",
      "imtoliver@columbus.gov",
      "akboxill@columbus.gov",
      "lanelson@columbus.gov",
      "bend@columbus.gov",
      "kathyc@columbus.gov",
      "larousseau@columbus.gov"

    ),

    username = c(
      "tamilangen",
      NA_character_,
      "ajmurphy",
      "davidnorris1217",
      "bczwickl",
      "jamaloney",
      "ocwodarcyk",
      NA_character_,
      NA_character_,
      "eralexy",
      "mgroux",
      "imtoliver",
      "llixoba",
      NA_character_,
      "bend",
      "kathyc",
      "larousseau"
    ),

    duo = c(
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE
    ),

    onboard = c(
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE
    )
  )

# ===================== #
# ---- Agency: ADAMH ----
# ===================== #
user_ADAMH <-
  tibble::tibble(
    agency = "ADAMH",
    name = c(
      "Dreanne Zimmerman",
      "McKayla Elliott",
      "Ben Tryon",
      "Sue Villilo",
      "Karly Tennant",
      "Stacy Herman",
      "Lisa Allison",
      "Nettie Ferguson",
      "Robert Lonardo",
      "Casey Bolitho",
      "Meg Griffing"
    ),
    email = c(
      "dzimmerman@adamhfranklin.org",
      "melliott@adamhfranklin.org",
      "btryon@adamhfranklin.org",
      "svillilo@adamhfranklin.org",
      "ktennant@adamhfranklin.org",
      "sherman@adamhfranklin.org",
      "lallison@adamhfranklin.org",
      "nferguson@adamhfranklin.org",
      "rlonardo@adamhfranklin.org",
      "cbolitho@adamhfranklin.org",
      "mgriffing@adamhfranklin.org"
    ),
    username = c(
      "dzimmerman",
      "mckaylaelliott",
      "btryon",
      NA_character_,
      "ktennant",
      "stacyherman",
      "lisaallison",
      NA_character_,
      "rlonardo",
      "caseybolitho",
      "meggriffing"
    ),
    duo = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      TRUE
    ),
    onboard = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE
    )
  )

# =================== #
# ---- Aggregate ----
# =================== #

FOCAL_USER_TABLE <-
  dplyr::bind_rows(
    user_FCPH,
    user_CPH,
    user_ADAMH
  )


usethis::use_data(FOCAL_USER_TABLE, overwrite = TRUE)
