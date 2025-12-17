# Publisher Consolidation Mapping
# Tracks corporate ownership and M&A for accurate publisher-level analysis
# Update this file when acquisitions occur
#
# Last updated: 2025-12-17

# Format: list of corporate parents with their subsidiary publisher names
# The subsidiary names should match exactly what Sensor Tower returns

publisher_consolidation <- list(

  # Scopely (Savvy Games Group subsidiary)
  "Scopely" = c(
    "Scopely",
    "Niantic, Inc.",           # Acquired 2025 (closed May 29, 2025)
    "GSN Games",               # Acquired 2017
    "DIGIT Game Studios",      # Acquired 2020
    "PierPlay"                 # Acquired 2021
  ),

  # Supercell
  "Supercell" = c(
    "Supercell",
    "Supercell Oy"
  ),

  # Playrix
  "Playrix" = c(
    "Playrix",
    "Playrix Games"
  ),

  # Dream Games
  "Dream Games" = c(
    "Dream Games",
    "Dream Games, Inc.",
    "Dream Games Teknoloji Anonim Sirketi"
  ),

  # Moon Active
  "Moon Active" = c(
    "Moon Active",
    "Moon Active Ltd"
  ),

  # Bandai Namco
  "Bandai Namco" = c(
    "BANDAI NAMCO Entertainment Inc.",
    "BANDAI NAMCO Entertainment",
    "Bandai Namco Entertainment"
  ),

  # Konami
  "Konami" = c(
    "KONAMI",
    "Konami",
    "Konami Digital Entertainment"
  ),

  # Tencent - note: ST often shows multiple Tencent entities
  "Tencent" = c(
    "Shenzhen Tencent Tianyou Technology Ltd",
    "Tencent",
    "Tencent Games",
    "Tencent Mobile Games",
    "Tencent Technology (Shenzhen) Company Limited",
    "Tencent Technology (Shenzhen) Company Ltd",
    "Level Infinite",          # Tencent's global publishing brand
    "Proxima Beta",            # Tencent subsidiary
    "PROXIMA BETA PTE. LIMITED",
    "Tencent Mobile International Limited",
    "Shenzhen Tencent Computer Systems Co., Ltd.",
    "Shenzhen Tencent Computer Systems Company Limited",
    "TiMi Studio Group"        # Tencent subsidiary
  ),

  # NetEase
  "NetEase" = c(
    "网易移动游戏",
    "NetEase Games",
    "NetEase Games Global",
    "NetEase",
    "NetEase, Inc",
    "NetEase, Inc."
  ),

  # HoYoverse / miHoYo
  # Sensor Tower may surface either the global publishing entity (Cognosphere) or the studio label.
  "HoYoverse" = c(
    "HoYoverse",
    "miHoYo Games",
    "miHoYo",
    "miHoYo Limited",
    "COGNOSPHERE PTE. LTD.",
    "COGNOSPHERE PTE LTD",
    "Cognosphere Pte. Ltd.",
    "Cognosphere Pte Ltd"
  ),

  # Sea Limited / Garena
  "Sea (Garena)" = c(
    "Garena",
    "GARENA INTERNATIONAL I PRIVATE LIMITED",
    "Garena International I Private Limited",
    "Garena Online"
  ),

  # Embracer Group (note: various restructurings)
  "Embracer Group" = c(
    "THQ Nordic",
    "Deep Silver",
    "Crystal Dynamics"
  ),

  # AppLovin
  "AppLovin" = c(
    "AppLovin",
    "Machine Zone",            # Acquired 2020
    "Belka Games"              # Acquired 2021
  ),

  # Playtika
  "Playtika" = c(
    "Playtika",
    "Playtika Holding Corp",
    "Wooga",                   # Acquired 2018
    "Seriously Digital Entertainment", # Acquired 2019
    "Reworks"                  # Acquired 2021
  ),

  # Aristocrat (Product Madness)
  "Aristocrat" = c(
    "Aristocrat",
    "Aristocrat Leisure",
    "Product Madness"
  ),

  # Take-Two Interactive
  "Take-Two" = c(
    "Take-Two Interactive",
    "Take-Two Interactive Software, Inc.",
    "Zynga",                   # Acquired 2022
    "Zynga Inc.",
    "Rockstar Games",
    "Rockstar Games, Inc.",
    "2K Games",
    "2K, Inc.",
    "Socialpoint",             # Via Zynga
    "Peak Games",              # Via Zynga acquisition 2020
    "Rollic",                  # Via Zynga
    "Gram Games"               # Via Zynga
  ),

  # Electronic Arts
  "EA" = c(
    "Electronic Arts",
    "Electronic Arts Inc.",
    "EA",
    "EA Games",
    "EA SPORTS",
    "Glu Mobile",              # Acquired 2021
    "Playdemic",               # Acquired 2021
    "Codemasters"              # Acquired 2021
  ),

  # Activision Blizzard (now Microsoft)
  "Microsoft Gaming" = c(
    "Microsoft",
    "Microsoft Corporation",
    "Activision",
    "Activision Publishing",
    "Blizzard Entertainment",
    "King",                    # Activision subsidiary
    "King.com",
    "King.com Limited",
    "Xbox",
    "Xbox Game Studios",
    "Bethesda",
    "Bethesda Softworks",
    "ZeniMax",
    "ZeniMax Media"
  ),

  # Sony Interactive Entertainment
  "Sony" = c(
    "Sony",
    "Sony Interactive Entertainment",
    "Sony Interactive Entertainment LLC",
    "PlayStation",
    "PlayStation Mobile",
    "Bungie",                  # Acquired 2022
    "Savage Game Studios",
    "Firewalk Studios"          # Acquired 2023; shut down 2024
  ),

  # Savvy Games Group (Saudi PIF)
  "Savvy Games Group" = c(
    "Savvy Games Group",
    "Savvy Games",
    "Scopely",                 # Acquired 2023
    "ESL FACEIT Group"          # Acquired 2022
    # Note: Scopely subs listed under Scopely above
  )
)

#' Consolidate publisher names to corporate parent
#'
#' @param publisher_name Character vector of publisher names from Sensor Tower
#' @return Character vector of consolidated corporate parent names
consolidate_publisher <- function(publisher_name) {
  sapply(publisher_name, function(name) {
    for (parent in names(publisher_consolidation)) {
      if (name %in% publisher_consolidation[[parent]]) {
        return(parent)
      }
    }
    return(name)  # Return original if no mapping found
  }, USE.NAMES = FALSE)
}

#' Get all subsidiaries for a corporate parent
#'
#' @param parent_name Character. Name of corporate parent
#' @return Character vector of subsidiary publisher names
get_subsidiaries <- function(parent_name) {
  if (parent_name %in% names(publisher_consolidation)) {
    return(publisher_consolidation[[parent_name]])
  }
  return(parent_name)
}

#' Check if a publisher is a subsidiary
#'
#' @param publisher_name Character. Publisher name to check
#' @return List with is_subsidiary (logical) and parent (character or NA)
check_subsidiary <- function(publisher_name) {
  for (parent in names(publisher_consolidation)) {
    if (publisher_name %in% publisher_consolidation[[parent]]) {
      return(list(
        is_subsidiary = parent != publisher_name,
        parent = parent
      ))
    }
  }
  return(list(is_subsidiary = FALSE, parent = NA_character_))
}

message("Publisher consolidation mapping loaded. ", length(publisher_consolidation), " corporate groups defined.")
