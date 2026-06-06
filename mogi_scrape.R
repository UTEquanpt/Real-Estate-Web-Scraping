# MOGI scraping script for TP.HCM sale/rent listings and details
# This script can read saved HTML files or live Mogi pages, then export a raw CSV.

suppressPackageStartupMessages({
  library(rvest)
  library(httr)
  library(xml2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(lubridate)
})

ua_string <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36"

safe_read_html <- function(source) {
  if (str_detect(source, "^https?://")) {
    resp <- httr::GET(source, httr::user_agent(ua_string), httr::timeout(30))
    if (httr::status_code(resp) != 200) {
      stop(sprintf("Cannot fetch page: %s (status %s)", source, httr::status_code(resp)))
    }
    # Force UTF-8 decoding of HTTP content to avoid garbled Vietnamese
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    xml2::read_html(txt)
  } else {
    xml2::read_html(source)
  }
}

normalize_text <- function(x) {
  if (length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  x <- str_replace_all(x, "[\r\n\t]+", " ")
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}

load_checkpoint <- function(path) {
  if (!file.exists(path)) return(tibble(source = character(), page = integer()))
  readRDS(path)
}

save_checkpoint <- function(checkpoint, path) {
  saveRDS(checkpoint, path)
}

save_raw_page <- function(data, path) {
  if (nrow(data) == 0) return(invisible(NULL))
  data <- data %>% distinct(url, .keep_all = TRUE)
  if (file.exists(path)) {
    existing <- read_csv(path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    combined <- bind_rows(existing, data) %>% distinct(url, .keep_all = TRUE)
    write_csv(combined, path, na = "")
  } else {
    write_csv(data, path, na = "")
  }
}

guess_loai_bds <- function(text) {
  text <- str_to_lower(ifelse(is.na(text), "", text))
  if (str_detect(text, regex("căn hộ|chung cư|officetel|studio", ignore_case = TRUE))) {
    "Căn hộ"
  } else if (str_detect(text, regex("đất|đất nền|lô đất|sân vườn", ignore_case = TRUE))) {
    "Đất nền"
  } else if (str_detect(text, regex("nhà phố|nhà riêng|nhà[[:space:]]|biệt thự|villa|shophouse", ignore_case = TRUE))) {
    "Nhà phố"
  } else if (str_detect(text, regex("phòng|kí túc xá|homestay|nhà trọ", ignore_case = TRUE))) {
    "Phòng/Cho thuê"
  } else {
    "Bất động sản khác"
  }
}

extract_quan_huyen <- function(address) {
  if (is.na(address) || address == "") return(NA_character_)
  address <- str_replace_all(address, "[[:space:]]+", " ")
  patterns <- c("Quận[[:space:]]*[^,]+", "Huyện[[:space:]]*[^,]+", "Quận[[:space:]]*\\d+", "Huyện[[:space:]]*\\d+")
  found <- purrr::map_chr(patterns, ~ str_extract(address, regex(.x, ignore_case = TRUE)))
  found <- found[!is.na(found) & found != ""]
  if (length(found) == 0) return(NA_character_)
  str_to_title(found[1])
}

parse_listing_node <- function(node, base_url = NULL, loai_gd = "ban") {
  title <- html_text(html_node(node, ".prop-title"), trim = TRUE)
  href <- html_attr(html_node(node, "a.link-overlay"), "href")
  if (!is.na(href) && !str_starts(href, "http")) {
    href <- xml2::url_absolute(href, base_url)
  }
  price <- html_text(html_node(node, ".price"), trim = TRUE)
  prop_addr <- html_text(html_node(node, ".prop-addr"), trim = TRUE)
  raw_attrs <- html_nodes(node, ".prop-attr > li, .prop-attr > div")
  raw_attr_text <- html_text(raw_attrs, trim = TRUE)
  raw_attr_text <- raw_attr_text[raw_attr_text != ""]

  dien_tich_raw <- raw_attr_text %>%
    keep(~ str_detect(.x, regex("\\d+[\\.,]?\\d*\\s*(m(\\\\^?2|2)|m²)", ignore_case = TRUE))) %>%
    first()
  so_phong_ngu <- raw_attr_text %>%
    keep(~ str_detect(.x, regex("phòng ngủ|pn|phong ngu|phòng", ignore_case = TRUE))) %>%
    first()

  tibble(
    tieu_de = normalize_text(title),
    gia_raw = normalize_text(price),
    dien_tich_raw = normalize_text(dien_tich_raw),
    # Detail page will provide full `dia_chi`; on listing keep NA so detail overrides it
    dia_chi = NA_character_,
    # Use listing `.prop-addr` only to extract the district (Quận/Huyện)
    quan_huyen = extract_quan_huyen(prop_addr),
    loai_bds = guess_loai_bds(title),
    loai_gd = loai_gd,
    url = normalize_text(href),
    nguon = "mogi.vn",
    so_phong_ngu = normalize_text(so_phong_ngu)
  )
}

extract_pagination_pages <- function(doc) {
  page_texts <- html_text(html_nodes(doc, ".pagination a"), trim = TRUE)
  page_numbers <- unique(as.integer(str_extract(page_texts, "\\d+")))
  page_numbers <- page_numbers[!is.na(page_numbers)]
  if (length(page_numbers) == 0) return(1L)
  max(page_numbers)
}

parse_listing_page <- function(doc, source = NULL, loai_gd = "ban") {
  nodes <- html_nodes(doc, ".props li")
  if (length(nodes) == 0) {
    nodes <- html_nodes(doc, "li")
  }
  if (length(nodes) == 0) {
    warning("No listing nodes found on page: ", source)
    return(tibble())
  }

  listings <- map_dfr(nodes, parse_listing_node, base_url = source, loai_gd = loai_gd)
  listings %>%
    filter(!is.na(tieu_de) | !is.na(gia_raw) | !is.na(url))
}

parse_detail_html <- function(doc) {
  # Extract address block (detail page) and clean district suffix
  address_raw <- html_text(html_node(doc, ".address"), trim = TRUE)
  # Remove trailing ", Quận X, TPHCM" or ", Huyện Y, TPHCM" if present
  dia_chi_clean <- NA_character_
  if (!is.na(address_raw) && nzchar(address_raw)) {
    dia_chi_clean <- str_replace(address_raw, "\\s*,?\\s*(Quận|Huyện)\\s*[^,]+\\s*,?\\s*TPHCM\\s*$", "")
    dia_chi_clean <- str_squish(dia_chi_clean)
    if (dia_chi_clean == "") dia_chi_clean <- NA_character_
  }

  info <- html_nodes(doc, ".info-attrs .info-attr")
  label_values <- map_dfr(info, function(node) {
    spans <- html_nodes(node, "span")
    labels <- html_text(spans, trim = TRUE)
    if (length(labels) < 2) return(tibble(label = NA_character_, value = NA_character_))
    tibble(label = normalize_text(labels[1]), value = normalize_text(labels[2]))
  })

  detail <- function(pattern) {
    row <- label_values %>% filter(str_detect(label, regex(pattern, ignore_case = TRUE)))
    if (nrow(row) == 0) NA_character_ else row$value[1]
  }

  iframe_node <- html_node(doc, ".map-content iframe")

  if (is.null(iframe_node)) {
    iframe_node <- html_node(doc, "iframe")
  }

  lat <- NA_real_
  lon <- NA_real_

  if (!is.null(iframe_node)) {

    iframe_url <- html_attr(iframe_node, "data-src")

    if (is.na(iframe_url) || iframe_url == "") {
      iframe_url <- html_attr(iframe_node, "src")
    }

    if (!is.na(iframe_url) && nzchar(iframe_url)) {

      coords <- str_match(
        iframe_url,
        "[?&]q=([0-9\\.-]+),([0-9\\.-]+)"
      )

      if (!is.null(dim(coords)) &&
          nrow(coords) > 0 &&
          ncol(coords) >= 3 &&
          !is.na(coords[1, 2])) {

        lat <- as.numeric(coords[1, 2])
        lon <- as.numeric(coords[1, 3])
      }
    }
  }
  # Try to extract the source ID (Mã BĐS) which helps dedupe later
  id_nguon_val <- detail("mã|mã bđs|mã bds|mã bd|mã bđs")

  tibble(
    dia_chi = normalize_text(address_raw),
    ngay_dang = normalize_text(detail("ngày đăng")),
    phap_ly = normalize_text(detail("pháp lý")),
    id_nguon = normalize_text(id_nguon_val),
    lat = lat,
    lon = lon
  )
}

build_page_urls <- function(base_url, pages = 1:3) {
  map_chr(pages, function(page) {
    if (page == 1) {
      base_url
    } else {
      if (str_detect(base_url, "\\?")) {
        paste0(base_url, "&cp=", page)
      } else {
        paste0(base_url, "?cp=", page)
      }
    }
  })
}

scrape_sources <- function(sources, checkpoint_path = "mogi_scrape_checkpoint.rds", raw_path = "mogi_hcm_raw.csv") {
  checkpoint <- load_checkpoint(checkpoint_path)
  results <- tibble()

  for (i in seq_len(nrow(sources))) {
    source_name <- sources$source[i]
    base_url <- sources$base_url[i]
    loai_gd <- sources$loai_gd[i]
    pages <- sources$pages[[i]]
    if (is.null(pages) || length(pages) == 0) {
      pages <- seq_len(8)
    }

    completed_pages <- checkpoint %>% filter(source == source_name) %>% pull(page)

    for (page in pages) {
      if (page %in% completed_pages) next
      page_url <- if (page == 1) base_url else paste0(base_url, "?cp=", page)
      message("Parsing listing page: ", page_url)

      doc <- tryCatch(
        safe_read_html(page_url),
        error = function(e) {
          warning("Page fetch failed: ", page_url, " -> ", e$message)
          return(NULL)
        }
      )

      if (is.null(doc)) break
      page_data <- parse_listing_page(doc, source = page_url, loai_gd = loai_gd)
      if (nrow(page_data) == 0) {
        message("No listings on page, stopping at page ", page)
        break
      }

      save_raw_page(page_data, raw_path)
      # Polite scraping: add delay between requests
      Sys.sleep(runif(1, 1.5, 2.5))
      checkpoint <- bind_rows(checkpoint, tibble(source = source_name, page = page)) %>% distinct(source, page)
      save_checkpoint(checkpoint, checkpoint_path)
      results <- bind_rows(results, page_data)
    }
  }

  results %>% distinct(url, .keep_all = TRUE)
}

scrape_details <- function(urls) {
  urls <- unique(urls[!is.na(urls)])
  if (length(urls) == 0)
    return(
      tibble(
        url = character(),
        dia_chi = character(),
        ngay_dang = character(),
        phap_ly = character(),
        id_nguon = character(),
        lat = numeric(),
        lon = numeric()
      )
    )
  map_dfr(urls, function(u) {
    message("Fetching detail page: ", u)
    doc <- tryCatch(safe_read_html(u), error = function(e) {
      warning("Cannot read detail page: ", u, " -> ", e$message)
      return(NULL)
    })
    if (is.null(doc))
      return(
        tibble(
          url = u,
          dia_chi = NA_character_,
          ngay_dang = NA_character_,
          phap_ly = NA_character_,
          id_nguon = NA_character_,
          lat = NA_real_,
          lon = NA_real_
        )
      )
    detail <- parse_detail_html(doc)
    # Polite scraping: add delay between detail page requests
    Sys.sleep(runif(1, 0.5, 1.5))
    tibble(url = u, !!!detail)
  })
}

# --- Main scrape workflow ---
listing_sources <- tibble(
  source = c("ban", "thue"),
  base_url = c("https://mogi.vn/ho-chi-minh/mua-nha-dat", "https://mogi.vn/ho-chi-minh/thue-nha-dat"),
  loai_gd = c("ban", "thue"),
  pages = list(1:200, 1:200)
)

# If `pages` is NULL, the script will detect the max pagination page from the first listing page.
# To limit scraping, set a specific page vector such as list(1:3, 1:3).

raw_listings <- scrape_sources(listing_sources)

if (nrow(raw_listings) == 0) {
  stop("No listings were extracted. Check the HTML source and selectors.")
}

# Attempt to enrich with detail page data when URLs are accessible.
detail_data <- scrape_details(raw_listings$url)

raw_data <- raw_listings %>%
  left_join(detail_data, by = "url", suffix = c("_listing", "_detail")) %>%
  mutate(
    dia_chi = coalesce(dia_chi_detail, dia_chi_listing)
  ) %>%
  select(
    tieu_de, gia_raw, dien_tich_raw, quan_huyen, loai_bds, loai_gd,
    url, nguon, so_phong_ngu, dia_chi, ngay_dang, phap_ly, id_nguon, lat, lon
  )

write_csv(raw_data, "mogi_hcm_raw.csv", na = "")
message("Raw scrape saved to mogi_hcm_raw.csv")
