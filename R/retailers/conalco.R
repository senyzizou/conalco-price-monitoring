# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261007

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) CONALCO
# <-----------------------------------------------------------------------------

class_xpath = \(class) paste0(
  "contains(concat(\" \", normalize-space(@class), ", "\" \"), \" ",
  class, " \")")

node_text = \(node, xpath) scalar(clean_text(xml_text(xml_find_first(node,
        xpath))))

node_attr = \(node, xpath, attr) scalar(xml_attr(xml_find_first(node,
      xpath), attr))

class_text = \(node, class) node_text(node, paste0(".//*[", class_xpath(class),
    "]"))

micro_content = \(node, prop) node_attr(node, paste0(".//meta[@itemprop=\"",
    prop,
    "\"]"), "content")

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) NUMERIC VALUES AND PACKS
# <-----------------------------------------------------------------------------

parse_number = \(x) {
  if (is.na(x) || !nzchar(x))
    return(NA_real_)
  x = clean_text(x)
  number_match = regexpr("-?[0-9]+(?:[.,][0-9]+)*", x, perl = T)
  if (number_match[1] < 0)
    return(NA_real_)
  number_text = regmatches(x, number_match)
  if (grepl(",", number_text, fixed = T))
  number_text = gsub(".", "", number_text, fixed = T)
  else if (grepl("^[0-9]{1,3}(\\.[0-9]{3})+$", number_text))
    number_text = gsub(".", "", number_text, fixed = T)
  as.numeric(sub(",", ".", number_text, fixed = T))
}

parse_decimal = \(x) {
  if (is.na(x))
    NA_real_
  else suppressWarnings(as.numeric(sub(",", ".", trimws(x), fixed = T)))
}

match_groups = \(text, pattern) {
  if (is.na(text))
    return(character())
  matched_groups = regexec(pattern, text, perl = T, ignore.case = T)
  regmatches(text, matched_groups)[[1]]
}

parse_volume = \(text) {
  volume_match = match_groups(text, paste0(
      "(-?[0-9]+(?:[.,][0-9]+)?)\\s*(milliliter|ml|",
      "centiliter|cl|liter|l)\\b"))
  if (length(volume_match) < 3L)
    return(NA_real_)
  scale = switch(tolower(volume_match[3]), ml = 0.001, milliliter = 0.001,
    cl = 0.01, centiliter = 0.01,
      1)
  parse_decimal(volume_match[2]) * scale
}

parse_pack = \(name, displayed_volume) {
  pack_match = match_groups(name, paste0("([0-9]+)\\s*[x×]\\s*([0-9]+(?:[.,",
      "][0-9]+)?\\s*(?:ml|cl|l|liter))\\b"))
  if (length(pack_match) >= 3L)
    return(list(pack_count = as.numeric(pack_match[2]), unit_volume_l =
        parse_volume(pack_match[3]),
        volume_l = as.numeric(pack_match[2]) * parse_volume(pack_match[3]),
        basis = "explicit_multipack"))
  # Ambiguous sets must not be silently treated as one homogeneous bottle.
  if (!is.na(name) && grepl(
      "Paket|\\bSet\\b|Geschenkset|Miniaturset|[0-9]+er[ -]|\\+",
      name, ignore.case = T, perl = T))
    return(list(pack_count = NA_real_, unit_volume_l = NA_real_, volume_l =
        displayed_volume,
        basis = "unclear_pack"))
  list(pack_count = 1, unit_volume_l = displayed_volume, volume_l =
    displayed_volume,
    basis = "single_unit_no_pack_marker")
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) OBSERVATION SCHEMA
# <-----------------------------------------------------------------------------

empty_observation = \(meta, expected_id = NA_character_, beverage_group =
  NA_character_) {
  fields = c("run_id", "observed_at", "date", "retailer",
    "retailer_product_id", "sku",
    "requested_product_id", "ean_gtin", "ean_gtin_all", "product_url",
    "product_name",
    "brand", "retailer_category", "beverage_group", "tax_group", "volume_l",
    "pack_count",
    "unit_volume_l", "pack_basis", "abv", "abv_qualifier", "price_current",
    "price_reference",
    "price_reference_type", "price_reference_label", "price_per_l_displayed",
    "price_per_l",
    "promo", "promo_label", "promo_valid_until", "availability",
    "availability_raw",
    "stock_quantity", "deposit", "currency", "raw_file", "raw_sha256",
    "response_status",
    "scraper_version", "parser_version", "parse_status", "parse_error",
    "validation_flags",
    "price_current_raw", "price_reference_raw", "volume_raw", "abv_raw",
    "deposit_raw",
    "gtin_raw", "price_per_l_raw", "promo_valid_until_raw", "source_format")
  x = as.list(setNames(rep(NA_character_, length(fields)), fields))
  for (n in c("volume_l", "pack_count", "unit_volume_l", "abv",
      "price_current", "price_reference",
      "price_per_l_displayed", "price_per_l", "stock_quantity", "deposit",
      "response_status")) x[[n]] = NA_real_
  x$promo = NA
  x$run_id = scalar(meta$run_id)
  x$observed_at = scalar(meta$observed_at_utc)
  x$date = berlin_date(x$observed_at)
  x$retailer = "conalco"
  x$requested_product_id = expected_id
  x$beverage_group = beverage_group
  x$product_url = scalar(meta$request_url)
  x$raw_file = scalar(meta$raw_file)
  x$raw_sha256 = scalar(meta$raw_sha256)
  x$response_status = if (is.null(meta$response_status))
    NA_real_
  else as.numeric(meta$response_status)
  x$scraper_version = scalar(meta$scraper_version)
  x$parser_version = PARSER_VERSION
  x$availability = "unknown"
  x$validation_flags = ""
  x$parse_status = "error"
  as.data.frame(x, stringsAsFactors = F)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) ARCHIVED PRODUCT DETAILS
# <-----------------------------------------------------------------------------

parse_conalco = \(metadata_file, expected_id = NA_character_,
  beverage_group = NA_character_) {
  meta = read_json(metadata_file, simplifyVector = T)
  product = empty_observation(meta, expected_id, beverage_group)
  if (is.na(product$response_status) || !product$response_status %in% 200:299) {
    product$parse_status = "http_error"
    product$parse_error = scalar(meta$error, paste0("HTTP ",
        product$response_status))
    return(product)
  }
  tryCatch({
      archived = read_archive(metadata_file)
      document = read_html(archived$body, encoding = "UTF-8")
      buy_boxes = xml_find_all(document, paste0("//*[", class_xpath(
            "product-detail-buy"),
          "]"))
      if (length(buy_boxes) != 1L)
        stop("Expected exactly one main product buy box; found ", length(
            buy_boxes))
      buy_box = buy_boxes[[1]]
      # Actual Conalco responses use schema.org HTML microdata, not JSON-LD.
      # Scope every price/identifier/availability to the main buy box.
      product$source_format = "html_microdata_and_main_buy_box"
      ## Main product identity <--------------------------------------------

      product$retailer_product_id = micro_content(buy_box, "productID")
      product$sku = node_text(buy_box, ".//*[@itemprop=\"sku\"]")
      product$product_name = node_text(document, paste0("//*[", class_xpath(
            "product-detail-name"),
          "]"))
      canonical_url = node_attr(document, "//link[@rel=\"canonical\"]", "href")
      if (!is.na(canonical_url))
      product$product_url = canonical_url
      product$brand = node_attr(buy_box,
        ".//*[@itemprop=\"brand\"]//meta[@itemprop=\"name\"]",
        "content")
      gtins = unique(xml_attr(xml_find_all(buy_box, paste0(
              ".//meta[@itemprop=\"gtin13\" or @itemprop=\"gtin8\" or ",
              "@itemprop=\"gtin12\" or @itemprop=\"gtin14\"]"
          )),
          "content"))
      gtins = gtins[!is.na(gtins) & nzchar(gtins)]
      product$ean_gtin = scalar(gtins)
      product$ean_gtin_all = if (length(gtins))
        paste(gtins, collapse = "|")
      else NA_character_
      product$gtin_raw = product$ean_gtin_all
      category = xml_find_all(buy_box,
        ".//a[starts-with(normalize-space(.),\"Kategorie:\")]")
      categories = clean_text(sub("^Kategorie:\\s*", "", xml_text(category)))
      product$retailer_category = if (length(categories))
        paste(unique(categories), collapse = " | ")
      else NA_character_
      ## Displayed prices <-------------------------------------------------

      product$currency = micro_content(buy_box, "priceCurrency")
      product$price_current = parse_decimal(micro_content(buy_box, "price"))
      price_node = xml_find_first(buy_box, paste0(".//*[", class_xpath(
            "product-detail-price"),
          "]"))
      price_fragment = read_html(paste0("<div>", as.character(price_node),
          "</div>"), encoding = "UTF-8")
      excluded = xml_find_all(price_fragment, paste0("//*[", class_xpath(
            "list-price"),
          " or ", class_xpath("list-price-price"), "]"))
      if (length(excluded))
        xml_remove(excluded)
      product$price_current_raw = clean_text(xml_text(xml_find_first(
            price_fragment, "//body")))
      visible_price = parse_number(product$price_current_raw)
      if (is.na(product$price_current))
      product$price_current = visible_price
      if (!is.na(visible_price) && !is.na(product$price_current) && abs(
          visible_price - product$price_current) >
          0.011)
      product$validation_flags = add_flag(product$validation_flags,
        "structured_visible_price_disagree")
      product$price_reference_raw = class_text(buy_box, "list-price-price")
      product$price_reference = parse_number(product$price_reference_raw)
      label = class_text(buy_box, "list-price-label")
      product$price_reference_label = label
      product$price_reference_type = if (is.na(product$price_reference))
        NA_character_
      else if (!is.na(label) && grepl("UVP", label))
          "rrp"
        else if (!is.na(label) && grepl("Regulärer Preis", label, fixed = T))
            "shop_label_regular_same_unit"
          else "displayed_strikethrough_unspecified"
      product$promo_label = class_text(buy_box, "list-price-percentage")
      if (is.na(product$promo_label))
      product$promo_label = class_text(buy_box, "product-detail-list-price")
      ## Sale units and pack sizes <----------------------------------------

      product$volume_raw = class_text(buy_box, "price-unit-content")
      displayed_volume = parse_volume(product$volume_raw)
      pack = parse_pack(product$product_name, displayed_volume)
      product$volume_l = pack$volume_l
      product$unit_volume_l = pack$unit_volume_l
      product$pack_count = pack$pack_count
      product$pack_basis = pack$basis
      if (pack$basis == "unclear_pack")
      product$validation_flags = add_flag(product$validation_flags,
        "pack_size_unclear")
      if (!is.na(displayed_volume) && !is.na(product$volume_l) && abs(
          displayed_volume - product$volume_l) >
          1e-04)
      product$validation_flags = add_flag(product$validation_flags,
        "pack_displayed_volume_disagree")
      ## Alcohol evidence <-------------------------------------------------

      highlights = xml_find_all(buy_box, paste0(".//*[", class_xpath(
            "listehighlights"),
          "]"))
      highlight_text = clean_text(xml_text(highlights))
      abv = highlight_text[grepl("Alkoholgehalt:", highlight_text, fixed = T)]
      product$abv_raw = scalar(abv)
      abv_match = match_groups(product$abv_raw, paste0(
          "Alkoholgehalt:\\s*([<>]?)\\s*([0-9]+(?:[.,", "][0-9]+)?)\\s*%"))
      if (length(abv_match) >= 3) {
        product$abv = parse_decimal(abv_match[3])
        product$abv_qualifier = if (nzchar(abv_match[2]))
          abv_match[2]
        else "exact"
      }
      # Do not equate an alcohol-free marketing label with an exact ABV of zero.
      product$price_per_l_raw = class_text(buy_box,
        "price-unit-reference-content")
      unit_price_match = match_groups(product$price_per_l_raw,
        "([0-9.,]+)\\s*€\\*?\\s*/\\s*([0-9.,]+)\\s*(l|liter)\\b")
      if (length(unit_price_match) >= 4)
      product$price_per_l_displayed = parse_number(unit_price_match[2])/
        parse_decimal(unit_price_match[3])
      if (!is.na(product$volume_l) && product$volume_l > 0)
      product$price_per_l = product$price_current/product$volume_l
      ## Availability, deposit and offer dates <----------------------------

      product$availability_raw = class_text(buy_box,
        "product-detail-delivery-information")
      availability_uri = node_attr(buy_box,
        ".//link[@itemprop=\"availability\"]", "href")
      if (!is.na(availability_uri)) {
        state = sub(".*/", "", availability_uri)
        product$availability = switch(state, InStock = "in_stock",
          OutOfStock = "out_of_stock",
          SoldOut = "out_of_stock", BackOrder = "backorder", PreOrder =
          "preorder",
            "unknown")
      }
      if (product$availability == "unknown" && !is.na(
          product$availability_raw) &&
          grepl("nicht verfügbar|ausverkauft",
          product$availability_raw, ignore.case = T))
      product$availability = "out_of_stock"
      stock = match_groups(product$availability_raw,
        "Lagerbestand:\\s*([0-9]+)")
      if (length(stock) >= 2)
      product$stock_quantity = as.numeric(stock[2])
      product$deposit_raw = class_text(buy_box, "product-detail-deposit")
      product$deposit = parse_number(product$deposit_raw)
      # No displayed deposit remains unknown, not zero.
      offer_end_text = highlight_text[grepl("Angebotsende", highlight_text,
          fixed = T)]
      product$promo_valid_until_raw = scalar(offer_end_text)
      product$promo_valid_until = parse_german_date(
        product$promo_valid_until_raw)
      # Only the explicitly labelled regular price in this same main buy box
      # establishes the comparable single-unit basis; all other types stay NA.
      if (!is.na(product$price_reference) && identical(
          product$price_reference_type,
          "shop_label_regular_same_unit") &&
          !is.na(product$price_current) && !is.na(product$volume_l) &&
          pack$basis != "unclear_pack")
      product$promo = product$price_current < product$price_reference - 0.005
      if (!is.na(expected_id) && !identical(product$retailer_product_id,
          expected_id))
      product$validation_flags = add_flag(product$validation_flags,
        "requested_returned_identifier_mismatch")
      required = c("retailer_product_id", "product_name", "price_current",
        "volume_l")
      product$parse_status = if (any(vapply(product[required], \(v) is.na(v[
                1]), logical(1))))
        "partial"
      else "ok"
      product
    }, error = \(error) {
      product$parse_status = "error"
      product$parse_error = conditionMessage(error)
      product
  })
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) OFFER DATES AND CARD DISCOVERY
# <-----------------------------------------------------------------------------

parse_german_date = \(text) {
  date_match = match_groups(text, paste0(
      "([0-9]{1,2})[.]?\\s+(Januar|Februar|März|April|Mai|Juni|",
      "Juli|August|September|Oktober|November|",
      "Dezember)\\s+([0-9]{4})"
  ))
  if (length(date_match) < 4)
    return(NA_character_)
  months = tolower(c("Januar", "Februar", "März", "April", "Mai", "Juni",
      "Juli", "August",
      "September", "Oktober", "November", "Dezember"))
  date = sprintf("%s-%02d-%02d", date_match[4], match(tolower(date_match[3]),
      months),
    as.integer(date_match[2]))
  parsed = as.Date(date, format = "%Y-%m-%d")
  if (is.na(parsed))
    NA_character_
  else format(parsed, "%Y-%m-%d")
}

discover_conalco_cards = \(raw_file) {
  # Offline discovery helper for a bounded category snapshot, no pagination.
  document = read_html(raw_file, encoding = "UTF-8")
  cards = xml_find_all(document, paste0("//*[", class_xpath("product-box"),
      "][@data-product-information]"))
  rows = lapply(cards, \(card) {
      info = fromJSON(xml_attr(card, "data-product-information"))
      data.frame(retailer_product_id = scalar(info$id), product_name =
        scalar(info$name),
        product_url = node_attr(card, paste0(".//a[", class_xpath(
              "product-name"),
            "]"), "href"), listing_text = clean_text(xml_text(card)),
        stringsAsFactors = F)
  })
  if (!length(rows))
    return(data.frame())
  x = do.call(rbind, rows)
  x[!duplicated(x$retailer_product_id), , drop = F]
}
