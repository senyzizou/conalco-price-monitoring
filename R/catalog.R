# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261009

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) CATALOG
# <-----------------------------------------------------------------------------

# Archive responses before parsing the public catalogue.
CATALOG_VERSION = "conalco-catalog-0.3.4"

catalog_url = \(url) {
  if (is.na(url) || !nzchar(url))
    return(NA_character_)
  if (startsWith(url, "/"))
  url = paste0("https://www.conalco.de", url)
  if (!grepl("^https://www[.]conalco[.]de/", url))
    return(NA_character_)
  sub("[?#].*$", "", url)
}

catalog_links = \(doc) {
  nodes = xml_find_all(doc, paste0("//*[", class_xpath("category-navigation"),
      "]//a[@href]"))
  x = data.frame(category_name = clean_text(xml_text(nodes)), category_url =
    vapply(xml_attr(nodes,
        "href"), catalog_url, character(1)), stringsAsFactors = F)
  x = x[!is.na(x$category_url), , drop = F]
  # These are ingredients, accessories or editorial/brand trees, not drink
  # families.
  omit = grepl(paste0(
      "sirup|tonic|filler|gläser|glaser|barzubehör|zucker|",
      "eingelegte|getrocknete|konzentrate|gutscheine|sonstige ",
      "artikel|magazin|marken|adventskalender"
    ),
    x$category_name, ignore.case = T)
  x$decision = as.character(ifelse(omit, "excluded_nonfamily_category",
      "crawl"))
  x[!duplicated(x$category_url), , drop = F]
}

catalog_pagination = \(doc) {
  wrappers = xml_find_all(doc, "//*[@data-listing=\"true\"]")
  if (length(wrappers) != 1L)
    stop("Expected one primary product listing; found ", length(wrappers))
  current = fromJSON(xml_attr(wrappers[[1]],
      "data-listing-pagination-options"))$page
  # Disabled next controls contain current + 1 even on the terminal page.
  # The explicit last-page control is authoritative, including when disabled.
  last_controls = xml_find_all(wrappers[[1]],
    paste0('.//nav//li[', class_xpath('page-last'), ']//input[@name="p"]'))
  inputs = if (length(last_controls)) last_controls else xml_find_all(
      wrappers[[1]],
      './/nav//input[@name="p" and not(@disabled)]')
  values = suppressWarnings(as.integer(xml_attr(inputs, 'value')))
  list(current = as.integer(current), last = if (length(values)) max(c(1L,
          values),
        na.rm = T) else 1L)
}

catalog_abv = \(raw, name = NA_character_) {
  # Preserve bounds, but do not put a bound in the exact numeric measurement.
  # The labelled food information at the end is preferred to marketing prose.
  labelled = if (is.na(raw))
    character()
  else regmatches(raw, gregexpr(paste0(
          "Alkoholgehalt:\\s*(?:weniger als|unter|bis zu|ca[.]?|",
          "[<>≤≥])?\\s*[0-9]+(?:[.,][0-9]+)?\\s*%\\s*(?:vol[.]?)?"
        ),
        raw, ignore.case = T, perl = T))[[1]]
  raw = if (length(labelled))
    tail(labelled, 1)
  else raw
  alcohol_match = match_groups(raw, paste0(
      "Alkoholgehalt:\\s*((?:weniger als|unter|bis zu|ca[.]?|",
      "[<>≤≥])?\\s*[0-9]+(?:[.,][0-9]+)?\\s*%\\s*(?:vol[.]?)?)"
  ))
  source = "labelled_alcohol_content"
  if (length(alcohol_match) < 2) {
    alcohol_match = match_groups(name, paste0(
        "((?:\\bweniger als|\\bunter|\\bbis zu|\\bca[.]?|",
        "[<>≤≥])?\\s*[0-9]+(?:[.,][0-9]+)?\\s*%\\s*(?:vol[.]?)?)"
    ))
    source = "product_name_explicit_percent"
  }
  if (length(alcohol_match) < 2)
    return(list(value = NA_real_, raw = NA_character_, qualifier =
        NA_character_,
        source = NA_character_))
  text = clean_text(alcohol_match[2])
  if (source == 'product_name_explicit_percent' && parse_number(text) > 0 &&
      !grepl('vol', text, ignore.case = T))
    return(list(value = NA_real_, raw = text, qualifier =
        'not_explicit_abv_unit',
        source = 'ambiguous_product_name_percent_not_abv'))
  exact = !grepl("weniger|unter|bis zu|ca[.]?|[<>≤≥]", text, ignore.case = T)
  list(value = if (exact) parse_number(text) else NA_real_, raw = text,
    qualifier =
    if (exact) "exact" else "bound_or_approximate",
    source = source)
}

catalog_designation = \(text) {
  designation_match = match_groups(text, paste0(
      "Verkehrsbezeichnung:\\s*(.*?)(?:Alkoholgehalt:|",
      "Flascheninhalt|Zutaten:|Ursprungsland:|",
      "Lebensmittelunternehmer:|$)"
  ))
  if (length(designation_match) >= 2)
    clean_text(designation_match[2])
  else NA_character_
}

parse_catalog_pack = \(name, displayed_volume) {
  # These signs describe ageing, a brand, a mixer or an outer gift box.
  normalized = gsub(
    paste0("[0-9]+\\s*Jahre\\s*[+]\\s*[0-9]+\\s*Jahre|",
      "Erste\\s*[+]\\s*Neue|Rum\\s*[+]\\s*Cola|", "[+]\\s*GB\\b"),
    '', name, ignore.case = T, perl = T
  )
  normalized = gsub('\\b[0-9]{4}er\\b', '', normalized, perl = T)
  parse_pack(normalized, displayed_volume)
}

catalog_pack_check = \(x) {
  explicit_label = !is.na(x$volume_raw) &&
    grepl(paste0("^[0-9]+(?:[.,][0-9]+)?\\s*(?:milliliter|ml|",
      "centiliter|cl|liter|l)$"),
    x$volume_raw, ignore.case = T, perl = T)
  x$volume_l_displayed = if (explicit_label)
    parse_volume(x$volume_raw)
  else NA_real_
  pack = parse_catalog_pack(x$product_name, x$volume_l_displayed)
  if (pack$basis == 'single_unit_no_pack_marker' && x$pack_basis ==
      'unclear_pack') {
    x$pack_count = pack$pack_count
    x$unit_volume_l = pack$unit_volume_l
    x$pack_basis = pack$basis
  }
  x$pack_count_declared = if (pack$basis == "explicit_multipack")
    pack$pack_count
  else NA_real_
  x$unit_volume_l_declared = if (pack$basis == "explicit_multipack")
    pack$unit_volume_l
  else NA_real_
  x$total_volume_l_declared = if (pack$basis == "explicit_multipack")
    pack$volume_l
  else NA_real_
  if (pack$basis == "explicit_multipack" && !is.na(x$volume_l_displayed) &&
      abs(pack$volume_l -
        x$volume_l_displayed) > 1e-04) {
    x$volume_l = NA_real_
    x$pack_count = NA_real_
    x$unit_volume_l = NA_real_
    x$pack_basis = "conflicting_display_and_pack_title"
    x$price_per_l = NA_real_
    x$promo = NA
    x$parse_status = "partial"
    x$validation_flags = add_flag(x$validation_flags, "pack_volume_conflict")
  }
  if (x$pack_basis == "unclear_pack")
  x$validation_flags = add_flag(x$validation_flags, "pack_size_unclear")
  x
}

catalog_classify = \(name, designation, categories, alcohol_content) {
  x = classify_products(data.frame(product_name = name, designation_raw =
      designation,
      retailer_category = categories, alcohol_content = alcohol_content))
  fields = c("beverage_family", "beverage_family_source", "scope_status",
    "scope_reason",
    "no_lo_status", "no_lo_evidence", "classification_version",
    "classification_rule",
    "classification_source", "classification_evidence",
    "classification_conflict",
    "scope_rule", "scope_evidence")
  as.list(x[1, fields])
}

catalog_clean_price = \(node, class) {
  price_node = xml_find_first(node, paste0(".//*[", class_xpath(class), "]"))
  if (inherits(price_node, "xml_missing"))
    return(NA_character_)
  clone = read_html(paste0("<div>", as.character(price_node), "</div>"))
  xml_remove(xml_find_all(clone, paste0("//*[", class_xpath("list-price"),
        "]")))
  clean_text(xml_text(xml_find_first(clone, "//body")))
}

catalog_parse_cards = \(metadata_file, category_name, category_url, page,
  card_xpath = '//*[@data-listing="true"]//*[@data-product-information]') {
  archived = read_archive(metadata_file)
  document = read_html(archived$body, encoding = "UTF-8")
  cards = xml_find_all(document, card_xpath)
  rows = lapply(cards, \(card) {
      info = fromJSON(xml_attr(card, "data-product-information"))
      product = empty_observation(archived$meta, scalar(info$id))
      product$retailer_product_id = scalar(info$id)
      product$product_name = scalar(info$name)
      product$product_url = node_attr(card, paste0(".//a[", class_xpath(
            "product-name"), "]"),
        "href")
      product$retailer_category = category_name
      product$parser_version = CATALOG_VERSION
      product$source_format = "html_primary_listing_card"
      description = class_text(card, "product-description")
      product$designation_raw = catalog_designation(description)
      alcohol = catalog_abv(description, product$product_name)
      product$alcohol_content = alcohol$value
      product$alcohol_content_raw = alcohol$raw
      product$alcohol_content_source = alcohol$source
      product$abv = alcohol$value
      product$abv_raw = alcohol$raw
      product$abv_qualifier = alcohol$qualifier
      product$volume_raw = class_text(card, "price-unit-content")
      v = parse_volume(product$volume_raw)
      if (is.na(v)) {
        m = match_groups(description, paste0(
            "Flascheninhalt[^:]*:\\s*([0-9]+(?:[.,",
            "][0-9]+)?\\s*(?:Liter|l|ml|cl))"))
        if (length(m) >= 2) {
          product$volume_raw = m[2]
          v = parse_volume(m[2])
        }
      }
      if (is.na(v)) {
        product$volume_raw = product$product_name
        v = parse_volume(product$product_name)
      }
      pack = parse_pack(product$product_name, v)
      product$volume_l = pack$volume_l
      product$pack_count = pack$pack_count
      product$unit_volume_l = pack$unit_volume_l
      product$pack_basis = pack$basis
      product$price_current_raw = catalog_clean_price(card, "product-price")
      product$price_current = parse_number(product$price_current_raw)
      product$price_reference_raw = class_text(card, "list-price-price")
      product$price_reference = parse_number(product$price_reference_raw)
      product$price_reference_label = class_text(card, "list-price-label")
      product$price_reference_type = if (is.na(product$price_reference))
        NA_character_
      else if (grepl("Regulärer Preis", product$price_reference_label,
            fixed = T))
          "shop_label_regular_same_unit"
        else "displayed_strikethrough_unspecified"
      product$promo_label = class_text(card, "list-price-percentage")
      product$promo_valid_until_raw = scalar(regmatches(description, regexpr(
            "Angebotsende ist[^)]*",
            description)))
      product$promo_valid_until = parse_german_date(
        product$promo_valid_until_raw)
      product$price_per_l_raw = class_text(card, "price-unit-reference")
      unit_price_match = match_groups(product$price_per_l_raw,
        "([0-9.,]+)\\s*€\\*?\\s*/\\s*([0-9.,]+)\\s*(l|liter)\\b")
      if (length(unit_price_match) >= 4)
      product$price_per_l_displayed = parse_number(unit_price_match[2])/
        parse_decimal(unit_price_match[3])
      if (!is.na(product$volume_l) && product$volume_l > 0)
      product$price_per_l = product$price_current/product$volume_l
      if (!is.na(product$price_reference) && identical(
          product$price_reference_type,
          "shop_label_regular_same_unit") &&
          pack$basis != "unclear_pack")
      product$promo = product$price_current < product$price_reference - 0.005
      gtin_match = match_groups(description,
        "Verknüpfte EAN-Codes:\\s*((?:[0-9]{8,14}[^0-9]*)+)")
      if (length(gtin_match) >= 2) {
        gtin_codes = regmatches(gtin_match[2], gregexpr("[0-9]{8,14}",
            gtin_match[2]))[[1]]
        product$ean_gtin = scalar(gtin_codes)
        product$ean_gtin_all = paste(unique(gtin_codes), collapse = "|")
        product$gtin_raw = gtin_match[2]
      }
      card_text = clean_text(xml_text(card))
      deposit_match = match_groups(card_text, paste0(
          "((?:EINWEG|MEHRWEG)?\\s*zzgl[.]\\s*[0-9.,", "]+\\s*€\\s*Pfand)"))
      if (length(deposit_match) >= 2) {
        product$deposit_raw = deposit_match[2]
        product$deposit = parse_number(deposit_match[2])
      }
      # A cart button alone is not a measured stock or availability statement.
      product$availability_raw = class_text(card,
        "product-delivery-information")
      if (!is.na(product$availability_raw) && grepl(
          "nicht verfügbar|ausverkauft", product$availability_raw,
          ignore.case = T))
      product$availability = "out_of_stock"
      product$currency = if (grepl("€", product$price_current_raw, fixed = T))
        "EUR"
      else NA_character_
      product$parse_status = if (anyNA(product[c("retailer_product_id",
              "product_name", "price_current",
              "volume_l")]))
        "partial"
      else "ok"
      classification = catalog_classify(product$product_name,
        product$designation_raw, category_name,
        product$alcohol_content)
      for (field in names(classification)) product[[field]] = classification[[
          field]]
      product$beverage_group = product$beverage_family
      product$category_url = category_url
      product$category_page = page
      product$listing_metadata_file = metadata_file
      product$detail_metadata_file = NA_character_
      catalog_pack_check(product)
  })
  if (!length(rows))
    return(data.frame())
  do.call(rbind, rows)
}

catalog_detail = \(metadata_file, listing) {
  product = parse_conalco(metadata_file, listing$retailer_product_id,
    listing$beverage_family)
  archived = read_archive(metadata_file)
  document = read_html(archived$body, encoding = "UTF-8")
  description = node_text(document, paste0("//*[", class_xpath(
        "product-detail-description-text"),
      "]"))
  if (is.na(description))
  description = node_text(document, "//main")
  alcohol = catalog_abv(product$abv_raw, product$product_name)
  if (is.na(alcohol$raw) && !is.na(description))
  alcohol = catalog_abv(description, product$product_name)
  # A numeric name claim is usable, but never equate 'alkoholfrei' to zero.
  product$alcohol_content = alcohol$value
  product$alcohol_content_raw = alcohol$raw
  product$alcohol_content_source = alcohol$source
  product$abv = alcohol$value
  product$abv_raw = alcohol$raw
  product$abv_qualifier = alcohol$qualifier
  product$designation_raw = catalog_designation(description)
  if (is.na(product$designation_raw))
  product$designation_raw = listing$designation_raw
  classification = catalog_classify(product$product_name,
    product$designation_raw,
    paste(product$retailer_category,
      listing$retailer_category), product$alcohol_content)
  for (field in names(classification)) product[[field]] = classification[[
      field]]
  product$beverage_group = product$beverage_family
  product$parser_version = CATALOG_VERSION
  product$detail_retailer_category = product$retailer_category
  for (field in c("category_url", "category_page",
      "listing_metadata_file")) product[[field]] = listing[[field]]
  product$detail_metadata_file = metadata_file
  catalog_pack_check(product)
}

catalog_seeds = \() data.frame(category_name = c("Bier",
    "Alkoholfreie Getränke",
    "Alkoholfreie Weine&Co", "Gin", "Wein&Co", "Schaumwein", "Spirituosen",
    "Likör",
    "Obst", "Rum", "Anis", "Whisky", "Vodka", "Mehr", "Katalog"), category_url =
  paste0("https://www.conalco.de",
    c("/bier/", "/alkoholfreie-getraenke/", "/Alkoholfreie-Weine-Co",
      "/gin/", "/wein-co/",
      "/schaumwein/", "/spirituosen/", "/likoer/", "/obst/", "/rum/",
      "/anis/", "/whisky/",
      "/vodka/", "/mehr/", "/Katalog")), stringsAsFactors = F)

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) CATALOGUE RESPONSE PARSING
# <-----------------------------------------------------------------------------

catalog_process_page = \(metadata, category_name, url, page) {
  if (is.null(metadata) || metadata$response_status != 200L)
    return(NULL)
  doc = read_html(read_archive(metadata$metadata_file)$body, encoding = "UTF-8")
  pagination = tryCatch(catalog_pagination(doc), error = \(e) NULL)
  if (is.null(pagination))
    return(list(doc = doc, pagination = NULL, rows = data.frame()))
  rows = catalog_parse_cards(metadata$metadata_file, category_name, url, page)
  list(doc = doc, pagination = pagination, rows = rows)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) PAGINATION AND INITIAL VALIDATION
# <-----------------------------------------------------------------------------

catalog_page_checks = \(parsed, page, last_page, members, category_url) {
  good = !is.null(parsed) && !is.null(parsed$pagination) &&
    parsed$pagination$current ==
    page && nrow(parsed$rows) > 0L
  empty_verified = !is.null(parsed) && !is.null(parsed$pagination) && page ==
    1L &&
    last_page == 1L && parsed$pagination$current == 1L && nrow(parsed$rows) ==
    0L && any(grepl("Keine Produkte gefunden.", clean_text(xml_text(
          xml_find_all(parsed$doc,
            paste0("//*[@data-listing=\"true\"]//*[contains(@class,",
              "\"alert\")]")))), fixed = T))
  if (good && page > 1 && length(members)) {
    same = vapply(members, \(z) identical(z$category_url[1], category_url) &&
        identical(sort(z$retailer_product_id), sort(
          parsed$rows$retailer_product_id)),
      logical(1))
    if (any(same))
    good = F
  }
  list(good = good, empty_verified = empty_verified)
}

catalog_page_status = \(good, empty_verified, parsed) {
  if (good) return("ok")
  if (empty_verified) return("empty_category_verified")
  if (!is.null(parsed) && is.null(parsed$pagination)) return(
      "no_primary_listing")
  "pagination_or_http_failed"
}

catalog_validate_sample = \(members, details, fetch, directory) {
  pool = do.call(rbind, members)
  pool = pool[!duplicated(pool$retailer_product_id), , drop = F]
  select = unique(c(head(which(pool$beverage_family == "beer"), 2), head(
        which(pool$beverage_family ==
            "spirits" & pool$no_lo_status == "alcoholic"), 2), head(which(
          pool$no_lo_status ==
            "alcohol_free_label"), 8)))
  checks = list()
  for (i in select) {
    row = pool[i, , drop = F]
    detail_metadata = fetch(row$product_url, "product",
      row$retailer_product_id)
    if (is.null(detail_metadata) || detail_metadata$response_status != 200L)
      next
    detail_product = catalog_detail(detail_metadata$metadata_file, row)
    details[[row$retailer_product_id]] = detail_product
    checks[[length(checks) + 1L]] = data.frame(product_id =
      row$retailer_product_id,
      name = row$product_name, listing_price = row$price_current,
      detail_price = detail_product$price_current,
      listing_volume = row$volume_l, detail_volume = detail_product$volume_l,
      listing_abv = row$alcohol_content,
      detail_abv = detail_product$alcohol_content, pass = !is.na(
        detail_product$retailer_product_id) &&
        detail_product$retailer_product_id == row$retailer_product_id &&
        !is.na(row$price_current) &&
        !is.na(detail_product$price_current) && abs(row$price_current -
          detail_product$price_current) <
        0.011 && !is.na(row$volume_l) && !is.na(detail_product$volume_l) &&
        abs(row$volume_l -
          detail_product$volume_l) < 1e-04 && (is.na(row$alcohol_content) &&
          is.na(detail_product$alcohol_content) ||
          !is.na(row$alcohol_content) && !is.na(
          detail_product$alcohol_content) && abs(row$alcohol_content -
            detail_product$alcohol_content) < 1e-04))
  }
  validation_checks = if (length(checks))
    do.call(rbind, checks)
  else data.frame()
  write_csv_once(validation_checks, file.path(directory,
      "validation_sample.csv"))
  validated = nrow(validation_checks) >= 10L && all(validation_checks$pass) &&
    any(pool$no_lo_status == "alcoholic") &&
    any(pool$no_lo_status == "alcohol_free_label")
  list(details = details, checks = validation_checks, validated = validated)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) CATALOGUE COLLECTION
# <-----------------------------------------------------------------------------

run_catalog = \(client = new_http_client(pause_seconds = 3, max_tries = 1,
    request_scope =
      "sequential public catalogue observation"),
  max_pages = 3000L, access_terms_sha256 = NULL) {
  assign("SCRAPER_VERSION", "0.2.1", envir = parent.env(environment()),
    inherits = T)
  lock = "data/raw/conalco/.catalog.lock"
  ensure_dir(dirname(lock))
  if (!dir.create(lock, showWarnings = F))
    stop(paste0("Catalog lock exists; check active process ",
        "before recovery"))
  on.exit(unlink(lock, recursive = T), add = T)
  id = new_run_id("catalog")
  dir = run_dir(id)
  ensure_dir(dir)
  log = file.path("logs", paste0(id, ".jsonl"))
  start = list(run_id = id, purpose = "catalog", retailer = "conalco",
    started_at_utc = utc_now(),
    parser_version = CATALOG_VERSION, scraper_version = SCRAPER_VERSION,
    source_snapshot = snapshot_source(dir),
    renv_lock_sha256 = digest(file = "renv.lock", algo = "sha256"),
    pause_seconds = 3,
    extraction_design = paste0(
      "exhaustive reachable family/category listings plus ",
      "targeted product detail validation/enrichment"
  ))
  write_json_once(start, file.path(dir, "started.json"))
  cat("RUN", id, "\n")
  flush.console()
  request_state = new.env(parent = emptyenv())
  request_state$index = 0L
  request_state$requests = list()
  request_state$stopped = F
  request_state$stop_reason = NA_character_
  request_state$robots_text = ""
  fetch = \(url, type, product_id = NA_character_) {
    if (request_state$stopped)
      return(NULL)
    if (!robots_allowed(request_state$robots_text, url)) {
      request_state$stopped = T
      request_state$stop_reason = paste("robots_disallowed", url)
      return(NULL)
    }
    request_state$index = request_state$index + 1L
    metadata = archive_response(client(url), id, type, request_state$index,
      product_id)
    request_state$requests[[length(request_state$requests) + 1L]] = metadata
    log_event(log, "http_response", url = url, response_status =
      metadata$response_status,
      error = metadata$error, metadata_file = metadata$metadata_file)
    if (is.na(metadata$response_status) || metadata$response_status %in% c(401,
        403, 429) ||
        metadata$response_status >=
        500L) {
      request_state$stopped = T
      request_state$stop_reason = paste("access_or_transport_stop",
        metadata$response_status, url)
    }
    metadata
  }
  ## Access checks <------------------------------------------------------

  robots = fetch("https://www.conalco.de/robots.txt", "robots")
  if (is.null(robots) || is.na(robots$response_status) ||
      !robots$response_status %in%
      c(200, 404, 410))
    stop("Robots check failed; archived response retained")
  if (robots$response_status == 200)
  request_state$robots_text = rawToChar(read_archive(robots$metadata_file)$body)
  delay_values = regmatches(request_state$robots_text, gregexpr(
      "(?im)^crawl-delay:\\s*[0-9]+(?:[.][0-9]+)?",
      request_state$robots_text, perl = T))[[1]]
  if (length(delay_values)) {
    delay = max(3, vapply(delay_values, parse_number, numeric(1)), na.rm = T)
    if (delay > 3) {
      Sys.sleep(delay)
      client = new_http_client(pause_seconds = delay, max_tries = 1,
        request_scope =
          "sequential public catalogue observation")
      log_event(log, "robots_crawl_delay_respected", pause_seconds = delay)
    }
  }
  terms = fetch("https://www.conalco.de/agb", "terms")
  if (is.null(terms) || terms$response_status != 200L)
    stop("Terms retrieval failed")
  terms_text = node_text(read_html(terms$raw_file), "//main")
  if (!is.null(access_terms_sha256) && !identical(digest(terms_text, algo =
        "sha256",
        serialize = F), access_terms_sha256))
    stop(paste0(
        "Public terms changed since the verified access review; ",
        "stop for review before catalogue requests"
    ))
  terms_matches = regmatches(terms_text,
    gregexpr(paste0(".{0,120}(?:scrap|crawl|robot|automatisiert|",
        "datenbank).{0,120}"),
      terms_text, ignore.case = T, perl = T))[[1]]
  write_json_once(list(robots_status = robots$response_status, terms_metadata =
      terms$metadata_file,
      review = paste0(
        "Reviewed public purchase terms; no explicit scraping ",
        "prohibition identified. Missing robots is not ",
        "affirmative permission."
      ),
      matching_terms_passages = terms_matches), file.path(dir,
      "access_review.json"))
  ## Category discovery <-------------------------------------------------

  queue = catalog_seeds()
  done = character()
  pages = list()
  members = list()
  links = list()
  details = list()
  validated = F

  # The first four seeds provide fresh beer, No/Lo and spirits before expansion.
  while (nrow(queue) && !request_state$stopped) {
    category_row = queue[1, , drop = F]
    queue = queue[-1, , drop = F]
    category_url = category_row$category_url
    category_name = category_row$category_name
    if (category_url %in% done)
      next
    done = c(done, category_url)
    metadata = fetch(category_url, "category")
    parsed = catalog_process_page(metadata, category_name, category_url, 1L)
    if (is.null(parsed)) {
      pages[[length(pages) + 1L]] = data.frame(category_name =
        category_name, category_url = category_url,
        page = 1L, expected_last = NA_integer_, n_cards = 0L, status =
        "http_failed",
        metadata_file = if (is.null(metadata))
          NA_character_
        else metadata$metadata_file)
      next
    }
    category_links = catalog_links(parsed$doc)
    if (nrow(category_links)) {
      category_links$discovered_on = category_url
      links[[length(links) + 1L]] = category_links
      add = category_links[category_links$decision == "crawl" &
          !category_links$category_url %in% c(done,
          queue$category_url),
        c("category_name", "category_url"), drop = F]
      queue = rbind(queue, add)
    }
    last_page = if (is.null(parsed$pagination))
      1L
    else parsed$pagination$last
    for (page in seq_len(last_page)) {
      if (page > 1L) {
        metadata = fetch(paste0(category_url, "?p=", page), "category")
        parsed = catalog_process_page(metadata, category_name,
          category_url, page)
      }
      page_checks = catalog_page_checks(parsed, page, last_page, members,
        category_url)
      good = page_checks$good
      empty_verified = page_checks$empty_verified
      pages[[length(pages) + 1L]] = data.frame(category_name =
        category_name,
        category_url = category_url,
        page = page, expected_last = last_page, n_cards = if (is.null(
            parsed))
          0L
        else nrow(parsed$rows), status = catalog_page_status(good,
          empty_verified, parsed),
        metadata_file = if (is.null(metadata))
          NA_character_
        else metadata$metadata_file)
      if (!is.null(parsed) && nrow(parsed$rows))
      members[[length(members) + 1L]] = parsed$rows
      if (length(pages) >= max_pages) {
        request_state$stopped = T
        request_state$stop_reason = "safety_page_limit"
        break
      }
      if (request_state$stopped)
        break
    }
    cat("CATEGORY", length(done), category_name, "pages", last_page,
      "discovered categories", length(done) +
        nrow(queue), "\n")
    flush.console()
    if (!validated && length(done) == 4L) {
      validation = catalog_validate_sample(members, details, fetch, dir)
      details = validation$details
      validated = validation$validated
      if (!validated) {
        request_state$stopped = T
        request_state$stop_reason = "validation_sample_failed"
      }
      cat("VALIDATION", validated, "checks", nrow(validation$checks), "\n")
      flush.console()
    }
  }
  ## Freeze discovery and publish <---------------------------------------

  # Freeze discovery before parsing/consolidating. No past run enters this
  # export.
  page_table = do.call(rbind, pages)
  link_table = if (length(links))
    do.call(rbind, links)
  else data.frame()
  write_csv_once(page_table, file.path(dir, "category_pages.csv"))
  write_csv_once(link_table, file.path(dir, "category_links.csv"))
  write_csv_once(queue, file.path(dir, "pending_categories.csv"))
  all = if (length(members))
    do.call(rbind, members)
  else data.frame()
  if (!nrow(all))
    stop("No product cards; archive retained")
  write_csv_once(all, file.path(dir, "listing_observations.csv"))
  write_json_once(list(run_id = id, requests = request_state$requests,
      stop_reason = request_state$stop_reason,
      validation_passed = validated), file.path(dir, "discovery.json"))
  cat("DISCOVERY FINISHED", length(unique(all$retailer_product_id)),
    "products", nrow(all),
    "memberships\n")
  flush.console()
  catalog_finalize(id, all, details, page_table, link_table, queue,
    request_state$requests,
    request_state$stop_reason,
    validated, fetch)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) PRODUCT CONSOLIDATION AND EXPORT
# <-----------------------------------------------------------------------------

catalog_finalize = \(id, all, details, pages, links, pending, requests,
  stop_reason,
  validated, fetch = NULL, output_dir = NULL, manifest_file = NULL,
  derivation_source_snapshot = NULL) {
  raw_directory = run_dir(id)
  output_directory = if (is.null(output_dir))
    file.path("data/processed/catalog", id)
  else output_dir
  ensure_dir(output_directory)
  groups = split(seq_len(nrow(all)), all$retailer_product_id)
  join = \(values) paste(unique(values[!is.na(values) & nzchar(values)]),
    collapse = " | ")
  # Keep every original category membership in a separate long table as well.
  memberships = unique(all[c("retailer_product_id", "retailer_category",
        "category_url",
        "category_page", "listing_metadata_file", "observed_at")])
  write_csv_once(memberships, file.path(output_directory,
      "category_memberships.csv"))
  rows = lapply(groups, \(detail_indices) {
      product_rows = all[detail_indices, , drop = F]
      best = order(product_rows$parse_status != "ok", is.na(
          product_rows$alcohol_content),
        product_rows$observed_at)[1]
      product = product_rows[best, , drop = F]
      product$retailer_category = join(product_rows$retailer_category)
      product$category_urls = join(product_rows$category_url)
      product$listing_metadata_files = join(product_rows$listing_metadata_file)
      product$listing_observations_n = nrow(product_rows)
      product$observed_listing_at = product$observed_at
      # Categories provide only supporting evidence; reclassify after
      # consolidation.
      classification = catalog_classify(product$product_name,
        product$designation_raw,
        product$retailer_category,
        product$alcohol_content)
      for (field in names(classification)) product[[field]] =
        classification[[field]]
      if (length(unique(product_rows$price_current[!is.na(
                product_rows$price_current)])) > 1)
      product$validation_flags = add_flag(product$validation_flags,
        "listing_price_changed_within_run")
      product
  })
  products = do.call(rbind, rows)
  rownames(products) = NULL
  # Target missing exact No/Lo alcohol information and missing core fields.
  # Retain unclear assignments, and inspect a bounded, reproducible sample of
  # them.
  relevant_indices = which(products$scope_status !=
      "excluded_nonfamily_product")
  no_lo = products$no_lo_status %in% c("alcohol_free_label",
    "reduced_alcohol_label", "zero_numeric",
    "low_numeric_threshold_1.2", "no_lo_candidate_category")
  core_missing = which(products$parse_status != "ok" & products$scope_status !=
      "excluded_nonfamily_product")
  no_lo_missing = which(no_lo & is.na(products$alcohol_content) &
      products$scope_status != "excluded_nonfamily_product")
  unclear_qc = head(which(products$scope_status == "unclear_review"), 12L)
  family_qc = unlist(lapply(split(relevant_indices, products$beverage_family[
          relevant_indices]), head,
      2L))
  detail_indices = unique(c(core_missing, no_lo_missing, unclear_qc, family_qc))
  detail_indices = setdiff(detail_indices, which(
      products$retailer_product_id %in% names(details)))
  for (k in seq_along(detail_indices)) {
    i = detail_indices[k]
    if (is.null(fetch))
      break
    m = fetch(products$product_url[i], "product",
      products$retailer_product_id[i])
    if (is.null(m))
      break
    if (m$response_status == 200L)
    details[[products$retailer_product_id[i]]] = catalog_detail(
      m$metadata_file, products[i,
        , drop = F])
    cat("DETAIL", k, "/", length(detail_indices), products$product_name[i],
      "HTTP", m$response_status,
      "\n")
    flush.console()
  }
  for (i in seq_len(nrow(products))) {
    detail_product = details[[products$retailer_product_id[i]]]
    if (is.null(detail_product))
      next
    for (field in names(detail_product)) if (!field %in% c(
          "retailer_category",
          "category_url", "category_page",
          "listing_metadata_file", "validation_flags"))
      products[i, field] = detail_product[[field]]
    categories = trimws(unlist(strsplit(c(products$retailer_category[i],
            detail_product$retailer_category),
          "\\s*[|]\\s*", perl = T)))
    products$retailer_category[i] = join(categories)
    products$validation_flags[i] = add_flag(products$validation_flags[i],
      detail_product$validation_flags)
  }
  products$beverage_group = products$beverage_family
  products = products %>% classify_products() %>% quality_checks()
  products$catalog_coverage_status = "reachable_categories_only_not_entire_shop"
  for (i in which(products$scope_status ==
        "unclear_review")) products$validation_flags[i] =
    add_flag(products$validation_flags[i],
    "beverage_assignment_unclear")
  for (i in which(products$no_lo_status ==
        "no_lo_candidate_category")) products$validation_flags[i] =
    add_flag(products$validation_flags[i],
    "no_lo_category_evidence_only")
  for (i in which(products$no_lo_status == "alcohol_free_label" & !is.na(
        products$alcohol_content) &
        products$alcohol_content > 1.2)) products$validation_flags[i] =
    add_flag(products$validation_flags[i],
    "alcohol_free_label_abv_conflict")
  for (i in which(is.na(products$brand))) products$validation_flags[i] =
    add_flag(products$validation_flags[i],
    "brand_not_extracted_from_listing")
  # Failed detail fetches never erase successfully captured listing
  # observations.
  relevant = products[products$scope_status != "excluded_nonfamily_product", ,
    drop = F]
  stopifnot(!anyDuplicated(relevant$retailer_product_id))
  write_csv_once(relevant, file.path(output_directory,
      "products_INCOMPLETE.csv"))
  write_csv_once(products[products$scope_status ==
        "excluded_nonfamily_product", , drop = F],
    file.path(output_directory, "excluded_products.csv"))
  sample_ix = unique(c(unlist(lapply(split(seq_len(nrow(relevant)),
            relevant$beverage_family),
          head, 2)), head(which(relevant$no_lo_status %in% c(
            "alcohol_free_label", "zero_numeric",
            "reduced_alcohol_label", "low_numeric_threshold_1.2")), 10)))
  cols = c("retailer_product_id", "ean_gtin", "product_name", "brand",
    "retailer_category",
    "beverage_family", "scope_status", "no_lo_status", "alcohol_content",
    "alcohol_content_raw",
    "volume_l", "pack_count", "price_current", "price_reference",
    "price_per_l_displayed",
    "deposit", "promo", "availability", "observed_at", "source_format",
    "validation_flags")
  write_csv_once(relevant[sample_ix, cols, drop = F], file.path(
      output_directory, "examples.csv"))
  write_csv_once(as.data.frame(table(relevant$beverage_family)), file.path(
      output_directory,
      "family_counts.csv"))
  fields = c("ean_gtin", "brand", "sku", "alcohol_content", "volume_l",
    "pack_count",
    "price_current", "price_reference", "price_per_l_displayed", "deposit",
    "promo",
    "stock_quantity")
  missing = data.frame(field = fields, n_missing = vapply(relevant[fields],
      \(values) sum(is.na(values)),
      integer(1)))
  write_csv_once(missing, file.path(output_directory, "missing_fields.csv"))
  coverage = do.call(rbind, lapply(split(seq_len(nrow(pages)),
        pages$category_url),
      \(detail_indices) {
        category_pages = pages[detail_indices, , drop = F]
        category_url = category_pages$category_url[1]
        data.frame(category_name = category_pages$category_name[1],
          category_url = category_url,
          expected_pages = max(category_pages$expected_last,
            na.rm = T), pages_requested = nrow(category_pages),
          pages_verified = sum(category_pages$status ==
              "ok"), unique_products = length(unique(all$retailer_product_id[
                all$category_url ==
                  category_url])), coverage = if (all(
              category_pages$status %in% c("ok", "empty_category_verified")) &&
              nrow(category_pages) == max(category_pages$expected_last, na.rm =
                T))
            "complete_observed_pagination"
          else if (all(category_pages$status == "no_primary_listing"))
              "landing_or_editorial_no_primary_listing"
            else "incomplete")
  }))
  write_csv_once(coverage, file.path(output_directory,
      "coverage_by_category.csv"))
  source = read_json(file.path(raw_directory, "started.json"),
    simplifyVector = F)
  # Recover all requests (including detail fetches) from immutable sidecars.
  metas = list.files(raw_directory, pattern = "^[0-9]+_.*[.]json$",
    full.names = T, recursive = T)
  requests = lapply(metas, read_json, simplifyVector = F)
  failures = Filter(\(m) is.null(m$response_status) || if (m$request_type ==
        "robots")
      !m$response_status %in% c(200L, 404L, 410L)
    else !m$response_status %in% 200:299, requests)
  write_json_once(failures, file.path(output_directory,
      "request_failures.json"))
  summary = list(n_discovered_unique_products = nrow(products),
    n_relevant_products = nrow(relevant),
    n_excluded = nrow(products) - nrow(relevant), n_unclear = sum(
      relevant$scope_status ==
        "unclear_review"), n_no_lo = sum(relevant$no_lo_status %in% c(
        "alcohol_free_label",
        "reduced_alcohol_label", "zero_numeric", "low_numeric_threshold_1.2")),
    n_no_lo_candidates =
    sum(relevant$no_lo_status ==
        "no_lo_candidate_category"), n_exact_zero = sum(
      relevant$alcohol_content ==
        0, na.rm = T), n_missing_abv = sum(is.na(relevant$alcohol_content)),
    n_detail_observations =
    sum(!is.na(relevant$detail_metadata_file)),
    n_listing_only = sum(is.na(relevant$detail_metadata_file)),
    n_unknown_availability =
    sum(relevant$availability ==
        "unknown"), n_partial_parses = sum(relevant$parse_status == "partial"),
    n_missing_price =
    sum(is.na(relevant$price_current)),
    n_missing_volume = sum(is.na(relevant$volume_l)), n_invalid_gtin = sum(
      grepl("(?:^|;)invalid_gtin(?:;|$)",
        relevant$validation_flags, perl = T)), n_category_pages = nrow(pages),
    n_category_pages_ok = sum(pages$status == "ok"), n_category_pages_failed =
    sum(pages$status %in%
        c("http_failed", "pagination_or_http_failed")), n_no_listing_pages =
    sum(pages$status ==
        "no_primary_listing"), n_pending_categories = nrow(pending),
    n_request_failures = length(failures),
    duplicate_observations = sum(duplicated(relevant$retailer_product_id)))
  files = list.files(output_directory, full.names = T)
  hashes = as.list(setNames(vapply(files, \(path) digest(file = path, algo =
            "sha256"),
        character(1)), basename(files)))
  source$parser_version = CATALOG_VERSION
  manifest = c(source, list(ended_at_utc = utc_now(), status = "partial",
      catalog_coverage_status =
      "incomplete_or_unproven",
      derivation_source_snapshot = derivation_source_snapshot,
      validation_passed = validated,
      stop_reason = stop_reason, summary = summary, exports = hashes,
      observation_file =
      file.path(output_directory,
        "products_INCOMPLETE.csv"), requests = requests))
  if (is.null(manifest_file))
  manifest_file = file.path(raw_directory, "manifest.json")
  write_json_once(manifest, manifest_file)
  write_json_once(summary, file.path(output_directory, "quality_summary.json"))
  catalog_write_quality_report(id, source, manifest, summary, relevant,
    missing, stop_reason, output_directory)
  cat("FINISHED", id, "products", nrow(relevant), "\nEXPORT", file.path(
      output_directory,
      "products_INCOMPLETE.csv"),
    "\n")
  invisible(manifest)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) IMMUTABLE QUALITY REPORT
# <-----------------------------------------------------------------------------

catalog_write_quality_report = \(id, source, manifest, summary, relevant,
  missing,
  stop_reason, output_directory) {
  md_table = \(values) c(paste0("| ", paste(names(values), collapse = " | "),
      " |"), paste0("| ",
      paste(rep("---", ncol(values)), collapse = " | "), " |"), apply(values,
      1, \(v) paste0("| ",
        paste(v, collapse = " | "), " |")))
  report = c("# Conalco: neuer Erhebungslauf", paste("Run-ID:", id), paste(
      "Tatsächliche Abrufe (UTC):",
      source$started_at_utc, "bis", manifest$ended_at_utc), "", paste0(
      "**Status: UNVOLLSTÄNDIG / vollständige ",
      "Gesamtkatalogabdeckung nicht bewiesen.**"
    ),
    paste0(
      "Alle Beobachtungen stammen aus neuen HTTP-Antworten ",
      "dieses Laufs. Keine Pilotdaten vom 7. Oktober und ",
      "keine Fixtures wurden verwendet."
    ),
    "", paste("Eindeutige erfasste Produkte:",
      summary$n_discovered_unique_products),
    paste("Exportierte relevante bzw. unklare Produkte:",
      summary$n_relevant_products),
    paste("No/Lo:", summary$n_no_lo, "; belegte numerische Nullwerte:",
      summary$n_exact_zero,
      "; fehlender/nicht exakt bestimmbarer Alkoholgehalt:",
      summary$n_missing_abv),
    paste(paste0("Zusätzliche No/Lo-Kandidaten ausschließlich ",
        "aufgrund der Kategoriezuordnung:"),
      summary$n_no_lo_candidates), paste(
      "Ausgeschlossene Nicht-Getränkefamilien:",
      summary$n_excluded,
      "; zur fachlichen Prüfung erhaltene unklare Produkte:",
      summary$n_unclear), paste("Kategorie-/Katalogseiten:",
      summary$n_category_pages,
      "; erfolgreich geprüfte Listingseiten:", summary$n_category_pages_ok,
      "; Fehler:",
      summary$n_category_pages_failed, "; Seiten ohne Primärlisting:",
      summary$n_no_listing_pages),
    paste("Offene Kategorien:", summary$n_pending_categories,
      "; HTTP-Fehler:", summary$n_request_failures,
      "; doppelte Produktbeobachtungen:", summary$duplicate_observations), "",
    "## Datenqualität und Interpretation",
    paste("Produktdetail-Beobachtungen:", summary$n_detail_observations,
      paste0("; ausschließlich auf Katalogkarten beruhende ", "Beobachtungen:"),
      summary$n_listing_only), paste("Unbekannte Verfügbarkeit:",
      summary$n_unknown_availability,
      "; teilweise geparste Produkte:", summary$n_partial_parses,
      "; fehlender Preis:",
      summary$n_missing_price, "; fehlendes Volumen:", summary$n_missing_volume,
      "; auffällige GTIN:", summary$n_invalid_gtin), paste0(
      "Die Katalogkarten enthalten Preise, Inhalt und ",
      "ausführliche Produktbeschreibungen. Marke, SKU, ",
      "Lagerbestand und Verfügbarkeit sind dort vielfach ",
      "nicht explizit enthalten. Diese Werte bleiben NA bzw. ",
      "unknown; ein Warenkorbbutton wird nicht als ",
      "Verfügbarkeitsmessung interpretiert."
    ),
    paste0(
      "Frische Detailseiten validieren zunächst Bier, ",
      "Spirituosen und mehrere No/Lo-Produkte. No/Lo und ",
      "unklare/fehlerhafte Karten werden anschließend gezielt ",
      "über Detailseiten ergänzt. Dieses Verfahren ist keine ",
      "vollständige Detailseitenerhebung."
    ),
    paste0(
      "alcohol_content ist eine exakte numerische Angabe in ",
      "Vol.-%. Grenzen und ungefähre Angaben bleiben NA; ",
      "alcohol_content_raw und alcohol_content_source ",
      "dokumentieren die Originalangabe. Ein Prozentwert im ",
      "Namen ist als Quelle ausgewiesen. Alkoholfrei allein ",
      "wird nie als 0 interpretiert."
    ),
    paste0(
      "No/Lo beruht auf produktbezogenen Bezeichnungen bzw. ",
      "explizitem ABV. low_numeric_threshold_1.2 ist eine ",
      "transparente analytische Suchschwelle, keine ",
      "rechtliche Getränkeklassifikation."
    ),
    paste0(
      "Vergleichspreis, Pfand und Promotions können ",
      "strukturell fehlen. Fehlendes Pfand ist NA. ",
      "Abweichende Preise in mehreren Kategorieantworten ",
      "werden gekennzeichnet; unterschiedliche Abrufzeiten ",
      "sind keine gleichzeitige Momentaufnahme."
    ),
    paste0(
      "Kategoriezuordnungen bleiben in ",
      "category_memberships.csv mit URL, Seite und ",
      "Quellenzeitpunkt erhalten. Vollständigkeit bezieht ",
      "sich nur auf den beobachteten öffentlichen ",
      "Kategoriebaum; versteckte/verwaiste Produkte und nicht ",
      "abgefragte Marken-/Zubehörbäume sind nicht ",
      "ausgeschlossen."
    ),
    "", "## Getränkefamilien", md_table(as.data.frame(table(
          relevant$beverage_family))),
    "", "## Fehlende Angaben", md_table(missing), "",
    "## Offene technische Punkte",
    paste("Stopgrund:", if (is.na(
          stop_reason)) "kein Abbruch während Discovery" else stop_reason),
    paste0(
      "Gemäß der präzisierten Nutzeranfrage beruht die ",
      "Erhebung auf dem vollständigen erreichbaren ",
      "Kartenkatalog mit gezielten Detailprüfungen. Eine ",
      "externe Vollständigkeitsreferenz (Händlerfeed) liegt ",
      "nicht vor. Klassifikationen unklarer Produkte und ",
      "Mischpakete benötigen fachliche Prüfung."
    ),
    paste0(
      "robots.txt und AGB wurden neu archiviert. Keine ",
      "Schutzmaßnahmen wurden umgangen. Die fehlende ",
      "Robots-Datei ist keine ausdrückliche Erlaubnis für ",
      "Dauerbetrieb."
    ),
    paste0(
      "Täglicher Betrieb wird erst nach geprüftem ",
      "institutionellen Zugangsrahmen, Backup und dauerhaft ",
      "verlässlicher Speicherung freigeschaltet. Lokales ",
      "unveränderliches Archiv allein ist noch kein extern ",
      "gesichertes Backup."
    ),
    "", "## Dateien", paste0(
      "products_INCOMPLETE.csv: alle erfolgreich erfassten ",
      "relevanten und unklaren Produkte."
    ),
    paste0(
      "examples.csv: kompakter fachlicher Beispielauszug; ",
      "family_counts.csv und missing_fields.csv: ",
      "Qualitätszähler."
    ),
    paste0(
      "excluded_products.csv: nachvollziehbare Ausschlüsse; ",
      "category_memberships.csv: ursprüngliche Zuordnungen; ",
      "coverage_by_category.csv: Abdeckung und Produktzahl ",
      "jeder geprüften Kategorie."
    ),
    paste0(
      "Roharchiv: category_pages.csv, category_links.csv, ",
      "pending_categories.csv, validation_sample.csv, ",
      "discovery.json, Response-Bodies und Metadaten."
  ))
  write_once(file.path(output_directory, "quality_report.md"),
    \(path) writeLines(report,
      path,
      useBytes = T))
}
