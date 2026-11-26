# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261007

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) VALIDATE
# <-----------------------------------------------------------------------------

valid_gtin = \(x) {
  if (is.na(x) || grepl("^0+$", x) || !grepl("^[0-9]+$", x) || !nchar(x) %in%
      c(8L,
      12L, 13L, 14L))
    return(F)
  digits = as.integer(strsplit(x, "", fixed = T)[[1]])
  base = rev(head(digits, -1))
  weights = rep(c(3L, 1L), length.out = length(base))
  ((10L - sum(base * weights)%%10L)%%10L) == tail(digits, 1)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) OBSERVATION PLAUSIBILITY
# <-----------------------------------------------------------------------------

validate_observations = \(x, previous = NULL, planned = nrow(x)) {
  if (!nrow(x))
    return(x)
  x$validation_flags[is.na(x$validation_flags)] = ""
  flag = \(products, mask, name) {
    ix = which(!is.na(mask) & mask)
    for (i in ix) products$validation_flags[i] = add_flag(
      products$validation_flags[i], name)
    products
  }
  missing_id = is.na(x$retailer_product_id) | !nzchar(x$retailer_product_id)
  x = flag(x, missing_id, "missing_product_identifier")
  identity = paste(x$run_id, x$retailer, x$retailer_product_id, sep = ":")
  dup = !missing_id & (duplicated(identity) | duplicated(identity, fromLast =
        T))
  x = flag(x, dup, "duplicate_product_identifier")
  x = flag(x, is.na(x$price_current), "missing_price")
  x = flag(x, !is.na(x$price_current) & (!is.finite(x$price_current) |
        x$price_current <= 0),
    "nonpositive_or_invalid_price")
  x = flag(x, is.na(x$volume_l), "missing_volume")
  x = flag(x, !is.na(x$volume_l) & (!is.finite(x$volume_l) | x$volume_l <=
        0 | x$volume_l >
        100), "implausible_volume")
  x = flag(x, !is.na(x$pack_count) & (x$pack_count < 1 | x$pack_count >
        1000 | x$pack_count !=
        floor(x$pack_count)), "implausible_pack_count")
  x = flag(x, is.na(x$abv), "missing_abv")
  x = flag(x, !is.na(x$abv) & (!is.finite(x$abv) | x$abv < 0 | x$abv > 100),
    "implausible_abv")
  x = flag(x, !is.na(x$price_per_l) & (!is.finite(x$price_per_l) |
        x$price_per_l < 0.1 | x$price_per_l >
        50000), "implausible_litre_price")
  x = flag(x, !is.na(x$price_per_l_displayed) & (!is.finite(
        x$price_per_l_displayed) |
        x$price_per_l_displayed <=
        0 | x$price_per_l_displayed > 50000),
    "implausible_displayed_litre_price")
  x = flag(x, !is.na(x$deposit) & (!is.finite(x$deposit) | x$deposit < 0),
    "negative_or_invalid_deposit")
  x = flag(x, !is.na(x$price_reference) & (!is.finite(x$price_reference) |
        x$price_reference <=
        0), "implausible_reference_price")
  x = flag(x, is.na(x$deposit), "deposit_not_displayed")
  x = flag(x, is.na(x$ean_gtin), "gtin_not_available")
  invalid_gtin = vapply(x$ean_gtin, \(g) !is.na(g) && !valid_gtin(g),
    logical(1))
  x = flag(x, invalid_gtin, "invalid_gtin")
  x = flag(x, x$availability == "unknown", "availability_unknown")
  x = flag(x, x$parse_status == "error", "parser_error")
  x = flag(x, x$parse_status == "http_error", "request_failed")
  x = flag(x, !is.na(x$currency) & x$currency != "EUR", "unexpected_currency")
  gp = abs(x$price_per_l - x$price_per_l_displayed) > pmax(0.03, abs(
      x$price_per_l) *
      0.005)
  x = flag(x, gp, "displayed_computed_litre_price_disagree")
  found = length(unique(x$retailer_product_id[!missing_id]))
  if (found < max(1L, ceiling(planned * 0.8)))
  x = flag(x, rep(T, nrow(x)), "unexpectedly_small_product_count")
  critical_fields = c("retailer_product_id", "price_current", "volume_l")
  missing_share = mean(vapply(x[critical_fields], \(v) mean(is.na(v)),
      numeric(1)))
  if (missing_share > 0.5)
  x = flag(x, rep(T, nrow(x)), "extreme_missing_core_fields")
  if (!is.null(previous) && nrow(previous)) {
    previous = previous[!is.na(previous$retailer_product_id), , drop = F]
    previous = previous[order(previous$observed_at, decreasing = T), ,
      drop = F]
    previous = previous[!duplicated(previous$retailer_product_id), , drop = F]
    j = match(x$retailer_product_id, previous$retailer_product_id)
    for (n in c("volume_l", "abv", "pack_count", "unit_volume_l")) {
      delta = abs(x[[n]] - previous[[n]][j]) > 1e-06
      x = flag(x, delta, paste0("attribute_changed_", n))
    }
    x = flag(x, !is.na(x$ean_gtin) & !is.na(previous$ean_gtin[j]) &
        x$ean_gtin != previous$ean_gtin[j],
      "attribute_changed_gtin")
  }
  x
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) RUN STATUS
# <-----------------------------------------------------------------------------

summarize_run = \(x, planned, responses) {
  http_status = vapply(responses, \(r) if (is.null(r$response_status) ||
        is.na(r$response_status))
      NA_real_
    else as.numeric(r$response_status), numeric(1))
  success = sum(!is.na(http_status) & http_status %in% 200:299)
  ids = x$retailer_product_id[!is.na(x$retailer_product_id) & nzchar(
      x$retailer_product_id)]
  critical_pattern = paste(c("duplicate_product_identifier",
      "nonpositive_or_invalid_price",
      "implausible_volume", "implausible_abv", "negative_or_invalid_deposit",
      "requested_returned_identifier_mismatch",
      "structured_visible_price_disagree"), collapse = "|")
  critical = if (nrow(x))
    any(grepl(critical_pattern, x$validation_flags))
  else T
  complete = planned > 0 && nrow(x) == planned && success == planned && all(
    x$parse_status ==
      "ok") && length(unique(ids)) == planned && !critical
  list(n_planned_products = planned, n_successful_http = success,
    n_failed_requests = planned -
      success, n_successfully_parsed = sum(x$parse_status %in% c("ok",
        "partial")),
    n_parser_errors = sum(x$parse_status == "error"), n_partial_parses = sum(
      x$parse_status ==
        "partial"), n_unique_products = length(unique(ids)),
    n_without_price = sum(is.na(x$price_current)),
    n_without_identifier = sum(is.na(x$retailer_product_id) | !nzchar(
        x$retailer_product_id)),
    n_flagged_products = sum(nzchar(x$validation_flags)), status = if (
      complete) "complete" else if (success ==
          0 || !any(x$parse_status %in% c("ok",
            "partial"))) "failed" else "partial")
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) COMPLETE RECORD AUDIT AND EXPLICIT CHANGE REASONS
# <-----------------------------------------------------------------------------

# Merged on 2026-10-19 from R/product_audit.R (file created 2026-10-15);
# content unchanged
AUDIT_VERSION = 'product-audit-0.1.0'
AUDIT_DIRECTORY = 'data/processed/audit_2026-10-15'
AUDIT_RUNS = c('catalog_20261009T134341_ab5a3026fb34',
  'catalog_20261012T092338_c2109ed57d1d')
AUDIT_ALDI_RUN = 'aldi_probe_20261012T103804_49f4a7be39cf'

read_historical_derivation = \(directory, filename = 'all_products.csv') {
  manifest = read_json(file.path(directory, 'manifest.json'))
  for (entry in c(manifest$inputs, manifest$outputs)) stopifnot(identical(
        digest(file = entry$path, algo = 'sha256'), entry$sha256))
  stopifnot(identical(digest(file = manifest$source_snapshot$path, algo =
          'sha256'),
      manifest$source_snapshot$sha256))
  read_observations(file.path(directory, filename))
}

audit_record_changes = \(before, after) {
  stopifnot(setequal(before$observation_key, after$observation_key),
    !anyDuplicated(after$observation_key))
  before = before[match(after$observation_key, before$observation_key), ]
  fields = c('beverage_group', 'scope_status', 'no_lo_status', 'pack_count',
    'unit_volume_l',
    'pack_basis', 'volume_l', 'alcohol_content', 'price_current',
    'price_reference',
    'price_regular', 'deposit', 'brand', 'ean_gtin', 'availability')
  changes = bind_rows(lapply(fields, \(field) {
        a = as.character(before[[field]])
        b = as.character(after[[field]])
        rows = which(coalesce(a, '') != coalesce(b, ''))
        if (!length(rows)) return(NULL)
        after[rows, c('observation_key', 'product_key', 'retailer_product_id',
            'run_id',
            'retailer', 'product_name', 'classification_rule',
            'scope_rule')] %>%
          mutate(field = field, previous_value = a[rows], corrected_value =
            b[rows])
  }))
  changes %>% mutate(reason = case_when(
      field %in% c('pack_count', 'unit_volume_l', 'pack_basis') ~
        paste0("Explicit year, ageing, brand, mixer or ",
        "outer-box text is not a multipack; archived ",
        "displayed content retained"),
      scope_rule == 'S03_ingredient_food' ~
        paste0("Labelled cocktail aroma or aromatic bitter is a ",
        "dosing ingredient, not a finished beverage"),
      scope_rule == 'S05_softdrink_mixer' ~
        paste0("Explicit Erfrischungsgetränk designation ",
        "resolves the former Verjus review case"),
      classification_rule == 'X01' ~
        paste0("Explicit ready-to-drink/Pisco cocktail/Coctel ",
        "name overrides generic spirit evidence"),
      classification_rule == 'X02' ~
        paste0("Pommeau is fruit must plus distillate; separate ",
        "descriptive family, customs group remains ", "unresolved"),
      classification_rule == 'D03' ~ paste0(
        "Explicit (apple) must designation establishes ",
        "the fermented fruit-drink family"),
      classification_rule == 'D08' ~ paste0(
        "Explicit bitter-with-soda or alcoholic hot ", "drink is a mixture"),
      classification_rule == 'D09' ~ paste0(
        "Explicit named fruit wine resolves the ", "previously unknown family"),
      classification_rule == 'D01' ~ paste0(
        "Explicit agricultural ethyl alcohol designation ",
        "supports the spirits family"),
      T ~ paste('Current evidence rule', classification_rule, scope_rule)
  ))
}

audit_all_classifications = \(products) {
  x = products
  matched = lapply(seq_len(nrow(classification_rules)), \(i) {
      rule = classification_rules[i, ]
      values = tolower(coalesce(x[[rule$source]], ''))
      grepl(rule$pattern, values, perl = T)
  })
  matches = do.call(cbind, matched)
  rules = vapply(seq_len(nrow(x)), \(i)
    paste(classification_rules$rule_id[matches[i, ]], collapse = '|'),
    character(1))
  fresh = classify_products(x)
  same = x$beverage_group == fresh$beverage_group & x$scope_status ==
    fresh$scope_status &
    x$no_lo_status == fresh$no_lo_status & x$classification_rule ==
    fresh$classification_rule
  stopifnot(all(same), !anyDuplicated(x$observation_key))
  x %>% select(observation_key, product_key, retailer_product_id, run_id,
    retailer,
    product_name, designation_raw, retailer_category, beverage_group,
    scope_status,
    no_lo_status, classification_rule, classification_source,
    classification_evidence,
    classification_conflict, analysis_category, analysis_category_rule,
    tax_review_reason) %>%
    mutate(all_matching_rules_in_priority_order = rules,
    current_code_consistent = same,
    disposition = case_when(
      scope_status == 'unclear_review' ~ 'unresolved_retained',
      !is.na(tax_review_reason) ~ 'family_supported_tax_review_separate',
      scope_status == 'excluded_nonfamily_product' ~ 'excluded_with_evidence',
      T ~ 'family_supported_by_recorded_evidence'
  ))
}

audit_field_completeness = \(products, state) {
  fields = c('retailer_product_id', 'ean_gtin', 'ean_gtin_all', 'brand',
    'product_name',
    'retailer_category', 'beverage_group', 'price_current', 'price_regular',
    'price_reference',
    'promo', 'alcohol_content', 'alcohol_content_raw', 'unit_volume_l',
    'pack_count',
    'volume_l', 'packaging_type', 'deposit', 'availability', 'retailer',
    'observed_at')
  bind_rows(lapply(split(products, products$run_id), \(x) {
        bind_rows(lapply(fields, \(field) {
              values = x[[field]]
              if (is.null(values)) values = rep(NA_character_, nrow(x))
              tibble(state = state, run_id = x$run_id[1], retailer =
                x$retailer[1], field = field,
                n_products = nrow(x), n_missing = sum(is.na(values) |
                    as.character(values) == ''),
                n_unknown = sum(as.character(values) == 'unknown', na.rm = T))
        }))
  }))
}

validate_audited_products = \(before, after) {
  stopifnot(nrow(before) == nrow(after), !anyDuplicated(after$observation_key),
    setequal(before$observation_key, after$observation_key),
    all(after$classification_version == CLASSIFICATION_VERSION))
  old = before[match(after$observation_key, before$observation_key), ]
  for (field in c('retailer_product_id', 'ean_gtin', 'ean_gtin_all', 'brand',
      'product_name',
      'retailer_category', 'price_current', 'price_reference',
      'price_regular', 'alcohol_content',
      'volume_l', 'deposit', 'observed_at', 'raw_sha256', 'availability'))
    stopifnot(isTRUE(all.equal(old[[field]], after[[field]],
          check.attributes = F)))
  complete_pack = !is.na(after$volume_l) & !is.na(after$pack_count) & !is.na(
    after$unit_volume_l)
  stopifnot(all(abs(after$pack_count[complete_pack] * after$unit_volume_l[
          complete_pack] -
          after$volume_l[complete_pack]) < 1e-6))
  key_groups = after %>% distinct(product_key, beverage_group) %>% count(
    product_key)
  stopifnot(all(key_groups$n == 1))
  known_spirits = grepl('Lagerkorn|Calvados', after$product_name, ignore.case =
      T) &
    after$scope_status != 'excluded_nonfamily_product'
  stopifnot(all(after$beverage_group[known_spirits] == 'spirits'))
  bag = grepl('Elephant Gin Tragetasche', after$product_name, fixed = T)
  stopifnot(any(bag), all(after$beverage_group[bag] == 'non_beverage'),
    all(after$scope_status[bag] == 'excluded_nonfamily_product'))
  invisible(T)
}
