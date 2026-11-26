# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261012

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) COMMON OBSERVATION SCHEMA
# <-----------------------------------------------------------------------------

SCHEMA_VERSION = 'monitoring-0.3.0'
DERIVATION_VERSION = 'offline-0.7.1'
no_lo_levels = c('alcohol_free_label', 'reduced_alcohol_label',
  'zero_numeric', 'low_numeric_threshold_1.2')

standardize_products = \(x) {
  text_columns = c('price_scope', 'price_scope_evidence', 'region',
    'store_id', 'packaging_type',
    'assortment_type', 'offer_valid_from', 'offer_valid_until',
    'price_valid_from', 'price_valid_until')
  for (column in text_columns) if (!column %in% names(x)) x[[column]] =
      NA_character_
  if (!'alcohol_content' %in% names(x)) x$alcohol_content = x$abv
  if (!'alcohol_content_raw' %in% names(x)) x$alcohol_content_raw = x$abv_raw
  x %>% mutate(
    product_key = paste(retailer, retailer_product_id, sep = ':'),
    observation_key = paste(run_id, retailer, retailer_product_id, sep = ':'),
    price_regular = if_else(price_reference_type ==
        'shop_label_regular_same_unit',
      price_reference, NA_real_),
    price_scope = coalesce(price_scope, if_else(retailer == 'conalco',
        'online_shop', 'unknown')),
    deposit_basis = if_else(!is.na(deposit), 'EUR_per_sale_unit',
      NA_character_),
    price_basis = 'EUR_per_sale_unit_excluding_displayed_deposit',
    schema_version = SCHEMA_VERSION
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) CHECKS ON ALL GTINS AND PRODUCT ATTRIBUTES
# <-----------------------------------------------------------------------------

gtin_tokens = \(x) {
  if (is.na(x) || !nzchar(x)) return(character())
  unique(trimws(strsplit(x, '|', fixed = T)[[1]]))
}
quality_checks = \(x) {
  x = standardize_products(x)
  # Keep source flags separately; this column is recomputed for each derivation.
  if (!'source_validation_flags' %in% names(x)) x$source_validation_flags =
    x$validation_flags
  x$validation_flags = ''
  x = validate_observations(x, planned = nrow(x))
  all_codes = lapply(seq_len(nrow(x)), \(i) unique(c(gtin_tokens(
          x$ean_gtin_all[i]),
        gtin_tokens(x$ean_gtin[i]))))
  x$gtin_valid_codes = vapply(all_codes, \(v) paste(v[vapply(v, valid_gtin,
          logical(1))], collapse =
        '|'), character(1))
  x$gtin_invalid_codes = vapply(all_codes, \(v) paste(v[!vapply(v,
          valid_gtin, logical(1))], collapse =
        '|'), character(1))
  masks = list(
    invalid_gtin_any = nzchar(x$gtin_invalid_codes),
    category_abv_conflict = x$beverage_group %in% c('beer', 'cider') &
      x$alcohol_content > 20,
    wine_abv_conflict = x$beverage_group %in% c('still_wine',
      'sparkling_wine', 'semi_sparkling_wine') &
      x$alcohol_content > 25,
    no_lo_abv_conflict = x$no_lo_status == 'alcohol_free_label' &
      x$alcohol_content > 0.5,
    implausible_deposit_per_unit = x$deposit > 1 * x$pack_count | x$deposit >
      50,
    pack_volume_conflict = x$pack_basis == 'conflicting_display_and_pack_title',
    pack_size_unclear = x$pack_basis == 'unclear_pack',
    pack_total_disagrees = abs(x$pack_count * x$unit_volume_l - x$volume_l) >
      0.0001,
    classification_conflict = x$classification_conflict,
    scope_review_required = x$scope_status == 'unclear_review',
    nolo_family_unresolved = x$no_lo_status %in% no_lo_levels &
      x$scope_status != 'included',
    accessory_in_analysis = x$scope_status == 'included' &
      x$beverage_group == 'non_beverage',
    price_scope_unverified = x$price_scope ==
      'advertised_website_geography_unverified'
  )
  if ('category_membership_count' %in% names(
      x)) masks$multiple_category_memberships =
    x$category_membership_count > 1
  for (flag in names(masks)) {
    rows = which(!is.na(masks[[flag]]) & masks[[flag]])
    x$validation_flags[rows] = vapply(x$validation_flags[rows], \(v) add_flag(
        v, flag), character(1))
  }
  x
}
flag_summary = \(x) {
  flags = unlist(strsplit(coalesce(x$validation_flags, ''), ';', fixed = T))
  tibble(flag = flags[nzchar(flags)]) %>% count(flag, sort = T)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) CONSERVATIVE LONGITUDINAL COMPARISON
# <-----------------------------------------------------------------------------

compare_observations = \(previous, current) {
  stopifnot(length(unique(c(previous$classification_version,
          current$classification_version))) == 1)
  fields = c('product_name', 'brand', 'ean_gtin_all', 'alcohol_content',
    'unit_volume_l',
    'pack_count', 'volume_l', 'beverage_group', 'scope_status',
    'price_current', 'availability')
  before = previous %>% select(product_key, all_of(fields))
  after = current %>% select(product_key, all_of(fields))
  stopifnot(!anyDuplicated(before$product_key), !anyDuplicated(
      after$product_key))
  joined = full_join(before, after, by = 'product_key', suffix = c('_before',
      '_after'))
  joined$presence = case_when(
    !joined$product_key %in% before$product_key ~ 'newly_observed',
    !joined$product_key %in% after$product_key ~ 'not_observed_current_run',
    T ~ 'observed_both'
  )
  joined$changed_fields = vapply(seq_len(nrow(joined)), \(i) {
      if (joined$presence[i] != 'observed_both') return(NA_character_)
      changed = vapply(fields, \(f) !identical(joined[[paste0(f,
                '_before')]][i], joined[[paste0(f,
                '_after')]][i]), logical(1))
      paste(fields[changed], collapse = '|')
    }, character(1))
  joined$coverage_caveat = paste0(
    "Incomplete reachable catalogues: absence does ", "not prove delisting")
  joined
}

# Large or unexplained changes between two snapshots of one retailer are
# flagged for review,
# never corrected or removed automatically. Sale-unit changes explain a price
# change only as review.
price_change_review = \(previous, current, relative_threshold = 0.3,
  absolute_threshold = 5) {
  stopifnot(length(unique(c(previous$retailer, current$retailer))) == 1,
    !anyDuplicated(previous$product_key), !anyDuplicated(current$product_key))
  fields = c('product_key', 'product_name', 'price_current', 'volume_l',
    'pack_count', 'alcohol_content',
    'deposit', 'price_valid_from', 'price_valid_until', 'observed_at')
  before = previous %>% select(any_of(fields))
  after = current %>% select(any_of(fields))
  joined = inner_join(before, after, by = 'product_key', suffix = c(
      '_before', '_after'))
  joined$price_change = joined$price_current_after - joined$price_current_before
  joined$price_change_relative = joined$price_change /
    joined$price_current_before
  sale_unit_changed = coalesce(abs(joined$volume_l_after -
        joined$volume_l_before) > 1e-6, F) |
    coalesce(joined$pack_count_after != joined$pack_count_before, F)
  large = coalesce(abs(joined$price_change_relative) > relative_threshold, F) |
    coalesce(abs(joined$price_change) > absolute_threshold, F)
  joined$price_change_flag = case_when(
    is.na(joined$price_change) ~ 'price_missing_in_one_snapshot',
    sale_unit_changed ~ 'sale_unit_changed_review',
    large ~ 'large_price_change_review',
    joined$price_change != 0 ~ 'price_changed_within_thresholds',
    T ~ 'unchanged'
  )
  joined$relative_threshold = relative_threshold
  joined$absolute_threshold_eur = absolute_threshold
  joined
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) CROSS-RETAILER CANDIDATES, NEVER AUTOMATIC MERGES
# <-----------------------------------------------------------------------------

match_retailers = \(left, right) {
  fields = c('product_key', 'product_name', 'brand', 'beverage_group',
    'alcohol_content',
    'unit_volume_l', 'pack_count')
  expand = \(x) bind_rows(lapply(seq_len(nrow(x)), \(i) {
        codes = gtin_tokens(x$gtin_valid_codes[i])
        if (!length(codes)) return(NULL)
        canonical = paste0(vapply(nchar(codes), \(n) paste(rep('0', 14 - n),
              collapse = ''),
            character(1)), codes)
        bind_cols(x[rep(i, length(codes)), fields], tibble(gtin = canonical))
  }))
  a = expand(left)
  b = expand(right)
  matches = tibble()
  if (nrow(a) && nrow(b)) {
    matches = inner_join(a, b, by = 'gtin', suffix = c('_left', '_right'),
      relationship = 'many-to-many')
    matches$method = 'valid_gtin_candidate'
  }
  # Missing GTIN: exact normalized name or low edit distance plus same family.
  candidates = list()
  normalize = \(v) gsub('[^[:alnum:]]', '', tolower(coalesce(v, '')))
  for (i in seq_len(nrow(right))) {
    rows = which(left$beverage_group == right$beverage_group[i])
    if (!length(rows)) next
    distances = as.numeric(adist(normalize(right$product_name[i]),
        normalize(left$product_name[rows])))
    ratio = distances / pmax(nchar(normalize(right$product_name[i])),
      nchar(normalize(left$product_name[rows])), 1)
    rows = rows[ratio <= 0.2]
    if (!length(rows)) next
    l = left[rows, fields]
    r = right[rep(i, length(rows)), fields]
    names(l) = paste0(names(l), '_left')
    names(r) = paste0(names(r), '_right')
    candidates[[length(candidates) + 1]] = bind_cols(l, r) %>%
      mutate(gtin = NA_character_, method =
        'name_family_candidate_manual_only')
  }
  matches = bind_rows(matches, bind_rows(candidates))
  if (!nrow(matches)) return(tibble(product_key_left = character(),
        product_key_right =
        character(), method = character(), match_status = character()))
  matches %>% mutate(
    brand_equal = tolower(brand_left) == tolower(brand_right),
    name_equal = normalize(product_name_left) == normalize(product_name_right),
    abv_equal = abs(alcohol_content_left - alcohol_content_right) < 0.01,
    unit_volume_equal = abs(unit_volume_l_left - unit_volume_l_right) < 0.0001,
    pack_count_equal = pack_count_left == pack_count_right,
    family_equal = beverage_group_left == beverage_group_right,
    sale_unit_comparable = unit_volume_equal & pack_count_equal,
    match_status = 'manual_review_required'
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) INDEPENDENT NAME/CATEGORY CHECKS; WARNINGS ONLY
# <-----------------------------------------------------------------------------

# Merged on 2026-10-19 from R/family_checks.R (file created 2026-10-12);
# content unchanged
FAMILY_CHECK_VERSION = 'family-checks-0.1.0'
family_evidence_warnings = \(products) {
  x = products %>% filter(scope_status != 'excluded_nonfamily_product')
  name = tolower(coalesce(x$product_name, ''))
  category = tolower(coalesce(x$retailer_category, ''))
  named_spirit = grepl(
    paste0("\\b(calvados|lagerkorn|doppelkorn|kornbrand|",
      "grappa|cognac|armagnac|weinbrand)\\b"),
    name, perl = T
  )
  named_beer = grepl('\\b(bier|pils|pilsener|radler|lager)\\b', name, perl = T)
  spirit_category = grepl(
    paste0("(^|[|>])\\s*(korn|calvados|obstbrand|grappa|",
      "cognac|armagnac|weinbrand)\\s*($|[|>])"),
    category, perl = T
  )
  beer_category = grepl('(^|[|>])\\s*bier\\s*($|[|>])', category, perl = T)
  rows = which((named_spirit & x$beverage_group %in% c('beer', 'cider')) |
      (spirit_category & x$beverage_group %in% c('beer', 'cider')) |
      (named_beer & spirit_category) | (named_spirit & beer_category))
  x[rows, ] %>% select(product_key, product_name, retailer_category,
    beverage_group, classification_version) %>% mutate(
    validation_flag = 'name_category_family_conflict',
    validation_version = FAMILY_CHECK_VERSION,
    disposition = 'manual_review_without_automatic_reclassification'
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) SEPARATE MACHINE RESULTS AND HUMAN REVIEW
# <-----------------------------------------------------------------------------

# Merged on 2026-10-19 from R/review.R (file created 2026-10-12); content
# unchanged
review_template = \(products) {
  products %>% select(retailer, retailer_product_id, product_key, run_id,
    ean_gtin, product_name, product_url, retailer_category, beverage_group,
    alcohol_content, alcohol_content_raw, volume_l, pack_count, unit_volume_l,
    no_lo_status, scope_status, validation_flags, raw_sha256, parser_version,
    classification_version) %>% mutate(
    proposed_beverage_group = NA_character_, proposed_scope_status =
    NA_character_,
    proposed_no_lo_status = NA_character_, review_decision = NA_character_,
    review_comment = NA_character_, reviewed = F, reviewer = NA_character_,
    reviewed_at = NA_character_, review_schema_version = 'shk-0.1.0'
  )
}

prepare_review_sample = \(products, previous_unclear_ids = character(),
  per_stratum = 20, seed =
  20261012) {
  stopifnot(!anyDuplicated(products$product_key), per_stratum >= 1)
  x = products %>% mutate(
    priority = retailer_product_id %in% previous_unclear_ids |
      scope_status == 'unclear_review' |
      no_lo_status %in% c(no_lo_levels, 'no_lo_candidate_category') |
      grepl(paste0("category_abv_conflict|no_lo_abv_conflict|",
        "pack_volume_conflict|pack_size_unclear"),
      validation_flags),
    review_stratum = paste(retailer, beverage_group, scope_status, sep = ':'),
    sampling_reason = if_else(priority, 'priority_census', 'stratified_random')
  )
  priorities = x %>% filter(priority) %>% mutate(inclusion_probability = 1,
    sampling_weight = 1)
  random = withr::with_seed(seed, x %>% filter(!priority) %>%
      group_by(review_stratum) %>%
      mutate(stratum_size = n()) %>%
      group_modify(\(data, key) {
        n = min(per_stratum, nrow(data))
        data[sample.int(nrow(data), n), ] %>% mutate(inclusion_probability =
          n / stratum_size,
          sampling_weight = 1 / inclusion_probability)
    }) %>% ungroup())
  bind_rows(priorities, random) %>% mutate(sampling_seed = seed)
}

validate_manual_reviews = \(review, machine) {
  keys = c('product_key', 'run_id', 'classification_version')
  automatic = review_template(machine)
  manual_fields = c('proposed_beverage_group', 'proposed_scope_status',
    'proposed_no_lo_status',
    'review_decision', 'review_comment', 'reviewed', 'reviewer',
    'reviewed_at', 'review_schema_version')
  protected = setdiff(names(automatic), manual_fields)
  stopifnot(!anyDuplicated(review[keys]), all(protected %in% names(review)))
  expected = automatic %>% semi_join(review, by = keys) %>% arrange(
    product_key, run_id)
  actual = review %>% arrange(product_key, run_id)
  stopifnot(nrow(expected) == nrow(actual))
  if (!isTRUE(all.equal(as.data.frame(expected[protected]), as.data.frame(
          actual[protected]),
        check.attributes = F))) stop(
      'Machine fields were edited; retain proposals separately')
  completed = review %>% filter(reviewed)
  stopifnot(all(completed$review_decision %in% c('agree', 'propose_change',
        'unresolved')),
    all(!is.na(completed$reviewer) & nzchar(completed$reviewer)), all(!is.na(
        completed$reviewed_at)))
  proposals = completed %>% filter(review_decision == 'propose_change')
  stopifnot(all(is.na(proposals$proposed_scope_status) |
        proposals$proposed_scope_status %in% c('included', 'unclear_review',
        'excluded_nonfamily_product')))
  allowed_groups = c(unique(classification_rules$beverage_group),
    'alternative_unspecified',
    'other_alcoholic', 'unclassified', 'softdrink_mixer', 'ingredient_food',
    'non_beverage')
  stopifnot(all(is.na(proposals$proposed_beverage_group) |
        proposals$proposed_beverage_group %in% allowed_groups),
    all(!is.na(proposals$review_comment) & nzchar(proposals$review_comment)))
  # Validated proposals are a separate ledger; adoption requires a new
  # derivation.
  completed
}
