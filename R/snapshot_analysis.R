# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261016

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) SINGLE VALIDATED SNAPSHOT, USING THE EXISTING CLASSIFICATION AND PRICES
# <-----------------------------------------------------------------------------

SNAPSHOT_CLASSIFICATION_VERSION = 'beverage-0.9.1'
SNAPSHOT_ANALYSIS_VERSION = 'price-analysis-0.5.0'

snapshot_product_style = \(x) {
  label = tolower(paste(coalesce(x$product_name, ''),
      coalesce(x$designation_raw, '')))
  production = if ('product_description_evidence' %in% names(x))
    tolower(coalesce(x$product_description_evidence, '')) else rep('', nrow(x))
  introduction = substr(production, 1, 600)
  cocktail_pattern = paste0(
    "cocktail|longdrink|mojito|cuba[ -]?libre|sex on ",
    "the beach|blue lagoon|",
    "pi[nñ]a colada|caipirinha|margarita|negroni|", "daiquiri|cosmopolitan|",
    "old fashioned|whisk[e]?y sour|moscow mule|mai ", "tai|gin.{0,8}tonic|",
    "rum.{0,8}cola|paloma|bellini|spritz|sprizz|hugo|",
    "caipiroska|espresso martini|",
    "pornstar martini|long island|campari soda|",
    "tequila sunrise|appletini|apple martini|",
    "mimosa|amaretto sour|gin sour|lemon drop|",
    "\\bcosmo\\b|lynchburg lemonade|limao fizz|", "skinny bitch|",
    "lillet.{0,20}berry|limon.{0,8}lemonade|apple ",
    "soda|wild berry|\\bbatida\\b")
  named_cocktail = grepl(cocktail_pattern, label, perl = T)
  longdrink_mix = grepl(paste0(
      '(?:whisk[e]?y|vodka|wodka|korn|rum|bourbon).{0,100}',
      "(?:cola|lemonade|lemon|soda|ginger|energy|mate|",
      "maracuja|ice[ -]?tea|pineapple|raspberry|",
      "grapefruit|mango|ananas|passion)"),
    paste(label, introduction), perl = T)
  marketed_cocktail = named_cocktail | longdrink_mix |
    (x$in_rtd_rts_category %in% T & grepl('cocktail|longdrink', introduction)) |
    grepl(paste0("(?:premix|fertig|trinkfertig|",
      "servierfertig)[^.?!]{0,30}(?:cocktail|", "longdrink)|",
      "(?:cocktail|longdrink)[^.?!]{0,40}(?:premix|",
      "fertig|trinkfertig|servierfertig)"),
    introduction, perl = T)
  ordinary_radler = grepl('radler', label, fixed = T)
  ordinary_schorle = grepl('weinschorle|weinschörle|wein.?schorle', label,
    perl = T)
  ordinary_cider = x$beverage_group == 'cider' & !named_cocktail
  own_ready = grepl(paste0(
      'trinkfertig|servierfertig|fertig[ -]*(gemischt|gemixt)|',
      "ready[ -]to[ -](drink|serve)|premix[ -]cocktail|",
      "vorgemischte[rn]? cocktail|fertigcocktail"),
    paste(label, introduction), perl = T)
  designation_base = grepl(
    'basis für|basis mit|cocktail[ -]?basis|konzentrat|sirup',
    tolower(coalesce(x$designation_raw, '')), perl = T)
  liqueur_base = named_cocktail & grepl('^(?:likör|liqueur)(?![\\p{L}])',
    tolower(coalesce(x$designation_raw, '')), perl = T) &
    !own_ready & grepl('basis für cocktails|basis für|zutat', production,
    perl = T)
  # Marketing style does not establish spirit composition or any tax criterion.
  ready_style = (own_ready | x$ready_mixed_documented |
      x$in_rtd_rts_category %in% T) & !liqueur_base &
    (!x$not_ready_documented | (own_ready & !designation_base))
  x$product_style = case_when(
    x$scope_status == 'excluded_nonfamily_product' ~
      'nonbeverage_or_ingredient',
    ordinary_radler ~ 'beer_mixture',
    ordinary_schorle ~ 'wine_spritzer',
    ordinary_cider ~ 'ordinary_cider',
    own_ready & designation_base & (x$in_rtd_rts_category %in% T |
        named_cocktail) ~ 'unclear_ready_form',
    liqueur_base ~ 'cocktail_flavoured_liqueur_or_base',
    marketed_cocktail & ready_style ~ 'cocktail_rtd',
    x$rtd_candidate_documented & ready_style ~ 'other_rtd',
    x$in_rtd_rts_category %in% T & !x$not_ready_documented ~ 'other_rtd',
    x$in_rtd_rts_category %in% T & x$not_ready_documented ~
      'nonready_cocktail_base_or_spirit',
    T ~ 'other_beverage'
  )
  x$product_style_rule = paste0(
    "S01_product_label_own_introduction_and_ready_for",
    "m_independent_of_alcohol_origin")
  x$product_style_ready_evidence = ifelse(own_ready, introduction,
    ifelse(x$in_rtd_rts_category %in% T,
      'Explicit RTD/RTS category; no documented required mixing', ''))
  x$product_style_evidence = paste(x$product_name, x$designation_raw,
    ifelse(x$in_rtd_rts_category %in% T,
      'Explicit RTD/RTS category membership', ''),
    x$product_style_ready_evidence, sep = ' | ')
  x$rtd_abv_band = case_when(
    is.na(x$alcohol_content) ~ 'unknown_exact_abv',
    x$alcohol_content <= 1.2 ~ 'le_1.2',
    x$alcohol_content < 10 ~ 'gt_1.2_lt_10',
    x$alcohol_content == 10 ~ 'exactly_10',
    T ~ 'gt_10'
  )
  x
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) AROMATISED WINE PRODUCTS AS INTERMEDIATE PRODUCTS
# <-----------------------------------------------------------------------------

# Directive 92/83/EEC Art. 8 and 17, implemented in SchaumwZwStG: CN 2204/2205
# products
# above 1.2 and up to 22 % vol are intermediate products unless they qualify as
# still wine
# (alcohol entirely from fermentation; up to 15 % vol, or up to 18 % vol
# without enrichment).
# Regulation (EU) 251/2014 permits, but does not require, added alcohol in
# aromatised wine
# and limits it to below 22 % vol. A vermouth name alone therefore establishes
# neither.
# Alcohol used only to dissolve or extract flavourings is not an addition of
# alcohol
# under Regulation (EU) 251/2014; maceration statements are therefore not
# evidence.
# Mixtures up to 10 % vol stay with the alkopop and RTD assessment.
snapshot_intermediate_evidence = \(x) {
  designation = tolower(coalesce(x$designation_raw, ''))
  aromatised_wine_designation = grepl(
    paste0("^(wermut|vermouth|wein-aperitif|aperitif auf ",
      "weinbasis|aromatisierte[rs]? wein)\\b"),
    designation, perl = T)
  candidate = x$beverage_group == 'fortified_wine_aperitif' &
    aromatised_wine_designation &
    is.na(x$analysis_category) & x$analysis_category_rule ==
    'T06_tax_classification_unresolved' &
    x$scope_status == 'included' & !is.na(x$alcohol_content) &
    x$alcohol_content > 10 & x$alcohol_content <= 22
  # A separate alcohol component in the declared ingredient list of the same
  # offer.
  ingredient_alcohol_pattern = paste0("^(ethyl|wein|neutral)?alkohol( ",
    "landwirtschaftlichen ursprungs)?$|",
    '^(weindestillat|branntwein|weinbrand|brandy|grappa)$')
  ingredient_alcohol = vapply(coalesce(x$ingredients_raw, ''), \(text) {
      components = trimws(strsplit(tolower(text), ',', fixed = T)[[1]])
      hits = components[grepl(ingredient_alcohol_pattern, components, perl = T)]
      if (length(hits)) paste('Zutaten:', text) else ''
    }, character(1), USE.NAMES = F)
  # Own production statements before any recipe or serving suggestion.
  description = coalesce(x$archived_description_raw, '')
  production = sub(paste0("(?i)(rezept(vorschlag|idee)\\b|rezept\\s*:|",
      "servier(vorschlag|empfehlung)).*"),
    '', description, perl = T)
  spirit_dash = paste0("schuss [^.!?]{0,40}(obstbrand|weinbrand|brandy|",
    "grappa|gin)\\b")
  wine_spirit_blend = paste0("komposition aus [^.!?]{0,30}wein[^.!?]{0,",
    "20}\\b(gin|weinbrand|brandy|grappa|destillat)")
  # The verb must take the base as its direct object, not a historical aside.
  liqueur_wine_base = 'verwendet\\s+(\\p{L}+\\s+){0,2}(mistelle|likörwein)'
  added_alcohol_pattern = paste0('(?i)[^.!?]*(', spirit_dash, '|',
    wine_spirit_blend, '|',
    liqueur_wine_base, ')[^.!?]*')
  statement = regmatches(production, regexpr(added_alcohol_pattern, production,
      perl = T))
  description_alcohol = rep('', nrow(x))
  description_alcohol[grepl(added_alcohol_pattern, production, perl = T)] =
    clean_text(statement)
  documented = candidate & (nzchar(ingredient_alcohol) | nzchar(
      description_alcohol))
  # Above 18 % vol a CN 2204/2205 product cannot be still wine; 22 % vol is
  # excluded by 251/2014.
  above_still_wine = candidate & !documented & x$alcohol_content > 18 &
    x$alcohol_content < 22
  assigned = documented | above_still_wine
  x$analysis_category[assigned] = ifelse(x$alcohol_content[assigned] <= 15,
    ANALYSIS_CATEGORIES[5], ANALYSIS_CATEGORIES[6])
  x$analysis_category_rule[documented] = paste0(
    "T08_aromatised_wine_documented_added_alcohol_abv", "_gt10_le22")
  x$analysis_category_rule[above_still_wine] =
    'T09_aromatised_wine_abv_gt18_lt22_not_still_wine'
  x$analysis_category_evidence[documented] = ifelse(nzchar(ingredient_alcohol[
        documented]),
    ingredient_alcohol[documented], description_alcohol[documented])
  x$analysis_category_evidence[above_still_wine] = paste0(
    'Verkehrsbezeichnung: ',
    x$designation_raw[above_still_wine], '; ', x$alcohol_content[
      above_still_wine], ' % vol > 18 % vol')
  x$main_category[assigned] = 'Zwischenerzeugnisse'
  x$main_category_rule[assigned] = x$analysis_category_rule[assigned]
  x$tax_assessment_status[assigned] = paste0(
    "product_evidence_supported_not_binding_customs_d", "etermination")
  x$tax_review_reason[assigned] = NA_character_
  x
}

snapshot_rtd_membership = \(x) {
  open_abv = !is.na(x$alcohol_content) & x$alcohol_content > 1.2 &
    x$alcohol_content < 10
  list(A = x$alkopop_legal_status == 'confirmed',
    B = open_abv & x$spirit_component_documented &
      x$other_component_documented &
      x$ready_mixed_documented & !x$alcohol_basis_conflict &
      !x$not_ready_documented,
    C = open_abv & x$product_style == 'cocktail_rtd',
    D = !is.na(x$alcohol_content) & x$alcohol_content >= 10 &
      x$product_style == 'cocktail_rtd',
    E = x$in_rtd_rts_category %in% T)
}

snapshot_summary = \(values, n_observed) {
  n = length(values)
  q = if (n) as.numeric(quantile(values, c(.25, .5, .75), type =
          7)) else rep(NA_real_, 3)
  tibble(n_observed = n_observed, n = n, n_excluded = n_observed - n,
    mean = if (n) mean(values) else NA_real_, median = q[2], q1 = q[1], q3 =
    q[3],
    iqr = q[3] - q[1], minimum = if (n) min(values) else NA_real_,
    maximum = if (n) max(values) else NA_real_)
}

# The archived manufacturer PDF is not publishable. A structured record next to
# the review
# documents its URL, retrieval time, SHA-256 and the factual product data that
# were used.
snapshot_source_record = \(path) {
  record_path = file.path(dirname(path), 'le_coq_source_record.json')
  if (!file.exists(record_path)) return(NULL)
  record = read_json(record_path)
  stopifnot(nzchar(record$source$sha256), length(record$products) > 0)
  record$path = record_path
  record
}

snapshot_source_available = \(archived_file, record) {
  if (file.exists(archived_file)) return(T)
  !is.null(record) && identical(record$source$local_archived_file,
    archived_file)
}

snapshot_source_conflicts = \(x, path) {
  if (is.null(path) || !file.exists(path)) return(x)
  review = read.csv(path, colClasses = 'character', na.strings = '')
  record = snapshot_source_record(path)
  stopifnot(!anyDuplicated(review$retailer_product_id),
    all(review$retailer_product_id %in% x$retailer_product_id),
    all(review$decision == 'hold_unclear_composition_conflict'))
  for (j in seq_len(nrow(review))) {
    i = match(review$retailer_product_id[j], x$retailer_product_id)
    stopifnot(!is.na(review$source_url[j]), !is.na(
        review$variant_match_evidence[j]),
      snapshot_source_available(review$archived_source_file[j], record))
    stopifnot(!is.na(x$alcohol_content[i]), x$alcohol_content[i] > 1.2,
      x$alcohol_content[i] < 10)
    x$alcohol_basis_conflict[i] = T
    x$alcohol_basis_status[i] = 'conflicting_product_sources'
    x$alkopop_legal_status[i] = 'unclear'
    x$alkopop_status[i] = 'unclear'
    x$alkopop_research_status[i] = 'possible_alkopop'
    x$alkopop_rule[i] = 'S02_unresolved_variant_specific_source_conflict'
    x$alkopop_reason[i] = review$contradiction[j]
    x$alkopop_research_reason[i] = review$contradiction[j]
    x$main_category[i] = 'Unklare RTD'
    x$main_category_rule[i] = x$alkopop_rule[i]
    x$main_category_reason[i] = review$contradiction[j]
    x$analysis_category[i] = NA_character_
    x$analysis_category_rule[i] = 'T07_rtd_status_separate'
    x$alkopop_missing_features[i] = paste(
      'resolve_contradictory_product_sources',
      x$alkopop_missing_features[i], sep = ';')
  }
  x
}

run_snapshot_price_analysis = \(run_id = NULL, conflict_review = NULL,
  output_root = 'data/processed/exports') {
  manifest_path = active_complete_catalog(run_id)
  manifest = read_json(manifest_path)
  run_id = manifest$run_id
  # The superseded partial 2026-10-12 catalogue must never be present. Only
  # the 2026-10-16 snapshot was released after its documented isolation;
  # later snapshots need no such receipt.
  stopifnot(!dir.exists(file.path('data/raw/conalco',
        'catalog_20261012T092338_c2109ed57d1d')))
  isolation = if (identical(run_id,
      'catalog_refresh_20261016T161846_574254837b68')) file.path(
      'config', 'old_run_isolation_receipt.json')
  if (!is.null(isolation)) stopifnot(file.exists(isolation),
    isTRUE(read_json(isolation)$verified))
  source = manifest$proof$products$path
  raw_products = read_observations(source)
  raw_products$in_rtd_rts_category = raw_products$in_rtd_rts_category %in% c(
    T, 'TRUE')
  baseline = raw_products %>% classify_products() %>% quality_checks() %>%
    assign_price_categories()
  products = snapshot_source_conflicts(baseline, conflict_review)
  products = snapshot_product_style(products)
  products = snapshot_intermediate_evidence(products)
  products$classification_version = SNAPSHOT_CLASSIFICATION_VERSION
  products$analysis_version = SNAPSHOT_ANALYSIS_VERSION
  products$derivation_version = 'snapshot-0.1.0'
  products$run_status = 'complete'
  products$catalog_coverage_status = 'complete_defined_relevant_scope'
  require_complete_catalog(manifest_path, products)
  main = price_eligibility(products, 'include_unknown', 'beverage')
  technical = products
  technical$analysis_category = 'Descriptive RTD research'
  technical = price_eligibility(technical, 'include_unknown', 'beverage')
  membership = snapshot_rtd_membership(products)
  use_rtd = Reduce(`|`, membership)
  products$beverage_price_eligible = main$price_analysis_eligible
  products$beverage_price_exclusion_reasons = main$price_exclusion_reasons
  products$rtd_price_eligible = technical$price_analysis_eligible & use_rtd
  products$rtd_price_exclusion_reasons = technical$price_exclusion_reasons
  products$price_analysis_eligible = products$beverage_price_eligible |
    products$rtd_price_eligible
  products = standardize_alcohol_price(products)
  products$standardized_price_eligible = products$price_analysis_eligible
  products$price_analysis_eligible = products$beverage_price_eligible
  products$price_exclusion_reasons = products$beverage_price_exclusion_reasons
  products$availability_policy = 'include_unknown'
  for (name in names(membership)) products[[paste0('in_group_', name)]] =
    membership[[name]]
  rtd = bind_rows(lapply(names(membership), \(name) {
        selected = membership[[name]]
        use = selected & products$rtd_price_eligible
        snapshot_summary(products$price_eur_per_10g[use], sum(selected)) %>%
          mutate(group = name, .before = 1)
  }))
  e = products[membership$E, ]
  breakdown = e %>% group_by(rtd_abv_band, product_style,
    alcohol_basis_status, alkopop_legal_status) %>%
    group_modify(\(rows, keys) snapshot_summary(
      rows$price_eur_per_10g[rows$rtd_price_eligible], nrow(rows))) %>%
    ungroup()
  exact10 = membership$D & products$alcohol_content == 10
  rtd = bind_rows(rtd, snapshot_summary(products$price_eur_per_10g[
        exact10 & products$rtd_price_eligible], sum(exact10)) %>% mutate(
      group = 'D_exactly_10', .before = 1))
  tables = list(product_panel = products,
    statistics_seven_categories = summarize_offer_prices(products,
      'tax_le15', 7),
    statistics_exact15 = summarize_offer_prices(products, 'primary_exact15', 7),
    rtd_statistics_A_to_E = rtd, rtd_E_breakdown = breakdown,
    rtd_products = products[use_rtd | products$rtd_candidate_documented, ],
    legal_alkopop_candidates = products[products$alkopop_candidate, ],
    classification_review_required = products[products$scope_status ==
        'unclear_review' |
        products$alkopop_legal_status == 'unclear' | !is.na(
        products$tax_review_reason) |
        products$product_style == 'unclear_ready_form', ],
    le_coq_review = products[grepl('le coq', products$product_name,
        ignore.case = T), ],
    price_exclusions = products[!products$beverage_price_eligible, ],
    rtd_price_exclusions = products[use_rtd & !products$rtd_price_eligible, ],
    availability = products %>% count(availability, name = 'n'),
    category_completeness = read.csv(manifest$proof$category_completeness$path,
      check.names = F),
    field_completeness = products %>% group_by(retailer, main_category) %>%
      summarise(n_products = n_distinct(retailer_product_id),
      n_price_volume_abv_complete = sum(!is.na(price_current) & price_current >
          0 &
          !is.na(volume_l) & volume_l > 0 & !is.na(alcohol_content) &
          alcohol_content >= 0), .groups = 'drop'))
  tables$field_completeness_seven_categories = bind_rows(lapply(
      ANALYSIS_CATEGORIES,
      \(category) {
        rows = products[which(products$analysis_category == category), ]
        tibble(retailer = 'conalco', category = category, n_products = nrow(
            rows),
          n_price_volume_abv_complete = sum(!is.na(rows$price_current) &
              rows$price_current > 0 &
              !is.na(rows$volume_l) & rows$volume_l > 0 & !is.na(
              rows$alcohol_content) & rows$alcohol_content >= 0))
  }))
  tables$source_availability = tibble(retailer = c('conalco', 'aldi_nord',
      'myTime',
      'Netto Online', 'EDEKA24'), source_status = c('new_complete_snapshot',
      paste0("historical_pilot_available_not_part_of_this_cona",
        "lco_snapshot"), rep('no_raw_data', 3)),
    n_products_this_snapshot = c(nrow(products), rep(NA_integer_, 4)))
  reasons = bind_rows(lapply(which(!products$beverage_price_eligible), \(i)
      tibble(retailer_product_id = products$retailer_product_id[i],
        main_category = products$main_category[i],
        reason = strsplit(products$beverage_price_exclusion_reasons[i], ';',
          fixed = T)[[1]])))
  tables$price_exclusion_counts = reasons %>% count(main_category, reason,
    name = 'n')
  fields = c('beverage_group', 'main_category', 'analysis_category',
    'alkopop_legal_status',
    'alkopop_research_status', 'alcohol_basis_status', 'alcohol_basis_conflict',
    'alkopop_rule', 'alkopop_reason')
  tables$classification_changes = bind_rows(lapply(fields, \(field) {
        before = as.character(baseline[[field]])
        after = as.character(products[[field]])
        changed = which(coalesce(before, '<NA>') != coalesce(after, '<NA>'))
        tibble(retailer_product_id = products$retailer_product_id[changed],
          field =
          field,
          before = before[changed], after = after[changed],
          reason = products$alkopop_reason[changed])
  }))
  tables$classification_changes = bind_rows(tables$classification_changes,
    tibble(retailer_product_id = products$retailer_product_id,
      field = 'product_style', before = NA_character_, after =
      products$product_style,
      reason = products$product_style_rule))
  tables$classification_before = baseline
  tables$classification_audit = products[c('retailer_product_id',
      'product_name', 'product_url',
      'observed_at', 'beverage_group', 'main_category', 'analysis_category',
      'product_style',
      'scope_status', 'analysis_category_rule', 'analysis_category_evidence',
      'tax_assessment_status', 'tax_review_reason', 'product_style_evidence',
      'product_style_ready_evidence', 'alcohol_basis_status',
      'alkopop_legal_status',
      'alkopop_missing_features', 'composition_evidence', 'ingredients_raw',
      'serving_recipe_text_excluded', 'detail_metadata_file',
      'listing_metadata_file')]
  # Products retained despite the 2026-10-16 navigation exclusions.
  retention = 'config/scope_product_retention.csv'
  if (!is.null(isolation)) tables$scope_product_retention = read.csv(
    retention, check.names = F, stringsAsFactors = F)
  tables$scope_exclusions = tables$category_completeness[
    tables$category_completeness$category_status ==
      'excluded_additional_brand_navigation', ]
  if (!is.null(conflict_review)) tables$source_conflict_review = read.csv(
    conflict_review,
    colClasses = 'character', na.strings = '')
  output = file.path(output_root,
    paste0('conalco_snapshot_', run_id, '_', SNAPSHOT_CLASSIFICATION_VERSION))
  stopifnot(!dir.exists(output))
  ensure_dir(output)
  for (name in names(tables)) {
    x = tables[[name]]
    x$run_id = rep(run_id, nrow(x))
    x$collection_started_at_utc = rep(manifest$started_at_utc, nrow(x))
    x$collection_ended_at_utc = rep(manifest$ended_at_utc, nrow(x))
    if (!'classification_version' %in% names(x)) x$classification_version =
      rep(SNAPSHOT_CLASSIFICATION_VERSION,
      nrow(x))
    x$analysis_version = rep(SNAPSHOT_ANALYSIS_VERSION, nrow(x))
    write_csv_once(x, file.path(output, paste0(name, '.csv')))
  }
  inputs = c(manifest_path, source, isolation, conflict_review)
  if (!is.null(conflict_review)) inputs = c(inputs,
    unique(tables$source_conflict_review$archived_source_file),
    paste0("data/external_validation/le_coq_20261016/",
      "catalog_refresh_20261016T161846_574254837b68/", "review_manifest.json"))
  # Without the local manufacturer archive the publishable source record is the
  # hashed input.
  record = if (is.null(conflict_review)) NULL else snapshot_source_record(
      conflict_review)
  if (!is.null(record) && any(!file.exists(inputs))) inputs = c(inputs[
      file.exists(inputs)], record$path)
  inputs = inputs[!is.na(inputs) & nzchar(inputs)]
  write_json_once(list(run_id = run_id, calculated_at_utc = utc_now(),
      classification_version = SNAPSHOT_CLASSIFICATION_VERSION,
      analysis_version = SNAPSHOT_ANALYSIS_VERSION, availability_policy =
      'include_unknown',
      reference_g = 10, ethanol_density_g_ml = .789, quantile_type = 7,
      weighting = paste0("Unweighted distinct observed Conalco sale-unit ",
        "offers; one new validated snapshot only"),
      inputs = lapply(inputs, refresh_file_record),
      code = lapply(list.files(c('R', 'scripts'), '[.](R|py)$', recursive = T,
          full.names = T),
        refresh_file_record),
      outputs = lapply(list.files(output, '[.]csv$', full.names = T),
        refresh_file_record)),
    file.path(output, 'manifest.json'))
  output
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) ALDI NORD SNAPSHOT ANALYSIS, SEPARATE FROM THE CONALCO REFERENCE
# <-----------------------------------------------------------------------------

ALDI_ANALYSIS_VERSION = 'price-analysis-0.5.0-aldi-0.2.0'
ALDI_LOGICAL_FIELDS = c('in_core_alcohol_category', 'aldi_alcohol_scope',
  'website_available_flag',
  'website_coming_soon_flag', 'website_recall_flag',
  'price_valid_at_observation', 'conflicting_website_categories',
  'aldi_seasonal_category_member', 'card_values_consistent',
  'slug_identity_consistent', 'aldi_new_listing_marked',
  'aldi_brand_product_marked', 'aldi_price_highlight_marked',
  'detail_price_equal', 'detail_volume_equal',
  'detail_deposit_equal', 'detail_sale_unit_equal', 'detail_identity_equal',
  'card_identity_consistent', 'detail_price_validity_equal',
  'detail_base_price_equal', 'detail_website_available_flag',
  'detail_website_coming_soon_flag', 'detail_website_recall_flag',
  'detail_price_valid_at_observation')

aldi_flag = \(v) ifelse(v %in% c(T, 'TRUE'), T, ifelse(v %in% c(F, 'FALSE'),
    F, NA))

# Website-specific reasons are added to the shared eligibility rules; nothing
# is relaxed.
aldi_price_exclusions = \(x) vapply(seq_len(nrow(x)), \(i) {
    reasons = c(
      if (!x$aldi_in_analysis_population[i]) 'aldi_scope:outside_alcohol_scope',
      if (!x$price_status[i] %in% 'displayed_website_price') paste0(
          'price_status:', x$price_status[i]),
      if (x$website_coming_soon_flag[i] %in% T) 'website_coming_soon',
      if (x$website_available_flag[i] %in% F) 'website_not_available_flag',
      if (is.na(x$price_valid_at_observation[i])) 'price_validity_unknown',
      if (x$price_valid_at_observation[i] %in%
          F) 'price_not_valid_at_observation',
      if (is.na(x$website_available_flag[i])) 'website_availability_unknown',
      if (x$website_recall_flag[i] %in% T) 'website_recall',
      if (any(x$detail_website_recall_flag[i] %in% T)) 'detail_website_recall',
      if (any(x$detail_website_available_flag[i] %in%
            F)) 'detail_website_unavailable',
      if (any(x$detail_website_coming_soon_flag[i] %in%
            T)) 'detail_website_coming_soon',
      if (length(x$detail_price_status) &&
          !x$detail_price_status[i] %in% c('displayed_website_price', NA))
        paste0('detail_price_status:', x$detail_price_status[i]),
      if (!x$card_values_consistent[i] %in% T) 'conflicting_product_cards',
      if (!x$slug_identity_consistent[i] %in% T) 'slug_identity_unresolved',
      if (x$conflicting_website_categories[i] %in%
          T) 'conflicting_website_categories',
      if (!x$sale_unit_status[i] %in%
          'liquid_volume_parsed') 'sale_unit_status_unresolved',
      if (!x$deposit_status[i] %in% c('no_deposit_explicit',
          'deposit_displayed_separately'))
        paste0('deposit_status:', x$deposit_status[i]),
      unlist(lapply(intersect(c('detail_price_equal', 'detail_volume_equal',
              'detail_deposit_equal', 'detail_sale_unit_equal',
              'detail_identity_equal', 'detail_price_validity_equal',
              'detail_base_price_equal', 'detail_price_valid_at_observation'),
            names(x)),
          \(field) if (x[[field]][i] %in% F) paste0(field, ':conflict'))),
      if (!x$abv_status[i] %in% c('explicit_exact',
          'explicit_exact_archived_evidence', 'not_stated_in_source'))
        paste0('abv_status:', x$abv_status[i]))
    paste(reasons, collapse = ';')
  }, character(1))

aldi_join_reasons = \(a, b) vapply(strsplit(paste(a, b, sep = ';'), ';',
    fixed = T),
  \(r) paste(unique(r[nzchar(r)]), collapse = ';'), character(1))

run_aldi_snapshot_analysis = \(run_id, output_root =
  'data/processed/exports', previous_panel = NULL,
  abv_evidence_path = NULL) {
  manifest_path = file.path(aldi_run_dir(run_id), 'manifest.json')
  manifest = require_complete_aldi_snapshot(manifest_path)
  source = manifest$proof$products$path
  raw = read_observations(source)
  for (field in intersect(ALDI_LOGICAL_FIELDS, names(raw))) raw[[field]] =
    aldi_flag(raw[[field]])
  for (field in intersect(c('category_membership_count',
        'price_validity_days'), names(raw)))
  raw[[field]] = as.numeric(raw[[field]])
  require_complete_aldi_snapshot(manifest_path, raw)
  raw$in_rtd_rts_category = F
  raw$website_alcohol_content = raw$alcohol_content
  raw = apply_aldi_abv_evidence(raw, abv_evidence_path)

  # reuse the validated beverage-0.9.1 evidence path with the versioned ALDI
  # rules
  products = raw %>% classify_products(aldi_classification_rules(),
    'aldi_nord') %>% quality_checks() %>%
    assign_price_categories('aldi_nord') %>% snapshot_product_style() %>%
    snapshot_intermediate_evidence()
  products$classification_version = ALDI_CLASSIFICATION_VERSION
  products$analysis_version = ALDI_ANALYSIS_VERSION
  products$derivation_version = 'aldi-snapshot-0.1.0'
  products$run_status = 'complete'
  products$catalog_coverage_status = 'complete_defined_relevant_scope'
  # The gate above verifies every source measurement before derivation.

  # all observed offers are kept; only the eligible subset receives a
  # standardized price
  products$aldi_in_analysis_population = products$aldi_alcohol_scope %in% T
  main = price_eligibility(products, 'include_unknown', 'beverage')
  reasons = aldi_join_reasons(main$price_exclusion_reasons,
    aldi_price_exclusions(products))
  products$beverage_price_exclusion_reasons = reasons
  products$beverage_price_eligible = !nzchar(reasons)
  products$price_exclusion_reasons = reasons
  products$price_analysis_eligible = products$beverage_price_eligible
  products = standardize_alcohol_price(products)
  products$standardized_price_eligible = products$price_analysis_eligible
  products$availability_policy = 'include_unknown'
  population = products[products$aldi_in_analysis_population, ]

  ## tables
  key_fields = c('retailer_product_id', 'product_name', 'brand', 'product_url',
    'retailer_category',
    'listed_category_keys', 'scope_roles', 'main_category',
    'analysis_category', 'beverage_group', 'no_lo_status',
    'scope_status', 'alcohol_content', 'abv_status', 'alcohol_content_source',
    'price_current', 'deposit',
    'volume_l', 'pack_count', 'unit_volume_l', 'price_eur_per_10g',
    'beverage_price_exclusion_reasons')
  exclusion_rows = bind_rows(lapply(which(!population$beverage_price_eligible),
      \(i)
      tibble(main_category = population$main_category[i],
        reason = strsplit(population$beverage_price_exclusion_reasons[i], ';',
          fixed = T)[[1]])))
  name_key = tolower(gsub('[^[:alnum:]]', '', coalesce(products$product_name,
        '')))
  duplicates = products %>% mutate(name_key = name_key) %>% group_by(name_key,
    volume_raw) %>%
    filter(n_distinct(retailer_product_id) > 1) %>% ungroup() %>% select(
    any_of(c(key_fields, 'volume_raw')))
  validated = dirname(manifest$proof$products$path)
  tables = list(product_panel = products,
    price_eligible_products = products[products$beverage_price_eligible, ] %>%
      select(any_of(key_fields)),
    statistics_seven_categories = summarize_offer_prices(population,
      'tax_le15', 7),
    statistics_exact15 = summarize_offer_prices(population,
      'primary_exact15', 7),
    statistics_seven_categories_website_abv_only = summarize_offer_prices(
      population %>%
        mutate(price_analysis_eligible = price_analysis_eligible &
          abv_status == 'explicit_exact'), 'tax_le15', 7),
    abv_provenance = population %>% count(main_category, abv_status,
      alcohol_content_source, name = 'n'),
    field_completeness = population %>% group_by(retailer_main_category,
      main_category) %>%
      summarise(n_products = n_distinct(retailer_product_id), n_price = sum(
        !is.na(price_current)),
      n_total_volume = sum(!is.na(volume_l)), n_exact_abv = sum(!is.na(
          alcohol_content)),
      n_deposit_status_known = sum(!deposit_status %in% c('not_displayed',
          'deposit_flag_or_value_missing')),
      n_price_eligible = sum(beverage_price_eligible), .groups = 'drop'),
    price_exclusion_counts = if (nrow(exclusion_rows)) exclusion_rows %>%
        count(main_category, reason, name = 'n')
    else tibble(main_category = character(), reason = character(), n =
          integer()),
    classification_review_required = products[which(
        products$aldi_in_analysis_population &
          (products$scope_status == 'unclear_review' |
            products$alkopop_legal_status == 'unclear' |
            !is.na(products$tax_review_reason) | products$product_style ==
            'unclear_ready_form' |
            products$conflicting_website_categories %in% T)), ],
    category_completeness = read.csv(file.path(validated,
        'category_completeness.csv'), check.names = F),
    category_memberships = read.csv(file.path(validated,
        'category_memberships.csv'), colClasses = 'character'),
    multi_category_products = products[which(
        products$category_membership_count > 1), ] %>%
      select(any_of(c(key_fields, 'category_membership_count'))),
    supplementary_scope_decisions = products[
      !products$in_core_alcohol_category %in% T, ] %>%
      select(any_of(c(key_fields, 'aldi_in_analysis_population',
          'sale_unit_status'))),
    duplicate_candidates = duplicates,
    rtd_alkopop_review = products[products$alkopop_candidate |
        products$rtd_candidate_documented, ] %>%
      select(any_of(c(key_fields, 'alkopop_legal_status',
          'alkopop_research_status', 'alkopop_missing_features',
          'product_style'))),
    price_scope_statement = tibble(retailer = 'aldi_nord', price_scope =
      'advertised_website_geography_unverified',
      statement = manifest$price_scope, store_specific = F,
      transaction_price = F,
      comparability_with_conalco =
        'not_yet_assessed_no_cross_retailer_statistics'),
    source_availability = tibble(retailer = c('conalco', 'aldi_nord', 'netto'),
      source_status = c(
        'validated_reference_snapshot_not_part_of_this_analysis',
        'this_validated_snapshot',
        'not_integrated'), n_products_this_snapshot = c(NA_integer_, nrow(
          products), NA_integer_)))
  if (!is.null(previous_panel)) tables$price_change_review =
    price_change_review(previous_panel, products)
  output = file.path(output_root, paste0('aldi_nord_snapshot_', run_id, '_',
      ALDI_CLASSIFICATION_VERSION))
  stopifnot(!dir.exists(output))
  ensure_dir(output)
  for (name in names(tables)) {
    x = tables[[name]]
    x$run_id = rep(run_id, nrow(x))
    x$collection_started_at_utc = rep(manifest$started_at_utc, nrow(x))
    x$collection_ended_at_utc = rep(manifest$ended_at_utc, nrow(x))
    x$classification_version = rep(ALDI_CLASSIFICATION_VERSION, nrow(x))
    x$analysis_version = rep(ALDI_ANALYSIS_VERSION, nrow(x))
    write_csv_once(x, file.path(output, paste0(name, '.csv')))
  }
  write_json_once(list(run_id = run_id, retailer = 'aldi_nord',
      calculated_at_utc = utc_now(),
      classification_version = ALDI_CLASSIFICATION_VERSION, analysis_version =
      ALDI_ANALYSIS_VERSION,
      parser_version = manifest$parser_version, availability_policy =
      'include_unknown', reference_g = 10,
      ethanol_density_g_ml = ETHANOL_DENSITY_G_ML, quantile_type = 7,
      weighting = paste0("Unweighted distinct ALDI Nord website sale-unit ",
        "offers; one validated snapshot only"),
      price_scope = manifest$price_scope, n_products = nrow(products),
      n_analysis_population = nrow(population),
      n_price_eligible = sum(products$beverage_price_eligible),
      inputs = lapply(unique(c(manifest_path, source, abv_evidence_path,
            na.omit(products$abv_evidence_evidence_file))),
        refresh_file_record),
      code = lapply(list.files(c('R', 'scripts'), '[.]R$', recursive = T,
          full.names = T), refresh_file_record),
      outputs = lapply(list.files(output, '[.]csv$', full.names = T),
        refresh_file_record)),
    file.path(output, 'manifest.json'))
  output
}
