# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261015

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) EVIDENCE-BASED ANALYTICAL GROUPS, SEPARATE FROM BEVERAGE FAMILIES
# <-----------------------------------------------------------------------------

PRICE_ANALYSIS_VERSION = 'price-analysis-0.4.1'
ANALYSIS_CATEGORIES = c('Bier', 'Wein', 'Schaumwein', 'Spirituosen',
  'Zwischenerzeugnisse ≤15 % vol', 'Zwischenerzeugnisse >15 % vol', 'Alkopops')
ETHANOL_DENSITY_G_ML = 0.789

# ABV is a necessary boundary, never sufficient evidence of a tax category.
assign_price_categories = \(products, evidence_retailers = 'conalco') {
  x = classify_alkopops(products, evidence_retailers)
  if (!'archived_description_raw' %in% names(x)) x$archived_description_raw =
    NA_character_
  name = tolower(coalesce(x$product_name, ''))
  designation = tolower(coalesce(x$designation_raw, ''))
  description = coalesce(x$archived_description_raw, '')
  if (length(description) == 0) description = rep('', nrow(x))
  # Serving suggestions must not establish the composition of the sold product.
  production = sub(paste0("(?i)(rezept(vorschlag|idee)\\b|rezept\\s*:|",
      "servier(vorschlag|empfehlung)|", "cocktail-inspiration).*"),
    '', description, perl = T)
  fortified_pattern = paste0(
    '(?i)[^.?!]*(aufgesprit|fortifiz|gespritet|',
    'weine werden mit[^.?!]*fruchtlikören|',
    'zugabe von[^.?!]*(branntwein|weingeist)|',
    "mit[^.?!]*(alkohol|weingeist)[^.?!]*(angereicher", "t|versetzt))[^.?!]*"
  )
  # Preserve row positions when no fortification evidence is present.
  fortification = vapply(production, \(text) {
      hint = paste0("aufgesprit|fortifiz|gespritet|fruchtlikören|",
        "branntwein|weingeist|angereichert|versetzt")
      if (!grepl(hint, text, ignore.case = T)) return('')
      # The original pattern cannot cross sentence punctuation; test only those
      # spans.
      spans = regmatches(text, gregexpr('[^.?!]+', text, perl = T))[[1]]
      spans = spans[grepl(hint, spans, ignore.case = T)]
      for (span in spans) {
        m = regexpr(fortified_pattern, span, perl = T)
        if (m[1] >= 0) return(clean_text(regmatches(span, m)))
      }
      ''
    }, character(1))
  designated_fortified = grepl('^likörwein\\b|^sherry\\b', designation,
    perl = T)
  wine_aperitif = x$beverage_group == 'fortified_wine_aperitif' |
    grepl(paste0("^aromatisierte[rs] wein|^aromatisiertes ",
      "weinhaltiges getränk"), designation)
  intermediate = (designated_fortified | (wine_aperitif & nzchar(
        fortification))) &
    !is.na(x$alcohol_content) & x$alcohol_content > 1.2 &
    x$alcohol_content <= 22

  alkopop = x$alkopop_status == 'confirmed'
  nolo = x$no_lo_status %in% no_lo_levels | (!is.na(x$alcohol_content) &
      x$alcohol_content <= 1.2)
  x$analysis_category = case_when(
    x$scope_status != 'included' | nolo ~ NA_character_,
    alkopop ~ 'Alkopops',
    intermediate & x$alcohol_content <= 15 ~ ANALYSIS_CATEGORIES[5],
    intermediate & x$alcohol_content > 15 ~ ANALYSIS_CATEGORIES[6],
    x$beverage_group == 'beer' ~ 'Bier',
    x$beverage_group == 'still_wine' ~ 'Wein',
    x$beverage_group == 'sparkling_wine' ~ 'Schaumwein',
    x$beverage_group == 'spirits' ~ 'Spirituosen',
    T ~ NA_character_
  )
  x$analysis_category_rule = case_when(
    x$scope_status != 'included' ~ x$scope_rule,
    nolo ~ 'T00_nolo_separate',
    alkopop ~ 'T01_spirit_soft_mixture_packaged_abv_gt1.2_lt10',
    intermediate & designated_fortified ~
      'T02_designated_fortified_wine_abv_gt1.2_le22',
    intermediate ~ paste0("T03_wine_aperitif_documented_fortification_abv_g",
      "t1.2_le22"),
    x$beverage_group %in% c('beer', 'still_wine', 'sparkling_wine', 'spirits') ~
      'T04_descriptive_beverage_family',
    x$beverage_group %in% c('cider', 'rice_wine', 'other_wine') ~
      'T05_other_fermented_separate',
    T ~ 'T06_tax_classification_unresolved'
  )
  x$analysis_category_evidence = case_when(
    alkopop ~ x$alkopop_evidence,
    intermediate & designated_fortified ~ x$designation_raw,
    intermediate ~ fortification,
    T ~ x$classification_evidence
  )
  x$tax_assessment_status = case_when(
    intermediate | alkopop ~ paste0(
      "product_evidence_supported_not_binding_customs_d", "etermination"),
    x$analysis_category_rule == 'T04_descriptive_beverage_family' ~
      'descriptive_family_not_automatic_tax_determination',
    T ~ 'not_assigned'
  )
  # Preserve the historical tax_group placeholder: no customs classification is
  # invented.
  x$tax_review_reason = case_when(
    x$beverage_group == 'fortified_fruit_must' ~
      paste0("Pommeau: fruit must plus distillate; CN 2206 ",
      "versus 2208 and fermentation evidence unresolved"),
    wine_aperitif & !intermediate & !nolo ~
      paste0("Wine aperitif/vermouth alone does not establish ",
      "added alcohol, CN code or exclusion from wine"),
    x$beverage_group == 'semi_sparkling_wine' & !nolo ~
      paste0("Perlwein: pressure/closure and fermentation ",
      "must be checked before a tax assignment"),
    x$beverage_group == 'ready_to_drink_other' & !alkopop & !nolo ~
      paste0("Mixture: spirit origin/composition or statutory ",
      "ABV >1.2 and <10 not established"),
    x$beverage_group == 'spirits' & !is.na(x$alcohol_content) &
      x$alcohol_content < 15 & !nolo &
      !grepl('eierlikör', designation) ~
      paste0("Low-strength descriptive spirits family: legal ",
      "spirit-drink status/tax basis unverified"),
    x$beverage_group == 'still_wine' & x$alcohol_content > 15 ~
      paste0("Wine above 15: exclusive fermentation and ",
      "non-enrichment require confirmation"),
    T ~ NA_character_
  )
  # Ambiguous low-strength generic aperitifs must not enter the spirits result.
  unclear_spirit = !is.na(x$tax_review_reason) & x$beverage_group == 'spirits' &
    !grepl('^(likör|spirituose)', designation)
  x$analysis_category[unclear_spirit] = NA_character_
  x$analysis_category_rule[unclear_spirit] = 'T06_tax_classification_unresolved'
  explicit_rtd = grepl(paste0(
      "mischgetränk|cocktail|longdrink|ready.to.(drink|", "serve)|spritz"),
    paste(name, designation), perl = T) | x$beverage_group ==
    'ready_to_drink_other' |
    x$joint_components %in% T
  unconfirmed_rtd = (explicit_rtd | x$alkopop_status == 'unclear') & !alkopop &
    !x$scope_status %in% 'excluded_nonfamily_product'
  conalco = if ('retailer' %in% names(x)) is.na(x$retailer) | x$retailer %in%
      evidence_retailers else
    rep(T, nrow(x))
  fermented_regular = x$fermented_only_documented & x$beverage_group %in% c(
    'beer', 'cider')
  regular_spirit_rtd = conalco & x$ordinary_spirit_designation &
    x$ready_mixed_documented &
    !is.na(x$alcohol_content) & x$alcohol_content >= 15
  regular_family = fermented_regular | regular_spirit_rtd
  unconfirmed_rtd[conalco] = (x$rtd_candidate_documented[conalco] &
      !x$not_ready_documented[conalco] & !regular_family[conalco]) |
    x$alkopop_research_status[conalco] %in% c('documented_spirit_rtd',
    'possible_alkopop', 'general_rtd_unknown_basis')
  unconfirmed_rtd = unconfirmed_rtd & !alkopop & !x$scope_status %in%
    'excluded_nonfamily_product'
  x$analysis_category[unconfirmed_rtd] = NA_character_
  x$analysis_category_rule[unconfirmed_rtd] = 'T07_rtd_status_separate'
  family_main = recode(x$beverage_group, beer = 'Bier', still_wine = 'Wein',
    sparkling_wine = 'Schaumwein', spirits = 'Spirituosen', .default =
      x$beverage_group)
  x$main_category = case_when(
    x$scope_status == 'excluded_nonfamily_product' ~ family_main,
    alkopop ~ 'Alkopops',
    x$alkopop_status == 'unclear' ~ 'Unklare RTD',
    unconfirmed_rtd ~ 'Sonstige RTD',
    intermediate ~ 'Zwischenerzeugnisse',
    T ~ family_main
  )
  x$main_category[conalco & x$alkopop_research_status ==
      'documented_spirit_rtd'] = 'Spirituosenhaltige RTD'
  x$main_category[conalco & x$alkopop_research_status == 'possible_alkopop'] =
    'Unklare RTD'
  x$main_category[conalco & x$alkopop_research_status ==
      'general_rtd_unknown_basis'] = 'RTD mit unbekannter Alkoholbasis'
  restore_family = conalco & (!unconfirmed_rtd | regular_family) & !alkopop &
    !intermediate
  x$main_category[restore_family] = family_main[restore_family]
  unresolved_aperitif = conalco & x$aperitif_base_documented &
    x$beverage_group == 'spirits' &
    !intermediate & !x$spirit_component_documented &
    !x$ordinary_spirit_designation
  x$analysis_category[unresolved_aperitif] = NA_character_
  x$analysis_category_rule[unresolved_aperitif] =
    'T06_aperitif_base_alcohol_origin_unresolved'
  x$main_category[unresolved_aperitif] =
    'Aperitif-/Cocktailbasis (Alkoholart ungeklärt)'
  x$analysis_category_evidence[unresolved_aperitif] =
    x$product_description_evidence[unresolved_aperitif]
  x$tax_assessment_status[unresolved_aperitif] = 'not_assigned'
  x$tax_review_reason[unresolved_aperitif] =
    paste0("Aperitif/cocktail base identified; suitable ",
    "spirit origin is not documented for this product")
  x$main_category_rule = if_else(unconfirmed_rtd | alkopop,
    x$alkopop_rule, x$analysis_category_rule)
  x$main_category_reason = if_else(unconfirmed_rtd | alkopop,
    x$alkopop_reason, coalesce(x$tax_review_reason, x$classification_evidence))
  x$analysis_version = ifelse(conalco, PRICE_ANALYSIS_VERSION,
    'price-analysis-0.2.0')
  x
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) ELIGIBILITY BEFORE ANY STANDARDIZED-PRICE CALCULATION
# <-----------------------------------------------------------------------------

price_eligibility = \(products, availability_policy = 'include_unknown',
  analysis_scope = 'beverage') {
  stopifnot(availability_policy %in% c('available_only', 'include_unknown',
      'all_priced'))
  stopifnot(analysis_scope %in% c('beverage', 'alkopop_research'))
  x = products
  flags = coalesce(x$validation_flags, '')
  x$price_exclusion_reasons = vapply(seq_len(nrow(x)), \(i) {
      reasons = character()
      if (x$scope_status[i] != 'included') reasons = c(reasons, paste0(
          'scope:', x$scope_status[i]))
      if (analysis_scope == 'beverage' && is.na(x$analysis_category[i]))
      reasons = c(reasons, x$analysis_category_rule[i])
      if (analysis_scope == 'alkopop_research') {
        if (!x$alkopop_research_status[i] %in% c('confirmed_legal_alkopop',
            'documented_spirit_rtd', 'possible_alkopop',
            'general_rtd_unknown_basis'))
        reasons = c(reasons, 'outside_alkopop_research_definition')
        if (is.na(x$alkopop_assessed_abv[i]))
        reasons = c(reasons, 'missing_final_mixture_abv')
        if (x$fermented_only_documented[i]) reasons = c(reasons,
          'exclusively_fermented_components')
      }
      if (is.na(x$alcohol_content[i])) reasons = c(reasons, 'missing_exact_abv')
      else if (!is.finite(x$alcohol_content[i]) || x$alcohol_content[i] <= 0 ||
            x$alcohol_content[i] > 100) reasons = c(reasons,
          'zero_or_invalid_abv')
      if (is.na(x$price_current[i]) || !is.finite(x$price_current[i]) ||
          x$price_current[i] <= 0) reasons = c(reasons,
        'missing_or_invalid_price')
      if (is.na(x$volume_l[i]) || !is.finite(x$volume_l[i]) || x$volume_l[
          i] <= 0)
      reasons = c(reasons, 'missing_or_invalid_total_volume')
      if (is.na(x$pack_count[i]) || is.na(x$unit_volume_l[i]) ||
          x$pack_basis[i] %in% c('unclear_pack',
          'conflicting_display_and_pack_title'))
      reasons = c(reasons, 'sale_unit_unresolved')
      if (!is.na(x$pack_count[i]) && (!is.finite(x$pack_count[i]) ||
            x$pack_count[i] < 1 ||
            x$pack_count[i] > 1000 || x$pack_count[i] != floor(x$pack_count[
              i])))
      reasons = c(reasons, 'invalid_pack_count')
      if (!is.na(x$unit_volume_l[i]) && (!is.finite(x$unit_volume_l[i]) ||
            x$unit_volume_l[i] <= 0))
      reasons = c(reasons, 'invalid_unit_volume')
      if (all(is.finite(c(x$pack_count[i], x$unit_volume_l[i], x$volume_l[
                i]))) &&
          abs(x$pack_count[i] * x$unit_volume_l[i] - x$volume_l[i]) > 1e-6)
      reasons = c(reasons, 'inconsistent_pack_total_volume')
      if (grepl('mix.paket|misch.box|tasting|probier.set|adventskalender',
          x$product_name[i], ignore.case = T))
      reasons = c(reasons, 'heterogeneous_pack_requires_verified_ethanol_total')
      if (availability_policy != 'all_priced' && x$availability[i] %in% c(
          'out_of_stock', 'backorder', 'preorder'))
      reasons = c(reasons, 'not_currently_available')
      if (availability_policy == 'available_only' && !x$availability[i] %in%
          'in_stock')
      reasons = c(reasons, 'availability_not_confirmed')
      if (is.na(x$currency[i]) || x$currency[i] != 'EUR') reasons = c(
        reasons, 'currency_unresolved')
      if (grepl(paste0("displayed_computed_litre_price_disagree|",
            "pack_total_disagrees|structured_visible_price_di",
            "sagree|no_lo_abv_conflict|implausible_abv|",
            "category_abv_conflict|wine_abv_conflict"),
          flags[i])) reasons = c(reasons, 'price_volume_conflict')
      paste(unique(reasons), collapse = ';')
    }, character(1))
  x$price_analysis_eligible = !nzchar(x$price_exclusion_reasons)
  x
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) PHASE 1: GATED COMPUTATION ON ALREADY AUDITED OBSERVATIONS
# <-----------------------------------------------------------------------------

calculate_standard_prices = \(products, release_manifest) {
  gate = read_json(release_manifest)
  stopifnot(isTRUE(gate$phase0_passed), identical(
      gate$classification_version, CLASSIFICATION_VERSION),
    identical(gate$analysis_version, PRICE_ANALYSIS_VERSION))
  for (entry in gate$corrected_products) stopifnot(identical(
        digest(file = entry$path, algo = 'sha256'), entry$sha256))
  for (entry in gate$code) stopifnot(identical(
        digest(file = entry$path, algo = 'sha256'), entry$sha256))
  approved = read_observations(gate$corrected_products[[1]]$path)
  stopifnot(identical(products$observation_key, approved$observation_key))
  for (field in c('price_current', 'volume_l', 'alcohol_content',
      'analysis_category',
      'price_analysis_eligible', 'price_exclusion_reasons',
      'classification_version'))
    stopifnot(identical(as.character(products[[field]]), as.character(
          approved[[field]])))
  standardize_alcohol_price(products)
}

standardize_alcohol_price = \(products, reference_alcohol_g = 10,
  ethanol_density_g_ml = ETHANOL_DENSITY_G_ML) {
  stopifnot(length(reference_alcohol_g) == 1, is.finite(reference_alcohol_g),
    reference_alcohol_g > 0,
    length(ethanol_density_g_ml) == 1, is.finite(ethanol_density_g_ml),
    ethanol_density_g_ml > 0)
  x = products
  x$total_beverage_volume_ml = x$volume_l * 1000
  x$pure_alcohol_ml = NA_real_
  x$pure_alcohol_g = NA_real_
  x$price_per_reference_g = NA_real_
  x$ethanol_g = NA_real_
  x$price_eur_per_10g = NA_real_
  use = x$price_analysis_eligible %in% T
  stopifnot(all(is.finite(x$price_current[use]) & x$price_current[use] > 0),
    all(is.finite(x$volume_l[use]) & x$volume_l[use] > 0),
    all(is.finite(x$alcohol_content[use]) & x$alcohol_content[use] > 0 &
        x$alcohol_content[use] <= 100))
  x$pure_alcohol_ml[use] = x$total_beverage_volume_ml[use] *
    x$alcohol_content[use] / 100
  x$pure_alcohol_g[use] = x$pure_alcohol_ml[use] * ethanol_density_g_ml
  x$price_per_reference_g[use] = x$price_current[use] * reference_alcohol_g /
    x$pure_alcohol_g[use]
  x$ethanol_g = x$pure_alcohol_g
  x$price_eur_per_10g[use] = x$price_current[use] * 10 / x$pure_alcohol_g[use]
  x$reference_alcohol_g = reference_alcohol_g
  x$standardized_price_flag = case_when(
    !use ~ 'not_calculable',
    x$price_eur_per_10g < 0.05 | x$price_eur_per_10g > 100 ~
      'extreme_review_not_automatically_excluded',
    T ~ 'within_review_thresholds'
  )
  stopifnot(all(is.finite(x$price_per_reference_g[use])), all(
      x$price_per_reference_g[use] > 0))
  x
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) SNAPSHOT SELECTION AND COMPREHENSIVE DESCRIPTIVE SUMMARIES
# <-----------------------------------------------------------------------------

select_latest_offers = \(products, as_of_date = NULL, by_day = F) {
  x = products
  if (!is.null(as_of_date)) x = x %>% filter(date <= as_of_date)
  if (!nrow(x)) return(x)
  if (!by_day) x = x %>% group_by(retailer) %>%
    filter(date == max(date)) %>% ungroup()
  x %>% arrange(desc(observed_at), desc(run_id), observation_key) %>%
    distinct(retailer, date, retailer_product_id, .keep_all = T) %>%
    arrange(retailer, date, retailer_product_id)
}

analysis_partition = \(products, partition = 'primary_exact15') {
  labels = products$main_category
  intermediate = labels == 'Zwischenerzeugnisse'
  if (partition == 'primary_exact15') {
    labels[intermediate] = case_when(
      products$alcohol_content[intermediate] < 15 ~
        'Zwischenerzeugnisse <15 % vol',
      products$alcohol_content[intermediate] == 15 ~
        'Zwischenerzeugnisse =15 % vol',
      products$alcohol_content[intermediate] > 15 ~
        'Zwischenerzeugnisse >15 % vol'
    )
  } else {
    stopifnot(partition == 'tax_le15')
    labels[intermediate] = ifelse(products$alcohol_content[intermediate] <=
        15,
      'Zwischenerzeugnisse ≤15 % vol', 'Zwischenerzeugnisse >15 % vol')
  }
  labels
}

summarize_offer_prices = \(products, partition = 'primary_exact15',
  quantile_type = 7,
  source_present = nrow(products) > 0) {
  categories = if (partition == 'primary_exact15') c(ANALYSIS_CATEGORIES[1:4],
      'Zwischenerzeugnisse <15 % vol', 'Zwischenerzeugnisse =15 % vol',
      'Zwischenerzeugnisse >15 % vol', 'Alkopops') else ANALYSIS_CATEGORIES
  labels = analysis_partition(products, partition)
  bind_rows(lapply(categories, \(category) {
        rows = products[which(labels == category), ]
        eligible = rows$price_analysis_eligible %in% T
        values = rows$price_per_reference_g[eligible]
        n = length(values)
        q = if (n) as.numeric(quantile(values, c(0.25, 0.5, 0.75), type =
                quantile_type)) else rep(NA_real_, 3)
        tibble(partition = partition, category = category, n_observations = n,
          n_unique_products = n_distinct(rows$product_key[eligible]),
          n_retailers = n_distinct(rows$retailer[eligible]), n_observed =
          nrow(rows),
          n_excluded = sum(!eligible), mean = if (n) mean(values) else NA_real_,
          median = q[2], q1 = q[1], q3 = q[3], iqr = q[3] - q[1],
          minimum = if (n) min(values) else NA_real_, maximum = if (n) max(
              values) else NA_real_,
          quantile_type = quantile_type, result_status = if (
            !source_present) 'no_raw_data' else
            if (
              !n) 'no_eligible_observations' else 'unweighted_observed_offers')
  }))
}

summarize_standard_prices = \(products) {
  bind_rows(lapply(ANALYSIS_CATEGORIES, \(category) {
        candidates = products[!is.na(products$analysis_category) &
            products$analysis_category == category, ]
        values = candidates$price_eur_per_10g[
          candidates$price_analysis_eligible]
        n = length(values)
        quartiles = if (n) as.numeric(quantile(values, c(0.25, 0.5, 0.75),
              type = 7)) else rep(NA_real_, 3)
        tibble(category = category, n_products_assigned = nrow(candidates),
          n = n,
          n_excluded_after_assignment = nrow(candidates) - n,
          mean_eur_per_10g = if (n) mean(values) else NA_real_,
          median_eur_per_10g = quartiles[2], q1_eur_per_10g = quartiles[1],
          q3_eur_per_10g = quartiles[3], iqr_eur_per_10g = quartiles[3] -
            quartiles[1],
          result_status = if (
            n) 'descriptive_unweighted' else
            'not_estimable_no_eligible_products')
  }))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) COMPUTE ONCE FOR THE UNION; KEEP THE TWO ELIGIBILITY DECISIONS EXPLICIT
# <-----------------------------------------------------------------------------

compute_conalco_prices = \(products, availability_policy = 'include_unknown',
  reference_alcohol_g = 10, ethanol_density_g_ml = ETHANOL_DENSITY_G_ML) {
  beverage = price_eligibility(products, availability_policy, 'beverage')
  research = price_eligibility(products, availability_policy,
    'alkopop_research')
  x = beverage
  x$beverage_price_eligible = beverage$price_analysis_eligible
  x$beverage_price_exclusion_reasons = beverage$price_exclusion_reasons
  x$research_price_eligible = research$price_analysis_eligible
  x$alkopop_price_eligible = research$price_analysis_eligible
  x$alkopop_price_exclusion_reasons = research$price_exclusion_reasons
  x$price_analysis_eligible = x$beverage_price_eligible |
    x$alkopop_price_eligible
  x = standardize_alcohol_price(x, reference_alcohol_g, ethanol_density_g_ml)
  x$standardized_price_eligible = x$price_analysis_eligible
  x$price_analysis_eligible = x$beverage_price_eligible
  x$price_exclusion_reasons = x$beverage_price_exclusion_reasons
  x$availability_policy = availability_policy
  x
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) NESTED LEGAL, OPERATIONAL AND EXPLORATORY POPULATIONS
# <-----------------------------------------------------------------------------

alkopop_group_membership = \(products, definition) {
  stopifnot(definition %in% c('legal', 'operational', 'exploratory',
      'general_rtd'))
  x = products
  legal = x$alkopop_legal_status == 'confirmed'
  operational = legal | x$alkopop_research_status == 'documented_spirit_rtd'
  exploratory = operational | x$alkopop_research_status == 'possible_alkopop'
  membership = switch(definition, legal = legal, operational = operational,
    exploratory = exploratory,
    general_rtd = x$alkopop_research_status == 'general_rtd_unknown_basis')
  # The exclusion is enforced again at the only entry point for all three
  # analyses.
  joint = x$joint_components %in% T & !is.na(x$joint_components_evidence) &
    nzchar(coalesce(x$joint_components_evidence, ''))
  membership & !x$fermented_only_documented & (!x$not_ready_documented |
      joint) &
    (is.na(x$alkopop_assessed_abv) | x$alkopop_assessed_abv > 1.2 &
      x$alkopop_assessed_abv < 10)
}

summarize_alkopop_prices = \(products, quantile_type = 7) {
  labels = c(legal = 'Gesetzlich bestätigte Alkopops',
    operational = 'Operationalisierte spirituosenhaltige RTD',
    exploratory = 'Explorative mögliche Alkopops',
    general_rtd = 'Allgemeine RTD unbekannter Basis – keine Alkopopgruppe')
  bind_rows(lapply(names(labels), \(definition) {
        rows = products[alkopop_group_membership(products, definition), ]
        rows$main_category = rep('Alkopops', nrow(rows))
        rows$price_analysis_eligible = rows$alkopop_price_eligible
        summarize_offer_prices(rows, quantile_type = quantile_type,
          source_present = nrow(products) > 0) %>%
          filter(category == 'Alkopops') %>% mutate(definition = definition,
          category = labels[[definition]])
  }))
}
