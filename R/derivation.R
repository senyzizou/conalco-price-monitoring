# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261012

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) OFFLINE DERIVATIONS OF IMMUTABLE CATALOGUES
# <-----------------------------------------------------------------------------

replay_catalog_products = \(source_directory) {
  paths = file.path(source_directory, c('products_INCOMPLETE.csv',
      'excluded_products.csv'))
  original = bind_rows(lapply(paths, read_observations))
  stopifnot(!anyDuplicated(original$retailer_product_id))
  rows = lapply(unique(original$raw_file), \(path) {
      old = original[original$raw_file == path, ]
      metadata = sub('[.]body$', '.json', path)
      archived = read_archive(metadata)
      doc = read_html(archived$body, encoding = 'UTF-8')
      if (old$source_format[1] == 'html_primary_listing_card') {
        parsed = catalog_parse_cards(metadata, old$retailer_category[1],
          old$category_url[1], old$category_page[1])
        parsed = parsed[match(old$retailer_product_id,
            parsed$retailer_product_id), ]
        cards = xml_find_all(doc,
          '//*[@data-listing="true"]//*[@data-product-information]')
        ids = vapply(cards, \(card) fromJSON(xml_attr(card,
              'data-product-information'))$id, character(1))
        descriptions = vapply(cards[match(old$retailer_product_id, ids)],
          \(card)
          node_text_verbatim(card, paste0('.//*[', class_xpath(
                'product-description'), ']')), character(1))
      } else {
        stopifnot(nrow(old) == 1)
        parsed = catalog_detail(metadata, old)
        descriptions = node_text_verbatim(doc,
          paste0('//*[', class_xpath('product-detail-description-text'), ']'))
      }
      parsed = attach_conalco_description_evidence(parsed, descriptions)
      stopifnot(identical(parsed$retailer_product_id, old$retailer_product_id))
      # Preserve consolidated category memberships, not just the selected card
      # category.
      parsed$retailer_category = old$retailer_category
      parsed$category_urls = old$category_urls
      parsed$source_validation_flags = old$validation_flags
      parsed
  })
  x = bind_rows(rows)
  stopifnot(nrow(x) == nrow(original), setequal(x$retailer_product_id,
      original$retailer_product_id))
  x$run_status = 'partial'
  x$catalog_coverage_status = 'reachable_categories_only_not_entire_shop'
  x$derivation_version = DERIVATION_VERSION
  x %>% classify_products() %>% quality_checks() %>% assign_price_categories()
}

replay_pilot_products = \(manifest_path) {
  manifest = read_json(manifest_path)
  stopifnot(identical(manifest$purpose, 'pilot'), !isTRUE(manifest$aborted))
  rows = lapply(manifest$product_responses, \(entry) {
      x = parse_conalco(entry$metadata_file, scalar(
          entry$retailer_product_id), scalar(entry$beverage_group))
      archived = read_archive(entry$metadata_file)
      doc = read_html(archived$body, encoding = 'UTF-8')
      x = attach_conalco_description_evidence(x, node_text_verbatim(doc,
          paste0('//*[', class_xpath('product-detail-description-text'), ']')))
      x$designation_raw = catalog_designation(x$archived_description_raw)
      alcohol = catalog_abv(x$archived_description_raw, x$product_name)
      x$alcohol_content = alcohol$value
      x$alcohol_content_raw = alcohol$raw
      x$alcohol_content_source = alcohol$source
      x$abv = alcohol$value
      x$abv_qualifier = alcohol$qualifier
      x$run_status = manifest$status
      x$derivation_version = DERIVATION_VERSION
      x
  })
  bind_rows(rows) %>% classify_products() %>% quality_checks() %>%
    assign_price_categories()
}

node_text_verbatim = \(node, xpath) {
  found = xml_find_first(node, xpath)
  if (inherits(found, 'xml_missing')) return(NA_character_)
  if (!xml_length(found)) return(xml_text(found))
  # Block tags also delimit evidence sections in detail-page HTML.
  markup = gsub('(<br\\b[^>]*>|</(?:p|div|h[1-6]|li)>)', '\\1\n\n',
    as.character(found), perl = T, ignore.case = T)
  xml_text(read_html(markup, encoding = 'UTF-8'))
}

attach_conalco_description_evidence = \(products, descriptions) {
  x = products
  sections = lapply(descriptions, conalco_evidence_sections)
  x$archived_description_raw = vapply(descriptions, clean_text, character(1))
  x$product_description_evidence = vapply(sections, \(s) s$production,
    character(1))
  x$ingredients_raw = vapply(sections, \(s) s$ingredients, character(1))
  x$recipe_section_removed = vapply(sections, \(s) s$recipe_removed, logical(1))
  x$product_evidence_source = 'same_observation_archived_product_description'
  x
}

archived_product_descriptions = \(products) {
  descriptions = rep(NA_character_, nrow(products))
  paths = unique(products$raw_file)
  for (path in paths) {
    ix = which(products$raw_file == path)
    archived = read_archive(sub('[.]body$', '.json', path))
    doc = read_html(archived$body, encoding = 'UTF-8')
    if (products$source_format[ix[1]] == 'html_primary_listing_card') {
      cards = xml_find_all(doc,
        '//*[@data-listing="true"]//*[@data-product-information]')
      ids = vapply(cards, \(card) fromJSON(
          xml_attr(card, 'data-product-information'))$id, character(1))
      stopifnot(all(products$retailer_product_id[ix] %in% ids))
      selected = match(products$retailer_product_id[ix], ids)
      descriptions[ix] = vapply(cards[selected], \(card) class_text(card,
          'product-description'), character(1))
    } else {
      stopifnot(length(ix) == 1)
      descriptions[ix] = node_text(doc, paste0('//*[', class_xpath(
            'product-detail-description-text'), ']'))
    }
  }
  descriptions
}

derive_conalco = \(source_directory) {
  paths = file.path(source_directory, c('products_INCOMPLETE.csv',
      'excluded_products.csv'))
  stopifnot(all(file.exists(paths)))
  original = bind_rows(lapply(paths, read_observations))
  stopifnot(!anyDuplicated(paste(original$retailer,
        original$retailer_product_id)))
  ids = unique(original$run_id)
  stopifnot(length(ids) == 1, startsWith(ids, 'catalog_'), all(
      original$retailer == 'conalco'))
  manifests = list.files(run_dir(ids), pattern = '^manifest.*[.]json$',
    full.names = T)
  matching = Filter(\(p) {
      m = read_json(p)
      !is.null(m$observation_file) && identical(normalizePath(
          m$observation_file), normalizePath(paths[1]))
    }, manifests)
  if (length(matching) != 1) stop(paste0(
        "Expected exactly one genuine catalogue manifest ",
        "for the source CSV"))
  manifest = read_json(matching[[1]])
  stopifnot(identical(manifest$purpose, 'catalog'), !isTRUE(manifest$aborted))
  for (p in paths) {
    expected = manifest$exports[[basename(p)]]
    if (is.null(expected) || !identical(digest(file = p, algo = 'sha256'),
        expected)) stop('Source CSV checksum mismatch: ', p)
  }
  x = original
  membership_path = file.path(source_directory, 'category_memberships.csv')
  if (file.exists(membership_path)) {
    memberships = read_observations(membership_path) %>%
      distinct(retailer_product_id, category_url) %>%
      count(retailer_product_id, name = 'category_membership_count')
    x = x %>% left_join(memberships, by = 'retailer_product_id',
      relationship = 'one-to-one')
  }
  x$source_parser_version = x$parser_version
  x$source_validation_flags = x$validation_flags
  # Keep product composition separate from broad recipe text used for No/Lo
  # labels.
  x$archived_description_raw = archived_product_descriptions(x)
  # Repair only the ambiguous pack marker; preserve four conflicting
  # declarations.
  rows = which(x$pack_basis == 'unclear_pack')
  for (i in rows) {
    pack = parse_catalog_pack(x$product_name[i], x$volume_l[i])
    if (pack$basis == 'single_unit_no_pack_marker') {
      x$pack_count[i] = pack$pack_count
      x$unit_volume_l[i] = pack$unit_volume_l
      x$pack_basis[i] = pack$basis
    }
  }
  x = x %>% classify_products() %>% standardize_products() %>%
    quality_checks() %>%
    mutate(derivation_version = DERIVATION_VERSION,
    parser_version = CATALOG_VERSION,
    run_status = 'partial', catalog_coverage_status =
      'reachable_categories_only_not_entire_shop')
  stopifnot(identical(x$retailer_category, original$retailer_category),
    identical(x$raw_sha256, original$raw_sha256), identical(x$alcohol_content,
      original$alcohol_content))
  x = assign_price_categories(x)
  changes = tibble(
    retailer_product_id = x$retailer_product_id, product_name = x$product_name,
    previous_group = original$beverage_group, beverage_group = x$beverage_group,
    previous_scope = original$scope_status, scope_status = x$scope_status,
    previous_no_lo = original$no_lo_status, no_lo_status = x$no_lo_status,
    classification_rule = x$classification_rule, scope_rule = x$scope_rule,
    previous_pack_basis = original$pack_basis, pack_basis = x$pack_basis,
    previously_exported = original$scope_status != 'excluded_nonfamily_product'
  ) %>% mutate(changed = previous_group != beverage_group |
      previous_scope != scope_status | previous_no_lo != no_lo_status |
      previous_pack_basis != pack_basis)
  list(products = x, changes = changes, source_paths = c(paths, matching[[1]],
      if (file.exists(membership_path)) membership_path,
      unique(original$raw_file), sub('[.]body$', '.json', unique(
          original$raw_file))))
}

example_products = \(x) {
  included = x %>% filter(scope_status == 'included')
  family = included %>% arrange(product_key) %>% group_by(beverage_group) %>%
    slice_head(n = 2) %>% ungroup()
  no_lo = included %>% filter(no_lo_status %in% no_lo_levels) %>%
    arrange(product_key) %>% slice_head(n = 6)
  corrected = included %>% filter(grepl('Lagerkorn|Calvados',
      product_name)) %>% slice_head(n = 4)
  bind_rows(family, no_lo, corrected) %>% distinct(product_key, .keep_all =
      T) %>%
    select(retailer, retailer_product_id, ean_gtin, product_name, brand,
    retailer_category,
    beverage_group, scope_status, no_lo_status, alcohol_content,
    alcohol_content_raw,
    volume_l, pack_count, unit_volume_l, price_current, price_regular,
    price_reference,
    price_reference_type, price_per_l, price_per_l_displayed, deposit,
    deposit_basis,
    price_scope, product_url, observed_at, run_id, parser_version,
    classification_version,
    classification_rule, scope_rule, validation_flags)
}

publish_derivation = \(result) {
  x = result$products
  stopifnot(length(unique(x$run_id)) == 1)
  directory = file.path('data/processed/reclassified', x$run_id[1],
    paste(DERIVATION_VERSION, CLASSIFICATION_VERSION, sep = '_'))
  if (dir.exists(directory)) stop(paste0(
        "Existing immutable derivation; bump the version ", "for any change"))
  ensure_dir(directory)
  snapshot = snapshot_source(directory)
  write_csv_once(x, file.path(directory, 'all_products.csv'))
  write_csv_once(x %>% filter(scope_status != 'excluded_nonfamily_product'),
    file.path(directory,
      'products_INCOMPLETE.csv'))
  write_csv_once(x %>% filter(scope_status == 'included'), file.path(
      directory, 'analysis_included.csv'))
  write_csv_once(x %>% filter(scope_status == 'unclear_review'), file.path(
      directory,
      'review_required.csv'))
  write_csv_once(x %>% filter(scope_status == 'excluded_nonfamily_product'),
    file.path(directory,
      'excluded_products.csv'))
  write_csv_once(example_products(x), file.path(directory, 'examples.csv'))
  write_csv_once(result$changes, file.path(directory,
      'classification_changes.csv'))
  write_csv_once(result$changes %>% filter(previously_exported &
        (previous_no_lo %in% no_lo_levels | previous_no_lo ==
          'no_lo_candidate_category')),
    file.path(directory, 'previous_no_lo_review.csv'))
  write_csv_once(x %>% count(beverage_group, scope_status, no_lo_status),
    file.path(directory,
      'group_counts.csv'))
  write_csv_once(flag_summary(x), file.path(directory, 'validation_counts.csv'))
  write_csv_once(classification_rules, file.path(directory,
      'classification_rules.csv'))
  files = list.files(directory, pattern = '[.]csv$', full.names = T)
  summary = list(n_source = nrow(x), n_included = sum(x$scope_status ==
        'included'),
    n_unclear = sum(x$scope_status == 'unclear_review'), n_excluded =
    sum(x$scope_status == 'excluded_nonfamily_product'),
    n_group_changes = sum(result$changes$previous_group !=
        result$changes$beverage_group),
    n_scope_changes = sum(result$changes$previous_scope !=
        result$changes$scope_status),
    n_relevant_no_lo = sum(x$scope_status == 'included' & x$no_lo_status %in%
        no_lo_levels),
    n_unclear_no_lo = sum(x$scope_status == 'unclear_review' &
        x$no_lo_status %in% no_lo_levels))
  hashes = \(paths) lapply(paths, \(p) list(path = p, sha256 = digest(file =
        p, algo = 'sha256')))
  write_json_once(list(run_id = x$run_id[1], derived_at = utc_now(), status =
      'partial_catalog',
      parser_version = unique(x$parser_version), derivation_version =
      DERIVATION_VERSION,
      classification_version = CLASSIFICATION_VERSION, summary = summary,
      source_snapshot = snapshot,
      inputs = hashes(result$source_paths), outputs = hashes(files)),
    file.path(directory, 'manifest.json'))
  cat(directory, '\n')
  print(summary)
  invisible(directory)
}

read_verified_derivation = \(directory) {
  manifest = read_json(file.path(directory, 'manifest.json'))
  stopifnot(identical(manifest$classification_version, CLASSIFICATION_VERSION))
  stopifnot(identical(digest(file = manifest$source_snapshot$path, algo =
          'sha256'),
      manifest$source_snapshot$sha256))
  for (entry in c(manifest$inputs, manifest$outputs)) {
    stopifnot(identical(digest(file = entry$path, algo = 'sha256'),
        entry$sha256))
  }
  x = read_observations(file.path(directory, 'all_products.csv'))
  stopifnot(all(x$classification_version == CLASSIFICATION_VERSION),
    !anyDuplicated(x$observation_key))
  x
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) EXPLICIT, CONSISTENTLY CLASSIFIED LONGITUDINAL SOURCES
# <-----------------------------------------------------------------------------

build_verified_catalog_panel = \(directories) {
  rows = lapply(directories, read_verified_derivation)
  x = bind_rows(rows) %>% arrange(observed_at, product_key)
  stopifnot(!anyDuplicated(x$observation_key), all(x$run_status == 'partial'),
    all(startsWith(x$run_id, 'catalog_')), all(x$retailer == 'conalco'))
  hash = substr(digest(x, algo = 'sha256'), 1, 16)
  path = file.path('data/processed/exports', paste0(
      'verified_catalog_panel_', CLASSIFICATION_VERSION,
      '_', hash, '.csv'))
  if (!file.exists(path)) {
    write_csv_once(x, path)
    write_json_once(list(classification_version = CLASSIFICATION_VERSION,
        source_manifests = lapply(directories, \(p) list(path = file.path(
              p, 'manifest.json'),
            sha256 = digest(file = file.path(p, 'manifest.json'), algo =
                'sha256'))),
        sha256 = digest(file = path, algo = 'sha256'), n_rows = nrow(x),
        n_runs = length(unique(x$run_id)), coverage =
          'partial_reachable_catalogues'), paste0(path, '.json'))
  }
  list(path = path, n_rows = nrow(x), n_runs = length(unique(x$run_id)))
}
