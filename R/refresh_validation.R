# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261016

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) VALIDATE COLLECTED EVIDENCE WITHOUT RE-SCRAPING OR HISTORICAL REPLAY
# <-----------------------------------------------------------------------------

refresh_file_record = \(path) list(path = path,
  sha256 = digest(file = path, algo = 'sha256'))

refresh_navigation_children = \(doc, category_url) {
  active = xml_find_all(doc, paste0('//a[', class_xpath(
        'category-navigation-link'),
      ' and @aria-current="page"]'))
  active = active[vapply(xml_attr(active, 'href'), catalog_url, character(
        1)) %in% category_url]
  descendants = unlist(lapply(active, \(node) xml_attr(xml_find_all(
          xml_parent(node), './/ul//a[@href]'), 'href')))
  tiles = if (!length(xml_find_all(doc, '//*[@data-listing="true"]')))
    xml_find_all(doc, paste0('//main//a[@href and ',
        "not(ancestor::*[@data-product-information]) and ", "not(ancestor::*[",
        class_xpath('category-navigation'), ']) and not(ancestor::*[',
        class_xpath('breadcrumb'), '])]')) else list()
  tile_urls = if (length(tiles)) xml_attr(tiles, 'href') else character()
  urls = vapply(c(descendants, tile_urls), catalog_url, character(1))
  unique(urls[!is.na(urls) & urls != category_url])
}

refresh_progress_export = \(id, output_root =
  'logs/conalco_refresh_2026-10-16') {
  directory = run_dir(id)
  state = readRDS(file.path(directory, 'checkpoint.rds'))
  stamp = format(Sys.time(), '%Y%m%dT%H%M%S', tz = 'UTC')
  output = ensure_dir(file.path(output_root, paste0(
        'progress_', stamp)))
  coverage = refresh_category_proof(state)
  ids = unique(unlist(lapply(state$pages, `[[`, 'ids')))
  metadata = lapply(list.files(directory, '^[0-9]+_[a-z]+[.]json$',
      full.names = T),
    read_json, simplifyVector = T)
  requests = bind_rows(lapply(metadata, \(m) data.frame(
        request_url = m$request_url, request_type = m$request_type,
        response_status = if (is.null(
            m$response_status)) NA_integer_ else m$response_status,
        observed_at_utc = m$observed_at_utc, error = scalar(m$error),
        metadata_file = m$metadata_file)))
  for (i in seq_len(nrow(coverage))) {
    attempts = requests[requests$request_type == 'category' &
        sub('[?].*$', '', requests$request_url) == coverage$category_url[i], ]
    coverage$n_category_request_attempts[i] = nrow(attempts)
    coverage$n_failed_attempts[i] = sum(is.na(attempts$response_status) |
        attempts$response_status != 200)
    if (coverage$successful_pages[i] == 0 && nrow(attempts) &&
        coverage$n_failed_attempts[i] > 0) {
      coverage$category_status[i] = 'failed_category_request'
      reasons = attempts$error[!is.na(attempts$error)]
      coverage$reason[i] = if (length(reasons)) tail(reasons, 1) else
        paste('Unresolved HTTP status', paste(unique(
              attempts$response_status), collapse = ';'))
    }
  }
  listings = bind_rows(lapply(state$pages, \(p) readRDS(p$parsed_file)$rows))
  products = listings[!duplicated(listings$retailer_product_id), ]
  products$run_status = 'partial'
  products$catalog_coverage_status = 'incomplete_defined_relevant_scope'
  products$classification_review_status =
    'provisional_existing_rules_not_final_audit'
  products$analysis_version = 'not_calculated_partial_collection'
  memberships = unique(listings[c('retailer_product_id', 'retailer_category',
        'category_url',
        'category_page', 'listing_metadata_file')])
  for (i in seq_len(nrow(coverage))) {
    category_ids = unique(memberships$retailer_product_id[
        memberships$category_url == coverage$category_url[i]])
    coverage$n_unique_observed_ids[i] = length(category_ids)
    coverage$n_embedded_product_descriptions[i] = sum(
      products$retailer_product_id %in% category_ids &
        products$detail_coverage_method ==
        'same_product_full_description_embedded_in_listing')
    coverage$n_individual_details_processed[i] = sum(category_ids %in%
        names(state$details))
  }
  if (is.null(state$last_error)) {
    events = file.path(directory, 'progress.jsonl')
    if (file.exists(events)) state$last_error = fromJSON(tail(readLines(
          events), 1))$reason
  }
  for (name in c('coverage', 'requests')) {
    x = get(name)
    x$run_id = rep(id, nrow(x))
    x$classification_version = rep(CLASSIFICATION_VERSION, nrow(x))
    x$analysis_version = rep('not_calculated_incomplete_collection', nrow(x))
    x$progress_as_of_utc = rep(utc_now(), nrow(x))
    write_csv_once(x, file.path(output, paste0(name, '.csv')))
  }
  write_csv_once(data.frame(retailer_product_id = ids, run_id = id,
      classification_version = CLASSIFICATION_VERSION,
      analysis_version = 'not_calculated_incomplete_collection'), file.path(
      output, 'product_ids.csv'))
  for (name in c('products', 'memberships')) {
    x = get(name)
    x$run_id = rep(id, nrow(x))
    x$classification_version = rep(CLASSIFICATION_VERSION, nrow(x))
    x$analysis_version = rep('not_calculated_partial_collection', nrow(x))
    x$progress_as_of_utc = rep(utc_now(), nrow(x))
    write_csv_once(x, file.path(output, paste0(name, '_PARTIAL.csv')))
  }
  write_json_once(list(run_id = id, status = 'partial', stage = state$stage,
      progress_at_utc = utc_now(), unique_product_ids = length(ids),
      processed_category_pages = length(state$pages), discovered_categories =
      nrow(state$queue),
      successful_http_200 = sum(requests$response_status == 200, na.rm = T),
      responses_other_http_status = sum(!is.na(requests$response_status) &
          requests$response_status != 200),
      transport_failures = sum(is.na(requests$response_status)),
      last_error = state$last_error, incomplete_categories =
      coverage$category_url[
        coverage$category_status != 'complete_observed_pagination'],
      resume_command = paste0(
        'Rscript scripts/start_conalco_refresh.R --run-id=', id),
      previous_partial_run_removed = F, price_statistics_calculated = F),
    file.path(output, 'progress_manifest.json'))
  output
}

refresh_auto_navigation_review = \(state) {
  review = data.frame(category_url = character(), decision = character(),
    reason = character(), metadata_sha256 = character())
  for (page in state$pages) {
    if (!page$url %in% state$queue$category_url || page$page != 1L ||
        !page$status %in% c('no_primary_listing', 'duplicate_ids_on_page'))
      next
    archived = read_archive(page$metadata_file)
    doc = read_html(archived$body, encoding = 'UTF-8')
    linked = refresh_navigation_children(doc, page$url)
    parsed = readRDS(page$parsed_file)
    # Use the existing scope decisions; product tiles are proved separately.
    children = intersect(linked, parsed$links$category_url[
        parsed$links$decision == 'crawl'])
    cards = xml_find_all(doc, '//main//*[@data-product-information]')
    slider_cards = xml_find_all(doc, paste0('//main//*[',
      class_xpath('cms-element-product-slider'),
      ']//*[@data-product-information]'))
    static_gallery = length(cards) > 0L &&
      length(slider_cards) == length(cards)
    pagination = xml_find_all(doc, paste0('//main//input[@name="p"] | ',
      '//main//a[contains(@href,"?p=")]'))
    cms = xml_find_all(doc, '//main//*[contains(@class,"cms-block")]')
    missing_listing = xml_find_all(doc, paste0('//main//*[',
      class_xpath('cms-block-product-listing'), ' or ',
      class_xpath('cms-element-product-listing'), ']'))
    if (archived$meta$response_status != 200L ||
        !length(cms) || length(missing_listing) || length(pagination) ||
        length(xml_find_all(doc, '//*[@data-listing="true"]')) ||
        (!length(children) && !static_gallery) ||
        !all(children %in% state$queue$category_url)) next
    direct = linked[grepl('-vol|[0-9][lL](?:-|$)', linked, perl = T)]
    if (length(direct)) {
      observed = unique(unlist(lapply(state$pages, \(entry)
        readRDS(entry$parsed_file)$rows$product_url)))
      if (!all(direct %in% observed)) next
    }
    review = bind_rows(review, data.frame(category_url = page$url,
      decision = 'navigation_container_verified',
      reason = paste('Current HTTP 200 navigation container with',
        length(children),
        'in-scope child links;', length(direct),
        'direct product tiles observed fresh; static gallery =',
        static_gallery, '; all visible cards checked at validation'),
      metadata_sha256 = digest(file = page$metadata_file, algo = 'sha256')))
  }
  review
}

refresh_navigation_changes = \(id, state, exhausted = F) {
  current = refresh_category_proof(state)
  previous = data.frame(category_name = character(),
    category_url = character(), category_id = character())
  reference = NULL
  reference_date = '0000-00-00'
  manifests = list.files('data/raw/conalco', '^manifest[.]json$',
    recursive = T, full.names = T)
  today = berlin_date(read_json(file.path(run_dir(id),
        'started.json'))$started_at_utc)
  for (path in rev(sort(manifests))) {
    candidate = read_json(path)
    if (!identical(candidate$status, 'complete') ||
        is.null(candidate$proof$category_completeness) ||
        berlin_date(candidate$started_at_utc) >= today) next
    entry = candidate$proof$category_completeness
    if (!file.exists(entry$path) || !identical(digest(file = entry$path,
          algo = 'sha256'), entry$sha256)) next
    previous = read.csv(entry$path, stringsAsFactors = F,
      colClasses = 'character', na.strings = '')
    reference = refresh_file_record(path)
    reference_date = berlin_date(candidate$started_at_utc)
    break
  }
  # A partial day still proves the presence of its positively linked URLs.
  starts = rev(sort(list.files('data/raw/conalco', '^started[.]json$',
    recursive = T, full.names = T)))
  for (path in starts) {
    date = berlin_date(read_json(path)$started_at_utc)
    checkpoint = file.path(dirname(path), 'checkpoint.rds')
    if (length(date) != 1L || is.na(date) || date >= today ||
        date <= reference_date || !file.exists(checkpoint)) next
    prior = readRDS(checkpoint)
    if (!isTRUE(prior$dynamic_navigation)) next
    prior$queue = refresh_current_queue(dirname(path), prior$pages)
    previous = refresh_category_proof(prior)
    reference = refresh_file_record(checkpoint)
    reference_date = date
    break
  }
  containers = refresh_auto_navigation_review(state)$category_url
  changes = lapply(seq_len(nrow(current)), \(i) {
    row = current[i, ]
    ix = which(previous$category_url == row$category_url)
    identity = which(!is.na(previous$category_id) &
      !is.na(row$category_id) & previous$category_id == row$category_id &
      !previous$category_url %in% current$category_url)
    status = if (length(ix)) {
      if (identical(previous$category_name[ix], row$category_name))
        'retained' else 'renamed_label'
    } else if (length(identity) == 1L &&
        sum(current$category_id == row$category_id, na.rm = T) == 1L) {
      ix = identity
      'url_changed_same_category_id'
    } else 'new_in_navigation'
    if (row$category_status == 'unresolved' &&
        !row$category_url %in% containers)
      status = 'linked_fetch_gap'
    data.frame(previous_url = if (length(ix)) previous$category_url[ix] else NA,
      current_url = row$category_url, category_name = row$category_name,
      category_id = row$category_id, status = status)
  })
  changes = bind_rows(changes)
  missing = previous[!previous$category_url %in%
      c(current$category_url, changes$previous_url), ]
  if (nrow(missing)) changes = bind_rows(changes, data.frame(
    previous_url = missing$category_url, current_url = NA_character_,
    category_name = missing$category_name, category_id = missing$category_id,
    status = if (exhausted) 'removed_from_navigation' else
      'not_rediscovered_unresolved'))
  list(reference = reference, exhausted = exhausted, changes = changes)
}

refresh_store_partial = \(id, reason = NULL) {
  directory = run_dir(id)
  checkpoint = file.path(directory, 'checkpoint.rds')
  state = readRDS(checkpoint)
  metadata_paths = sort(list.files(directory, '^[0-9]+_[a-z]+[.]json$',
    full.names = T))
  code_paths = c('R/catalog.R', 'R/http.R', 'R/catalog_refresh.R',
    'R/refresh_validation.R', 'R/conalco_daily.R')
  code_paths = code_paths[file.exists(code_paths)]
  source = c(checkpoint, metadata_paths, code_paths)
  hashes = vapply(source, \(p) digest(file = p, algo = 'sha256'), character(1))
  key = substr(digest(hashes, algo = 'sha256'), 1, 20)
  output = ensure_dir(file.path('data/processed/catalog_refresh', id,
      'partial', key))
  manifest_path = file.path(output, 'manifest.json')
  if (file.exists(manifest_path)) {
    receipt = read_json(manifest_path)
    for (entry in receipt$proof) stopifnot(identical(
      digest(file = entry$path, algo = 'sha256'), entry$sha256))
    return(manifest_path)
  }
  listings = bind_rows(lapply(state$pages,
    \(p) readRDS(p$parsed_file)$rows))
  if (!nrow(listings)) listings = data.frame(retailer_product_id = character(),
    observed_at = character(), category_url = character())
  dates = unique(berlin_date(listings$observed_at))
  expected = berlin_date(read_json(file.path(directory,
        'started.json'))$started_at_utc)
  stopifnot(!anyNA(dates), all(dates == expected))
  products = listings[!duplicated(listings$retailer_product_id), , drop = F]
  for (i in seq_len(nrow(products))) {
    detail = state$details[[products$retailer_product_id[i]]]
    if (is.null(detail)) next
    for (field in setdiff(names(detail), names(products)))
      products[[field]] = NA
    products[i, names(detail)] = detail
  }
  stopifnot(all(berlin_date(products$observed_at) == expected))
  products$run_status = rep('partial', nrow(products))
  products$catalog_coverage_status = rep('incomplete_defined_relevant_scope',
    nrow(products))
  products$classification_review_status = rep('provisional_not_final_audit',
    nrow(products))
  products$analysis_version = rep('not_calculated_partial_collection',
    nrow(products))
  for (path in unique(listings$listing_metadata_file)) read_archive(path)
  requests = bind_rows(lapply(metadata_paths, \(path) {
    meta = read_json(path)
    data.frame(request_url = meta$request_url,
      response_status = if (is.null(meta$response_status)) NA else
        meta$response_status, observed_at_utc = meta$observed_at_utc,
      metadata_file = path)
  }))
  failures = bind_rows(lapply(state$failures, as.data.frame))
  if (!ncol(failures)) failures = data.frame(url = character(),
    page = integer(), kind = character(), response_status = integer(),
    observed_at_utc = character(), recorded_at_utc = character(),
    metadata_file = character(), reason = character())
  navigation = refresh_navigation_changes(id, state, exhausted = F)
  tables = list(observations_PARTIAL = products,
    memberships_PARTIAL = unique(listings[intersect(c('retailer_product_id',
      'category_url', 'category_page', 'observed_at', 'listing_metadata_file'),
      names(listings))]), coverage = refresh_category_proof(state),
    requests = requests, failures = failures,
    navigation_changes = navigation$changes)
  proof = lapply(names(tables), \(name) {
    path = file.path(output, paste0(name, '.csv.gz'))
    pending = tempfile('.partial_', tmpdir = output)
    on.exit(unlink(pending), add = T)
    local({
      p = pending
      connection = gzfile(p, 'wt', compression = 6)
      on.exit(close(connection))
      write.csv(tables[[name]], connection, row.names = F, na = '')
    })
    if (file.exists(path)) {
      stopifnot(identical(digest(file = path, algo = 'sha256'),
          digest(file = pending, algo = 'sha256')))
    } else if (!file.rename(pending, path)) stop('Cannot commit partial table')
    refresh_file_record(path)
  })
  names(proof) = names(tables)
  source_path = file.path(output, 'collection_source/source_snapshot.json')
  if (!file.exists(source_path))
    snapshot_source(ensure_dir(dirname(source_path)))
  proof$collection_source = refresh_file_record(source_path)
  write_json_once(list(run_id = id, status = 'partial', date = expected,
      catalog_coverage_status = 'incomplete_defined_relevant_scope',
      n_products = nrow(products), n_pages = length(state$pages),
      reason = reason, checkpoint = refresh_file_record(checkpoint),
      reference_navigation = navigation$reference,
      price_statistics_calculated = F, proof = proof,
      refresh_version = CATALOG_REFRESH_VERSION), manifest_path)
  manifest_path
}

refresh_decode_pagination_pass = \(pass, url, order = NULL) {
  entries = lapply(pass, \(page) {
    metadata = read_json(page$metadata_file)
    stopifnot(metadata$response_status == 200L)
    parsed = refresh_page(metadata, 'Pagination proof', url, page$page)
    stopifnot(!is.null(parsed$pagination),
      parsed$pagination$current == page$page,
      identical(as.character(parsed$rows$retailer_product_id),
        as.character(unlist(page$ids, use.names = F))))
    if (!is.null(order)) stopifnot(
      identical(order, 'name-asc'),
      identical(refresh_query_value(metadata$request_url, 'order'), order),
      refresh_order_evidence(page$metadata_file, T))
    checks = catalog_page_checks(parsed, page$page, parsed$pagination$last,
      list(), url)
    page$status = catalog_page_status(checks$good, checks$empty_verified,
      parsed)
    page$last = parsed$pagination$last
    page$category_id = parsed$category_id
    page$terminal_page_verified = parsed$terminal_page_verified
    list(page = page, rows = parsed$rows)
  })
  pages = lapply(entries, `[[`, 'page')
  sources = vapply(pass, `[[`, '', 'metadata_file')
  rows = bind_rows(lapply(entries, `[[`, 'rows'))
  list(pages = pages, rows = rows, sources = sources)
}

# Mandatory offer fields and source-supported unknown measurements.
refresh_reference_fields = \(rows) {
  required = c('retailer_product_id', 'product_name', 'product_url',
    'price_current', 'volume_l', 'alcohol_content', 'pack_count',
    'scope_status', 'no_lo_status', 'pack_basis')
  if (!all(required %in% names(rows))) return(F)
  text = c('retailer_product_id', 'product_name', 'product_url')
  if (anyNA(rows[text]) || any(!vapply(rows[text], \(x)
      all(nzchar(trimws(x))), logical(1)))) return(F)
  if (any(is.nan(rows$volume_l)) || any(is.nan(rows$alcohol_content)) ||
      any(is.nan(rows$pack_count))) return(F)
  known_abv = !is.na(rows$alcohol_content)
  known_pack = !is.na(rows$pack_count)
  known_volume = !is.na(rows$volume_l)
  all(is.finite(rows$price_current) & rows$price_current >= 0) &&
    all(known_volume | rows$scope_status %in% 'excluded_nonfamily_product') &&
    all(is.finite(rows$volume_l[known_volume]) &
      rows$volume_l[known_volume] > 0) &&
    all(known_abv | rows$scope_status %in% 'excluded_nonfamily_product' |
      rows$no_lo_status %in% 'alcohol_free_label') &&
    all(is.finite(rows$alcohol_content[known_abv]) &
      rows$alcohol_content[known_abv] >= 0 &
      rows$alcohol_content[known_abv] <= 100) &&
    all(known_pack | rows$pack_basis %in% 'unclear_pack') &&
    all(is.finite(rows$pack_count[known_pack]) &
      rows$pack_count[known_pack] > 0 &
      rows$pack_count[known_pack] == floor(rows$pack_count[known_pack]))
}

refresh_check_pagination_reviews = \(reviews) {
  sources = character()
  for (url in names(reviews)) {
    review = reviews[[url]]
    placements = !is.null(review$reference)
    stopifnot(isTRUE(review$verified), !isTRUE(review$exhausted),
      review$attempts >= if (placements) 1L else 2L,
      review$attempts <= 3L,
      length(review$passes) >= if (placements) 2L else 3L,
      length(review$passes) <= 4L)
    primary = lapply(tail(review$passes, 2L), refresh_decode_pagination_pass,
      url = url)
    signatures = lapply(primary, \(p)
      refresh_pagination_signature(p$pages, allow_overlap = placements))
    stopifnot(!is.null(signatures[[1]]), !is.null(signatures[[2]]),
      if (placements) identical(signatures[[1]]$category_id,
        signatures[[2]]$category_id) else
        identical(signatures[[1]], signatures[[2]]),
      !length(intersect(primary[[1]]$sources, primary[[2]]$sources)))
    used = unlist(lapply(review$passes, \(pass)
      vapply(pass, `[[`, '', 'metadata_file')), use.names = F)
    if (placements) {
      reference = review$reference
      stopifnot(identical(reference$order, 'name-asc'),
        length(reference$passes) == 2L,
        identical(reference$discovery_metadata, primary[[2]]$sources[1]),
        refresh_order_evidence(reference$discovery_metadata),
        !is.na(signatures[[1]]$category_id),
        anyDuplicated(unlist(signatures[[2]]$ids)) > 0L,
        all(vapply(primary, \(p) refresh_placement_values(p$rows), logical(1))))
      independent = lapply(reference$passes, refresh_decode_pagination_pass,
        url = url, order = reference$order)
      control = lapply(independent, \(p)
        refresh_pagination_signature(p$pages))
      stopifnot(!is.null(control[[1]]),
        identical(control[[1]], control[[2]]),
        identical(signatures[[1]]$category_id, control[[1]]$category_id),
        all(unlist(lapply(signatures, `[[`, 'ids')) %in%
          unlist(control[[1]]$ids)),
        !length(intersect(independent[[1]]$sources, independent[[2]]$sources)))
      extra = !setequal(unlist(signatures[[1]]$ids),
        unlist(control[[1]]$ids))
      imported = extra || anyNA(refresh_boundary_ids(signatures[[1]]$ids)) ||
        !identical(signatures[[1]], signatures[[2]])
      stopifnot(if (imported)
        identical(as.integer(reference$accepted_pass), 1L) else
        is.null(reference$accepted_pass))
      observed = bind_rows(lapply(c(primary, independent), `[[`, 'rows'))
      stopifnot(refresh_placement_values(observed))
      for (pass in independent) stopifnot(refresh_reference_fields(pass$rows))
      for (field in c('volume_l', 'alcohol_content', 'pack_count'))
        stopifnot(identical(
          is.na(independent[[1]]$rows[[field]]),
          is.na(independent[[2]]$rows[[field]])))
      for (id in unique(observed$retailer_product_id)) stopifnot(
        length(unique(observed$product_url[
          observed$retailer_product_id == id])) == 1L)
      stopifnot(!length(intersect(used,
        unlist(lapply(independent, `[[`, 'sources')))))
      used = c(used, unlist(lapply(independent, `[[`, 'sources')))
    }
    metadata = lapply(unique(used), read_json)
    runs = vapply(metadata, `[[`, '', 'run_id')
    dates = berlin_date(vapply(metadata, `[[`, '', 'observed_at_utc'))
    stopifnot(length(unique(runs)) == 1L, !anyNA(dates),
      length(unique(dates)) == 1L,
      all(startsWith(used, paste0(run_dir(runs[1]), '/'))))
    sources = c(sources, used)
  }
  unique(sources)
}

validate_conalco_refresh = \(id, scope_review_path, scope_exclusion_path =
  NULL) {
  directory = run_dir(id)
  finished = read_json(file.path(directory, 'collection_finished.json'))
  state = readRDS(file.path(directory, 'checkpoint.rds'))
  reviews = state$pagination_reviews
  reviews = reviews[names(reviews) %in% state$queue$category_url]
  review_sources = refresh_check_pagination_reviews(reviews)
  stopifnot(state$stage %in% c('partial', 'complete_pending_scope_validation'))
  input = finished$output_directory
  if (isTRUE(state$dynamic_navigation)) {
    state$pages = Filter(\(p) p$url %in% state$queue$category_url, state$pages)
    scope_review_path = file.path(input, 'navigation_review_auto.csv')
    if (!file.exists(scope_review_path)) write_csv_once(
      refresh_auto_navigation_review(state), scope_review_path)
  }
  products = read_observations(file.path(input, 'products_collected.csv'))
  members = read.csv(file.path(input, 'category_memberships.csv'),
    colClasses = 'character')
  coverage = read.csv(file.path(input, 'category_completeness.csv'),
    stringsAsFactors = F)
  if ('category_id' %in% names(coverage)) {
    identities = refresh_category_proof(state)
    coverage$category_id = identities$category_id[match(coverage$category_url,
        identities$category_url)]
  }
  stopifnot(!anyDuplicated(products$retailer_product_id), all(
      products$run_id == id))
  pages = bind_rows(lapply(state$pages, \(p) {
        terminal = isTRUE(p$terminal_page_verified)
        if (is.null(p$terminal_page_verified) && !is.na(p$last) && p$page ==
            p$last) {
          doc = read_html(read_archive(p$metadata_file)$body, encoding =
              'UTF-8')
          terminal = !length(xml_find_all(doc, paste0(
                '//*[@data-listing="true"]//li[',
                class_xpath('page-next'),
                ']//input[@name="p" and not(@disabled)]')))
        }
        data.frame(category_url = p$url, page = p$page, expected_last_page =
          p$last,
          status = p$status, terminal_page_verified = terminal,
          metadata_file = p$metadata_file, stringsAsFactors = F)
  }))
  links = bind_rows(lapply(state$pages, \(p) {
        if (p$page != 1L) return(NULL)
        x = readRDS(p$parsed_file)$links
        x$discovered_on_category_url = rep(p$url, nrow(x))
        x$source_metadata_file = rep(p$metadata_file, nrow(x))
        x
  }))
  stopifnot(all(links$category_url[links$decision == 'crawl'] %in%
        coverage$category_url))
  review = read.csv(scope_review_path, colClasses = 'character', na.strings =
      '')
  exclusion_review = if (is.null(
      scope_exclusion_path)) NULL else refresh_file_record(scope_exclusion_path)
  excluded_urls = catalog_reviewed_exclusions(exclusion_review, id)
  coverage$in_scope = !coverage$category_url %in% excluded_urls
  pages$in_scope = !pages$category_url %in% excluded_urls
  links$original_decision = links$decision
  links$decision[links$category_url %in% excluded_urls] =
    'excluded_additional_brand_navigation_user_instruction'
  coverage$duplicate_featured_placements = 0L
  unresolved = which(coverage$category_status != 'complete_observed_pagination')
  for (i in unresolved) {
    url = coverage$category_url[i]
    category_pages = Filter(\(p) identical(p$url, url), state$pages)
    if (url %in% excluded_urls) {
      stopifnot(length(category_pages) == 1L, is.na(category_pages[[
              1]]$last),
        category_pages[[1]]$status == 'http_404_unresolved_category',
        !length(category_pages[[1]]$ids))
      coverage$category_status[i] = 'excluded_additional_brand_navigation'
      coverage$reason[i] = paste(paste0(
          "Explicit user scope decision: additional ",
          "brand-action navigation excluded;"),
        paste0("HTTP 404 retained as evidence; all observed ",
          "products remain included via regular categories"))
      coverage$expected_pages[i] = 0L
      coverage$successful_pages[i] = 0L
      coverage$total_count_evidence[i] = paste0(
        "Outside approved scope; HTTP 404 is not ",
        "evidence of empty stock")
      next
    }
    # A review can resolve a genuine navigation container, never missing
    # listing pages.
    stopifnot(length(category_pages) == 1L, is.na(category_pages[[1]]$last),
      category_pages[[1]]$status %in% c('no_primary_listing',
        'duplicate_ids_on_page'))
    ix = which(review$category_url == url & review$decision ==
        'navigation_container_verified')
    # A shared copy with sanitized cookie headers maps back to the reviewed
    # original hash (docs/conalco_metadata_sanitization.csv)
    actual_hash = digest(file = category_pages[[1]]$metadata_file,
      algo = 'sha256')
    sanitized_map = 'docs/conalco_metadata_sanitization.csv'
    if (file.exists(sanitized_map)) {
      sanitized = read.csv(sanitized_map, stringsAsFactors = F)
      jx = which(sanitized$sanitized_sha256 == actual_hash)
      if (length(jx) == 1L) actual_hash = sanitized$original_sha256[jx]
    }
    stopifnot(length(ix) == 1L, !is.na(review$reason[ix]), nzchar(
        review$reason[ix]),
      identical(review$metadata_sha256[ix], actual_hash))
    doc = read_html(read_archive(category_pages[[1]]$metadata_file)$body,
      encoding = 'UTF-8')
    stopifnot(!length(xml_find_all(doc, '//*[@data-listing="true"]')))
    placements = readRDS(category_pages[[1]]$parsed_file)$rows
    for (rows in split(placements, placements$retailer_product_id)) {
      for (field in c('product_name', 'price_current', 'volume_l',
          'alcohol_content', 'pack_count'))
        stopifnot(length(unique(rows[[field]][!is.na(rows[[field]])])) <=
            1L)
    }
    cards = xml_find_all(doc, '//main//*[@data-product-information]')
    ids = unique(vapply(cards, \(card) fromJSON(
          xml_attr(card, 'data-product-information'))$id, character(1)))
    stopifnot(all(ids %in% products$retailer_product_id),
      setequal(ids, unique(members$retailer_product_id[members$category_url ==
              url])))
    coverage$category_status[i] = 'navigation_container_verified'
    coverage$reason[i] = review$reason[ix]
    coverage$expected_pages[i] = 0L
    coverage$successful_pages[i] = 0L
    coverage$unique_product_ids[i] = length(ids)
    coverage$duplicate_featured_placements[i] = length(cards) - length(ids)
    coverage$total_count_evidence[i] = paste0(
      "Verified static navigation container; no ",
      "independent listing total supplied")
  }
  stopifnot(all(coverage$category_status %in%
        c('complete_observed_pagination', 'navigation_container_verified',
        'excluded_additional_brand_navigation')),
    all(coverage$unresolved_details == 0), all(!is.na(
        products$detail_coverage_method)))
  requests = lapply(list.files(directory, '^[0-9]+_[a-z]+[.]json$',
      full.names = T),
    read_json)
  for (i in seq_len(nrow(coverage))) {
    url = coverage$category_url[i]
    first = Filter(\(p) identical(p$url, url) && p$page == 1L,
      state$pages)[[1]]
    archived = read_archive(first$metadata_file)
    coverage$source_response_status[i] = archived$meta$response_status
    doc = read_html(archived$body, encoding = 'UTF-8')
    children = if (url %in%
        excluded_urls) character() else refresh_navigation_children(doc, url)
    children = intersect(children, coverage$category_url)
    coverage$subcategory_urls[i] = paste(children, collapse = ' | ')
    coverage$n_observed_subcategories[i] = length(children)
    ids = unique(members$retailer_product_id[members$category_url == url])
    coverage$n_failed_detail_attempts[i] = sum(vapply(requests, \(m)
        identical(m$request_type, 'product') && scalar(
          m$retailer_product_id) %in% ids &&
          (is.null(m$response_status) || m$response_status >= 400),
        logical(1)))
    coverage$n_failed_category_attempts[i] = sum(vapply(requests, \(m)
        identical(m$request_type, 'category') && sub('[?].*$', '',
          m$request_url) == url &&
          (is.null(m$response_status) || m$response_status >= 400),
        logical(1)))
    coverage$failed_or_unresolved_details[i] = 0L
    if (first$status == 'empty_category_verified') {
      coverage$reported_total_products[i] = 0L
      coverage$total_count_evidence[i] =
        'Explicit retailer message: Keine Produkte gefunden.'
    }
  }
  details = products[c('retailer_product_id', 'detail_coverage_method',
      'listing_metadata_file', 'detail_metadata_file')]
  details$detail_status = 'processed'
  # Check every actually used immutable raw response, not just an output CSV.
  sources = unique(c(pages$metadata_file[pages$in_scope],
      products$listing_metadata_file,
    products$detail_metadata_file, review_sources))
  sources = sources[!is.na(sources) & nzchar(sources)]
  stopifnot(all(startsWith(sources, paste0(directory, '/'))))
  raw_inventory = bind_rows(lapply(sources, \(path) {
        archived = read_archive(path)
        stopifnot(archived$meta$response_status == 200L,
          archived$meta$run_id == id)
        data.frame(metadata_file = path, raw_file = archived$meta$raw_file,
          raw_sha256 = archived$meta$raw_sha256,
          observed_at_utc = archived$meta$observed_at_utc, stringsAsFactors = F)
  }))
  output = ensure_dir(file.path(input, 'validated'))
  tables = list(category_completeness = coverage, page_completeness = pages,
    category_links = links, category_memberships = members,
    product_ids = data.frame(retailer_product_id =
        products$retailer_product_id),
    product_details = details, raw_inventory = raw_inventory, products =
      products)
  proof = list()
  if (length(reviews)) {
    path = file.path(output, 'pagination_reviews.json')
    write_json_once(reviews, path)
    proof$pagination_reviews = refresh_file_record(path)
  }
  if (isTRUE(state$dynamic_navigation)) {
    navigation = list.files(directory, '^[0-9]+_navigation[.]json$',
      full.names = T)
    changes = refresh_navigation_changes(id, state, exhausted = T)
    path = file.path(output, 'navigation_discovery.json')
    write_json_once(list(policy = 'current_navigation_v1',
        run_id = id, exhausted = T, root = refresh_file_record(navigation),
        current_urls = state$queue$category_url,
        reference = changes$reference, changes = changes$changes), path)
    proof$navigation_discovery = refresh_file_record(path)
  }
  for (name in names(tables)) {
    x = tables[[name]]
    if (!'run_id' %in% names(x)) x$run_id = rep(id, nrow(x))
    x$classification_version = rep(CLASSIFICATION_VERSION, nrow(x))
    x$analysis_version = rep('not_yet_calculated', nrow(x))
    x$collection_started_at_utc = rep(read_json(file.path(directory,
          'started.json'))$started_at_utc, nrow(x))
    x$collection_ended_at_utc = rep(finished$ended_at_utc, nrow(x))
    path = file.path(output, paste0(name, '.csv'))
    write_csv_once(x, path)
    proof[[name]] = refresh_file_record(path)
  }
  manifest = list(run_id = id, purpose = 'catalog_refresh', retailer =
    'conalco',
    status = 'complete', aborted = F,
    catalog_coverage_status = 'complete_defined_relevant_scope',
    started_at_utc = read_json(file.path(directory,
        'started.json'))$started_at_utc,
    ended_at_utc = finished$ended_at_utc, validated_at_utc = utc_now(),
    parser_version = CATALOG_VERSION, refresh_version = CATALOG_REFRESH_VERSION,
    scope = list(navigation_reviewed = T, discovery_exhausted = T,
      discovery_policy = if (isTRUE(state$dynamic_navigation))
        'current_navigation_v1' else 'legacy',
      definition = read_json(file.path(directory,
          'collection_scope.json'))$scope,
      exclusion_review = exclusion_review,
      approved_excluded_navigation_urls = excluded_urls,
      exclusion_effect = if (length(excluded_urls)) paste0(
          "Only two additional navigation pages excluded; ",
          "no observed product or regular category removed") else
        'No navigation URL excluded by a run-specific user scope decision',
      calendar_exception = paste0(
        "Alcohol-containing calendar category included; ",
        "heterogeneous packs remain identifiable"),
      external_total_limit = paste0(
        "No independent retailer-wide product total ",
        "supplied; completeness applies to the ",
        "explicitly documented relevant navigation scope")),
    n_unique_products = nrow(products), scope_review = refresh_file_record(
      scope_review_path),
    proof = proof, observation_file = proof$products$path)
  path = file.path(directory, 'manifest.json')
  write_json_once(manifest, path)
  require_complete_catalog(path)
  path
}

backup_complete_refresh = \(manifest_path) {
  manifest = require_complete_catalog(manifest_path)
  id = manifest$run_id
  roots = c(run_dir(id), dirname(manifest$proof$products$path))
  paths = unique(unlist(lapply(roots, \(root) list.files(root,
          recursive = T, full.names = T, all.files = T))))
  paths = paths[!dir.exists(paths)]
  destination = ensure_dir(file.path(
      'data/backups/conalco_refresh_2026-10-16', id))
  inventory = bind_rows(lapply(paths, \(path) {
        copy = file.path(destination, path)
        ensure_dir(dirname(copy))
        if (!file.exists(copy) && !file.copy(path, copy, copy.date =
              T)) stop('Backup failed: ', path)
        sha = digest(file = path, algo = 'sha256')
        stopifnot(identical(sha, digest(file = copy, algo = 'sha256')))
        data.frame(source_path = path, backup_path = copy, bytes = file.info(
            path)$size,
          sha256 = sha, stringsAsFactors = F)
  }))
  write_csv_once(inventory, file.path(destination, 'backup_inventory.csv'))
  receipt = list(run_id = id, verified = T, verified_at_utc = utc_now(),
    n_files = nrow(inventory), backup_directory = destination,
    inventory = refresh_file_record(file.path(destination,
        'backup_inventory.csv')),
    limitation = paste0("Verified local copy on the same Mac; no ",
      "external backup claimed"))
  write_json_once(receipt, paste0("logs/conalco_refresh_2026-10-16/",
      "new_catalog_backup_receipt.json"))
  invisible(receipt)
}

isolate_previous_partial_catalog = \(manifest_path) {
  manifest = require_complete_catalog(manifest_path)
  backup = read_json(paste0("logs/conalco_refresh_2026-10-16/",
      "new_catalog_backup_receipt.json"))
  stopifnot(isTRUE(backup$verified), identical(backup$run_id, manifest$run_id))
  old_id = 'catalog_20261012T092338_c2109ed57d1d'
  roots = file.path(c('data/raw/conalco', 'data/processed/catalog',
      'data/processed/reclassified'), old_id)
  stopifnot(all(dir.exists(roots)),
    identical(read_json(file.path(roots[1], 'manifest.json'))$status,
      'partial'))
  destination = ensure_dir(file.path('..', 'isolated_catalog_archives',
      paste0(old_id, '_isolated_', format(Sys.time(), '%Y%m%dT%H%M%S', tz =
            'UTC'))))
  destination = normalizePath(destination)
  stopifnot(!startsWith(destination, paste0(normalizePath('.'), '/')))
  paths = unlist(lapply(roots, \(root) list.files(root, recursive = T,
        full.names = T, all.files = T)))
  paths = paths[!dir.exists(paths)]
  inventory = bind_rows(lapply(paths, \(path) data.frame(original_path = path,
        isolated_path = file.path(destination, path), bytes = file.info(
          path)$size,
        sha256 = digest(file = path, algo = 'sha256'), stringsAsFactors = F)))
  # Publish the pre-removal inventory before copying or removing any source.
  write_csv_once(inventory, file.path(destination,
      'before_removal_inventory.csv'))
  for (i in seq_len(nrow(inventory))) {
    ensure_dir(dirname(inventory$isolated_path[i]))
    stopifnot(file.copy(inventory$original_path[i],
        inventory$isolated_path[i], copy.date = T),
      identical(digest(file = inventory$isolated_path[i], algo = 'sha256'),
        inventory$sha256[i]))
  }
  write_json_once(list(verified = T, n_files = nrow(inventory),
      checked_at_utc = utc_now()),
    file.path(destination, 'copy_verified_before_removal.json'))
  # The exact three run-specific allowlisted roots are now safely copied and
  # verified.
  for (root in roots) {
    stopifnot(basename(root) == old_id)
    if (unlink(root, recursive = T) != 0L || dir.exists(root)) stop(
        'Removal failed: ', root)
  }
  stopifnot(!any(dir.exists(roots)))
  receipt = list(verified = T, removed_at_utc = utc_now(), old_run_id = old_id,
    new_run_id = manifest$run_id, n_removed_files = nrow(inventory),
    removed_roots = roots,
    isolated_backup = destination,
    inventory = refresh_file_record(file.path(destination,
        'before_removal_inventory.csv')),
    active_registrations_removed = paste0(
      "Original raw manifest and every run-specific ",
      "processed manifest removed with the three roots;",
      " no matching config registry existed"),
    preserved = paste0("Scientific exports, historical audit outputs, ",
      "existing backups, logs, code and tests retained;",
      " old outputs labelled as unverified-scope ",
      "historical results in README"),
    active_default = paste0("Only a separately validated complete snapshot ",
      "may feed the main analysis"))
  write_json_once(receipt, paste0("logs/conalco_refresh_2026-10-16/",
      "old_run_isolation_receipt.json"))
  write_json_once(list(run_id = manifest$run_id, manifest =
      refresh_file_record(manifest_path),
      isolation_receipt = paste0("logs/conalco_refresh_2026-10-16/",
        "old_run_isolation_receipt.json")),
    'config/active_conalco_catalog.json')
  invisible(receipt)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) CENTRAL COMPLETENESS GATE FOR FINAL CROSS-SECTIONAL ANALYSES
# <-----------------------------------------------------------------------------

# Merged on 2026-10-19 from R/catalog_gate.R (file created 2026-10-16); content
# unchanged
catalog_reviewed_exclusions = \(entry, run_id) {
  if (is.null(entry)) return(character())
  stopifnot(file.exists(entry$path),
    identical(digest(file = entry$path, algo = 'sha256'), entry$sha256))
  review = read_json(entry$path)
  urls = as.character(unlist(review$category_urls))
  allowed = c('https://www.conalco.de/likoerella/',
    'https://www.conalco.de/tokaj-spirit/')
  stopifnot(identical(review$run_id, run_id),
    identical(review$decision,
      'user_excludes_named_additional_brand_navigation_only'),
    isTRUE(review$retain_all_observed_products), length(urls) == 2L,
    !anyDuplicated(urls), setequal(urls, allowed))
  urls
}

require_complete_catalog = \(manifest_path, products = NULL) {
  fail = \(reason) stop('FINAL ANALYSIS BLOCKED: ', reason, call. = F)
  if (!file.exists(manifest_path)) fail(
      'no validated complete catalogue manifest')
  manifest = read_json(manifest_path, simplifyVector = F)
  if (!identical(manifest$status, 'complete') || isTRUE(manifest$aborted))
    fail('catalogue status is not complete')
  if (!identical(manifest$catalog_coverage_status,
      'complete_defined_relevant_scope') ||
      is.null(manifest$scope) || !isTRUE(manifest$scope$navigation_reviewed) ||
      !isTRUE(manifest$scope$discovery_exhausted)) fail(
      'relevant scope or navigation coverage is unproven')
  required = c('category_completeness', 'page_completeness', 'product_ids',
    'product_details', 'category_links', 'products')
  for (name in required) {
    entry = manifest$proof[[name]]
    if (is.null(entry) || !file.exists(scalar(entry$path)) ||
        !identical(digest(file = entry$path, algo = 'sha256'), entry$sha256))
      fail(paste('missing or changed completeness evidence:', name))
  }
  coverage = read.csv(manifest$proof$category_completeness$path,
    stringsAsFactors = F, na.strings = '')
  if (isTRUE(manifest$refresh_version %in%
      c('conalco-refresh-0.3.0', 'conalco-refresh-0.4.0')) &&
      !'pagination_stability_status' %in% names(coverage))
    fail('pagination stability field is missing')
  listing = coverage$category_status == 'complete_observed_pagination'
  container = coverage$category_status == 'navigation_container_verified'
  excluded = coverage$category_status == 'excluded_additional_brand_navigation'
  approved_exclusions = tryCatch(catalog_reviewed_exclusions(
      manifest$scope$exclusion_review, manifest$run_id),
    error = \(e) fail('invalid documented navigation exclusion decision'))
  if (!setequal(coverage$category_url[excluded], approved_exclusions))
    fail(paste0("navigation exclusions lack the exact documented ",
        "user scope decision"))
  if (!nrow(coverage) || anyNA(listing) || any(!listing & !container &
        !excluded))
    fail('one or more categories are incomplete or unresolved')
  if (any(excluded) && (!all(c('in_scope', 'source_response_status',
          'unique_product_ids') %in% names(coverage)) ||
        any(coverage$in_scope[excluded]) || any(
        coverage$source_response_status[excluded] != 404L) ||
        any(coverage$unique_product_ids[excluded] != 0L) ||
        any(coverage$expected_pages[excluded] != 0L |
          coverage$successful_pages[excluded] != 0L)))
    fail(paste0("excluded navigation is not the documented ",
        "unavailable extra-page evidence"))
  if (anyNA(coverage$expected_pages[listing]) ||
      any(coverage$expected_pages[listing] != coverage$successful_pages[
        listing]) ||
      any(coverage$unresolved_details != 0)) fail(
      'pagination or product-detail coverage is incomplete')
  explicit = c('https://www.conalco.de/ready-to-drink/',
    'https://www.conalco.de/Ready-to-Serve')
  if (identical(manifest$scope$discovery_policy, 'current_navigation_v1')) {
    entry = manifest$proof$navigation_discovery
    if (is.null(entry) || !file.exists(entry$path) || !identical(
        digest(file = entry$path, algo = 'sha256'), entry$sha256))
      fail('current navigation discovery proof is missing or changed')
    discovery = read_json(entry$path)
    root = discovery$root
    if (!isTRUE(discovery$exhausted) ||
        !identical(discovery$run_id, manifest$run_id) ||
        !identical(digest(file = root$path, algo = 'sha256'), root$sha256) ||
        !setequal(unlist(discovery$current_urls), coverage$category_url))
      fail('current navigation scope is not exhausted')
    seeds = refresh_scope_seeds(dirname(root$path), dynamic = T)
    links = read.csv(manifest$proof$category_links$path,
      colClasses = 'character')
    if (!all(seeds$category_url %in% coverage$category_url) ||
        !all(links$category_url[links$decision == 'crawl'] %in%
          coverage$category_url))
      fail('a current navigation category is missing')
  } else if (!all(explicit %in% coverage$category_url[listing])) fail(
    'explicit RTD/RTS coverage is missing')
  pages = read.csv(manifest$proof$page_completeness$path, stringsAsFactors = F)
  if ('pagination_stability_status' %in% names(coverage)) {
    if (anyNA(coverage$pagination_stability_status) ||
        any(!coverage$pagination_stability_status %in%
          c('not_rechecked', 'verified_consecutive_passes',
            'verified_boundary_placements', 'verified_independent_reference')))
      fail('pagination stability is unproven')
    verified = coverage$category_url[
      coverage$pagination_stability_status %in%
        c('verified_consecutive_passes', 'verified_boundary_placements',
          'verified_independent_reference')]
    if (length(verified) || !is.null(manifest$proof$pagination_reviews)) {
      entry = manifest$proof$pagination_reviews
      if (is.null(entry) || !file.exists(entry$path) || !identical(
          digest(file = entry$path, algo = 'sha256'), entry$sha256))
        fail('pagination recheck proof is missing or changed')
      reviews = read_json(entry$path)
      if (!setequal(names(reviews), verified))
        fail('pagination recheck categories differ')
      refresh_check_pagination_reviews(reviews)
      for (url in verified) {
        method = coverage$pagination_stability_status[
          coverage$category_url == url]
        placement = method %in% c('verified_boundary_placements',
          'verified_independent_reference')
        if (!identical(placement, !is.null(reviews[[url]]$reference)))
          fail('pagination method differs from its independent evidence')
        independent = !is.null(reviews[[url]]$reference$accepted_pass)
        if (!identical(independent, method == 'verified_independent_reference'))
          fail('accepted pagination source differs from its evidence')
        pass = tail(reviews[[url]]$passes, 1L)[[1]]
        if (independent) pass = reviews[[url]]$reference$passes[[1]]
        sources = vapply(pass, `[[`, '', 'metadata_file')
        if (!setequal(sources, pages$metadata_file[pages$category_url == url]))
          fail('accepted pagination pages differ from the verified pass')
        if (independent) {
          decoded = refresh_decode_pagination_pass(pass, url, 'name-asc')$rows
          # Compare at the same CSV boundary as the canonical snapshot.
          # The original responses were compared exactly above.
          reference_file = tempfile('reference_', fileext = '.csv')
          on.exit(unlink(reference_file), add = T)
          write_csv_once(decoded, reference_file)
          decoded = read_observations(reference_file)
          unlink(reference_file)
          for (name in c('category_memberships', 'raw_inventory')) {
            entry = manifest$proof[[name]]
            if (is.null(entry) || !file.exists(entry$path) || !identical(
                digest(file = entry$path, algo = 'sha256'), entry$sha256))
              fail('independent membership or raw evidence is missing')
          }
          members = read.csv(manifest$proof$category_memberships$path,
            colClasses = 'character')
          found = members[members$category_url == url, , drop = F]
          canonical = read_observations(manifest$proof$products$path)
          keys = c('retailer_product_id', 'category_page',
            'listing_metadata_file')
          key = \(x) do.call(paste, c(x[keys], sep = '|'))
          if (!setequal(key(found), key(decoded)) ||
              !all(decoded$retailer_product_id %in%
                canonical$retailer_product_id) ||
              coverage$unique_product_ids[coverage$category_url == url] !=
                nrow(decoded))
            fail('independent products or category memberships are missing')
          canonical = canonical[canonical$retailer_product_id %in%
            decoded$retailer_product_id, , drop = F]
          index = match(decoded$retailer_product_id,
            canonical$retailer_product_id)
          fields = c('retailer_product_id', 'product_name', 'price_current',
            'volume_l', 'alcohol_content', 'pack_count')
          missing = vapply(c('volume_l', 'alcohol_content', 'pack_count'),
            \(field) any(is.na(canonical[[field]][index]) &
              !is.na(decoded[[field]])),
            logical(1))
          if (!refresh_reference_fields(canonical) || any(missing) ||
              !identical(decoded$product_url, canonical$product_url[index]) ||
              !refresh_placement_values(bind_rows(decoded[fields],
                canonical[fields])))
            fail('canonical values differ from the independent responses')
          inventory = read.csv(manifest$proof$raw_inventory$path,
            colClasses = 'character')
          controls = unlist(lapply(reviews[[url]]$reference$passes, \(p)
            vapply(p, `[[`, '', 'metadata_file')), use.names = F)
          if (!all(controls %in% inventory$metadata_file))
            fail('independent original responses are absent from inventory')
        }
      }
    }
  }
  for (i in which(listing)) {
    found = pages[pages$category_url == coverage$category_url[i], ]
    placement = 'pagination_stability_status' %in% names(coverage) &&
      coverage$pagination_stability_status[i] == 'verified_boundary_placements'
    if (!identical(sort(unique(as.integer(found$page))), seq_len(
          coverage$expected_pages[i])) ||
        any(!found$status %in% c('ok', 'empty_category_verified',
          if (placement) 'overlapping_ids_between_pages')) ||
        !any(found$terminal_page_verified %in% T)) fail(
        'page-level terminal proof is missing')
  }
  ids = read.csv(manifest$proof$product_ids$path, colClasses =
      'character')$retailer_product_id
  details = read.csv(manifest$proof$product_details$path, colClasses =
      'character')
  if (anyDuplicated(ids) || !length(ids) || !setequal(ids,
      details$retailer_product_id) ||
      anyDuplicated(details$retailer_product_id) || any(
      details$detail_status != 'processed'))
    fail('product identities or detail evidence are incomplete')
  approved = read_observations(manifest$proof$products$path)
  if (is.null(products)) products = approved
  if (!identical(unique(products$run_id), manifest$run_id) ||
      !all(products$retailer == 'conalco') || anyDuplicated(
      products$retailer_product_id) ||
      !setequal(ids, products$retailer_product_id)) fail(
      'mixed, duplicated or inconsistent analysis snapshot')
  if (anyNA(products$observed_at) || any(!grepl('Z$', products$observed_at)))
    fail('actual observation timestamps with time zone are missing')
  index = match(products$retailer_product_id, approved$retailer_product_id)
  for (field in intersect(c('observed_at', 'price_current', 'volume_l',
        'alcohol_content', 'raw_sha256'), names(approved))) {
    if (!identical(products[[field]], approved[[field]][index]))
      fail(paste('measurement differs from the validated single snapshot:',
          field))
  }
  invisible(manifest)
}

active_complete_catalog = \(run_id = NULL) {
  if (is.null(run_id)) {
    path = 'config/active_conalco_catalog.json'
    if (!file.exists(path)) stop(paste0(
          "FINAL ANALYSIS BLOCKED: no complete Conalco ",
          "snapshot registered"))
    run_id = read_json(path)$run_id
  }
  path = file.path('data/raw/conalco', run_id, 'manifest.json')
  require_complete_catalog(path)
  path
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) COMPLETENESS GATE FOR ALDI NORD SNAPSHOT ANALYSES
# <-----------------------------------------------------------------------------

# Separate from require_complete_catalog(): ALDI proves completeness per
# embedded listing
# (reported hits = embedded cards) instead of HTML pagination, and has no
# RTD/RTS categories.
require_complete_aldi_snapshot = \(manifest_path, products = NULL) {
  fail = \(reason) stop('ALDI ANALYSIS BLOCKED: ', reason, call. = F)
  if (!file.exists(manifest_path)) fail(
      'no validated complete ALDI Nord manifest')
  manifest = read_json(manifest_path, simplifyVector = F)
  if (!identical(manifest$retailer, 'aldi_nord') || !identical(
      manifest$status, 'complete') ||
      isTRUE(manifest$aborted)) fail('snapshot status is not complete')
  if (!identical(manifest$catalog_coverage_status,
      'complete_defined_relevant_scope') ||
      !isTRUE(manifest$scope$navigation_reviewed) || !isTRUE(
      manifest$scope$discovery_exhausted))
    fail('relevant scope or navigation coverage is unproven')
  if (!identical(manifest$scope$detail_policy, 'all_in_scope_products') ||
      !identical(manifest$scope$full_collection_status, 'completed') ||
      !isTRUE(manifest$scope$details_complete))
    fail(paste0("structural pilot or incomplete detail coverage; ",
        "full collection pending"))
  required = c('category_completeness', 'category_memberships',
    'product_ids', 'product_details',
    'raw_inventory', 'products')
  for (name in required) {
    entry = manifest$proof[[name]]
    if (is.null(entry) || !file.exists(scalar(entry$path)) ||
        !identical(digest(file = entry$path, algo = 'sha256'), entry$sha256))
      fail(paste('missing or changed completeness evidence:', name))
  }
  coverage = read.csv(manifest$proof$category_completeness$path,
    stringsAsFactors = F, na.strings = '')
  allowed = c('complete_embedded_listing', 'empty_category_verified',
    'navigation_container_verified')
  listing = coverage$status == 'complete_embedded_listing'
  if (!nrow(coverage) || any(!coverage$status[coverage$required %in% T] %in%
        allowed))
    fail(paste0("one or more required category pages are ",
        "incomplete or unresolved"))
  if (!any(listing & coverage$scope_role == 'core_alcohol')) fail(
      'no complete core alcohol listing')
  if (any(coverage$reported_total_products[listing] !=
        coverage$embedded_product_cards[listing]) ||
      any(coverage$embedded_product_cards[listing] !=
        coverage$unique_product_ids[listing]) ||
      any(!coverage$exhaustive_nb_hits[listing] %in% T) || any(
      coverage$nb_pages[listing] != 1L))
    fail('listing counts do not prove embedded completeness')
  ids = read.csv(manifest$proof$product_ids$path, colClasses =
      'character')$retailer_product_id
  details = read.csv(manifest$proof$product_details$path, colClasses =
    'character', na.strings = '')
  if (!length(ids) || anyDuplicated(ids) || anyDuplicated(
      details$retailer_product_id) ||
      !all(details$retailer_product_id %in% ids) ||
      any(!details$detail_status %in% 'processed'))
    fail('product identities or detail evidence are incomplete')
  approved = read_observations(manifest$proof$products$path)
  if (is.null(products)) products = approved
  if (!identical(unique(products$run_id), manifest$run_id) || !all(
      products$retailer == 'aldi_nord') ||
      anyDuplicated(products$retailer_product_id) || !setequal(ids,
      products$retailer_product_id))
    fail('mixed, duplicated or inconsistent analysis snapshot')
  if (anyNA(products$observed_at) || any(!grepl('Z$', products$observed_at)))
    fail('actual observation timestamps with time zone are missing')
  required_ids = approved$retailer_product_id[approved$aldi_alcohol_scope %in%
      'TRUE']
  if (!setequal(required_ids, details$retailer_product_id))
    fail(paste0("details do not cover every product in the ",
        "analytical scope"))
  if (any(!approved$slug_identity_consistent %in% 'TRUE') ||
      any(!approved$card_identity_consistent %in% 'TRUE') ||
      any(approved$detail_identity_equal %in% 'FALSE'))
    fail('conflicting product identity')
  index = match(products$retailer_product_id, approved$retailer_product_id)
  derived = c('parser_version', 'collection_started_at_utc',
    'collection_ended_at_utc',
    'classification_version', 'analysis_version', 'derivation_version',
    'validation_flags', 'parse_status')
  fields = setdiff(names(approved), derived)
  if (!all(fields %in% names(products))) fail(
      'source measurement fields missing')
  for (field in fields) {
    if (!identical(as.character(products[[field]]), as.character(approved[[
              field]][index])))
      fail(paste('measurement differs from the validated single snapshot:',
          field))
  }
  invisible(manifest)
}
