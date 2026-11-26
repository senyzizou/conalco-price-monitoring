# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261016

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) RESUMABLE ADAPTER AROUND THE EXISTING CATALOGUE PARSER
# <-----------------------------------------------------------------------------

CATALOG_REFRESH_VERSION = 'conalco-refresh-0.4.2'

refresh_global_stop = \(message) {
  stop(structure(list(message = message, call = NULL),
    class = c('conalco_global_error', 'error', 'condition')))
}

refresh_query_value = \(url, key, default = NA_character_) {
  if (!grepl('?', url, fixed = T)) return(default)
  pairs = strsplit(sub('^[^?]*[?]', '', url), '&', fixed = T)[[1]]
  found = pairs[sub('=.*$', '', pairs) == key]
  if (length(found) > 1L) stop('Ambiguous listing query parameter: ', key)
  if (!length(found)) default else URLdecode(sub('^[^=]*=', '', found))
}

refresh_request_url = \(url, routes = list()) {
  category = sub('[?].*$', '', url)
  for (route in routes) {
    if (!identical(category, route$category_url)) next
    page = refresh_query_value(url, 'p', '1')
    if (!grepl('^[1-9][0-9]*$', page)) stop('Invalid listing page')
    actual = gsub('{page}', page, route$request_url_template, fixed = T)
    if (!startsWith(actual, 'https://www.conalco.de/widgets/cms/navigation/'))
      stop(paste0("Endpoint route must be a documented public ",
          "Conalco listing"))
    order = refresh_query_value(url, 'order')
    if (!is.na(order)) {
      if (order != 'name-asc') stop('Unsupported independent listing order')
      actual = paste0(actual, if (grepl('?', actual, fixed = T)) '&' else '?',
        'order=name-asc')
    }
    return(actual)
  }
  url
}

refresh_verify_route = \(metadata, url, actual) {
  if (identical(url, actual)) return(metadata)
  expected = sub(paste0('^https://www[.]conalco[.]de/widgets/cms/navigation/',
      '([a-f0-9]{32})[?].*$'), '\\1', actual)
  if (!grepl('^[a-f0-9]{32}$', expected))
    stop('Documented listing route has no unambiguous category identity')
  doc = read_html(read_archive(metadata$metadata_file)$body, encoding = 'UTF-8')
  page = as.integer(refresh_query_value(url, 'p', '1'))
  if (!identical(refresh_category_identity(doc), expected) ||
      !identical(catalog_pagination(doc)$current, page))
    stop('Fresh documented listing differs from expected category/page')
  metadata
}

refresh_recover_public_endpoint = \(url, id, runtime, directory, client,
  old_directory = 'data/raw/conalco/catalog_20261012T092338_c2109ed57d1d') {
  canonical = sub('[?].*$', '', url)
  paths = list.files(old_directory, '^[0-9]+_category[.]json$', full.names = T)
  candidates = Filter(\(path) {
      meta = read_json(path)
      identical(meta$request_url, canonical) && identical(
        meta$response_status, 200L)
    }, paths)
  if (length(candidates) != 1L) stop(
      'No unique documented public endpoint for ', canonical)
  archived = read_archive(candidates[[1]])
  doc = read_html(archived$body, encoding = 'UTF-8')
  node = xml_find_all(doc, '//*[@data-listing="true"]')
  if (length(node) != 1L) stop(
      'Historical technical endpoint is not unambiguous')
  attribute = xml_attr(node[[1]], 'data-listing-options')
  options = fromJSON(attribute)
  endpoint = scalar(options$dataUrl)
  if (!grepl('^/widgets/cms/navigation/[a-f0-9]{32}$', endpoint))
    stop(paste0("Endpoint is not a documented public Shopware ",
        "category listing"))
  category_id = sub('^.*/', '', endpoint)
  route = list(category_url = canonical, request_url_template = paste0(
      'https://www.conalco.de', endpoint, '?slots=', options$params$slots,
      '&no-aggregations=1&p={page}'), source_metadata = candidates[[1]],
    source_raw_sha256 = archived$meta$raw_sha256, technical_attribute =
    attribute,
    source_use = paste0("Technical category and slot ID only; no ",
      "historical product or price observations reused"))
  actual = refresh_request_url(url, list(route))
  metadata = refresh_fetch(actual, 'category', id, runtime, directory, client)
  fresh_doc = read_html(read_archive(metadata$metadata_file)$body, encoding =
      'UTF-8')
  expected_page = if (grepl('[?]p=[0-9]+$', url)) as.integer(sub('^.*[?]p=',
        '', url)) else 1L
  stopifnot(identical(refresh_category_identity(fresh_doc), category_id),
    identical(catalog_pagination(fresh_doc)$current, expected_page),
    metadata$run_id == id)
  route$fresh_success_metadata = metadata$metadata_file
  write_json_once(list(created_at_utc = utc_now(), routes = list(route)),
    file.path(directory, paste0('category_endpoint_routes_', category_id,
        '.json')))
  runtime$routes = c(runtime$routes, list(route))
  cat('PUBLIC ENDPOINT RECOVERED', canonical, category_id, '\n')
  flush.console()
  metadata
}

refresh_save_state = \(state, directory) {
  # Checkpoints are mutable operational state; archived responses remain
  # immutable.
  path = file.path(directory, 'checkpoint.rds')
  pending = tempfile('.checkpoint_', tmpdir = directory)
  saveRDS(state, pending)
  if (!file.rename(pending, path)) stop('Cannot persist collection checkpoint')
  invisible(state)
}

refresh_retry_delay = \(headers, attempt, now = Sys.time()) {
  value = scalar(headers[['retry-after']])
  seconds = suppressWarnings(as.numeric(value))
  if (is.na(seconds) && !is.na(value)) {
    # HTTP dates use English names; curl parses them independently of LC_TIME.
    date = curl::parse_date(value)
    seconds = as.numeric(difftime(date, now, units = 'secs'))
  }
  max(10 * 2^(attempt - 1L), if (is.na(seconds)) 0 else seconds)
}

refresh_response_cache = \(directory, types = 'category|navigation|product') {
  # A plain digit class also accepts indices above 9999.
  paths = sort(list.files(directory, paste0('^[0-9]+_(', types, ')[.]json$'),
      full.names = T))
  entries = lapply(paths, read_json, simplifyVector = T)
  entries = Filter(\(m) identical(as.integer(m$response_status), 200L), entries)
  cache = new.env(parent = emptyenv())
  for (m in entries) cache[[m$request_url]] = m
  cache
}

refresh_fetch = \(url, type, id, state, directory, client, product_id =
  NA_character_,
  sleep = Sys.sleep, retailer = 'conalco', base_dir = 'data/raw/conalco',
  max_requests = 1500L,
  max_hours = 2, return_404_types = 'category', max_redirects = 5L,
  global_access = F, reuse = T) {
  stopifnot(length(max_redirects) == 1L, is.finite(max_redirects),
    max_redirects >= 0, max_redirects == floor(max_redirects))
  if (reuse && exists(url, state$cache, inherits = F)) {
    metadata = state$cache[[url]]
    read_archive(metadata$metadata_file)
    if (global_access && !identical(
        berlin_date(metadata$observed_at_utc),
        berlin_date(read_json(file.path(directory,
          'started.json'))$started_at_utc)))
      refresh_global_stop('Cached response belongs to another Berlin day')
    return(metadata)
  }
  existing = list.files(directory, '^[0-9]+_[a-z]+[.]json$')
  index = max(c(0L, as.integer(sub('_.*', '', existing))))
  if (is.null(state$started_at)) state$started_at = Sys.time()
  elapsed = as.numeric(difftime(Sys.time(), state$started_at, units = 'hours'))
  if (index >= max_requests || elapsed >= max_hours)
    refresh_global_stop(paste0('Bounded refresh budget reached (',
      max_requests, ' archived requests / ', max_hours,
      ' hours per invocation); progress retained'))
  request_url = url
  visited = character()
  attempt = 1L
  repeat {
    if (reuse && exists(request_url, state$cache, inherits = F)) {
      metadata = state$cache[[request_url]]
      read_archive(metadata$metadata_file)
      if (global_access && !identical(
          berlin_date(metadata$observed_at_utc),
          berlin_date(read_json(file.path(directory,
            'started.json'))$started_at_utc)))
        refresh_global_stop('Cached response belongs to another Berlin day')
      state$cache[[url]] = metadata
      return(metadata)
    }
    elapsed = as.numeric(difftime(Sys.time(), state$started_at,
        units = 'hours'))
    if (index >= max_requests || elapsed >= max_hours)
      refresh_global_stop('Bounded refresh budget reached; progress retained')
    log_event(file.path(directory, 'request_events.jsonl'), 'request_started',
      request_url = request_url, request_type = type,
      request_index = index + 1L)
    response = client(request_url)
    index = index + 1L
    metadata = archive_response(response, id, type, index, product_id,
      base_dir = base_dir,
      retailer = retailer)
    status = metadata$response_status
    cat(utc_now(), 'HTTP', status, type, request_url, '\n')
    flush.console()
    started_path = file.path(directory, 'started.json')
    if (global_access && file.exists(started_path)) {
      expected_date = berlin_date(read_json(started_path)$started_at_utc)
      observed_date = berlin_date(metadata$observed_at_utc)
      if (anyNA(observed_date) || !identical(observed_date, expected_date))
        refresh_global_stop(paste('Berlin date changed;',
            'response archived, not parsed'))
    }
    if (global_access && (status %in% c(401L, 403L, 407L, 429L, 451L) ||
        http_access_challenge(response)))
      refresh_global_stop(paste0('Access/rate-limit stop after archive ',
        metadata$metadata_file, '; HTTP ', status))
    if (!is.na(status) && status == 200L) {
      state$cache[[url]] = metadata
      state$cache[[request_url]] = metadata
      return(metadata)
    }
    # Archive each hop before checking its target. Never retry a loop.
    if (!is.na(status) && status %in% c(301L, 302L, 303L, 307L, 308L)) {
      target = http_redirect_url(request_url, metadata$headers, visited)
      if (length(visited) >= max_redirects)
        stop('HTTP redirect blocked: hop limit reached; archive: ',
          metadata$metadata_file)
      visited = c(visited, request_url)
      request_url = target
      attempt = 1L
      next
    }
    # A missing category remains unresolved while other queued work continues.
    if (type %in% return_404_types && !is.na(status) && status ==
        404L) return(
        metadata)
    redirect_loop = grepl('redirects hit maximum|Maximum.*redirects followed',
      coalesce(scalar(metadata$error), ''), ignore.case = T)
    transient = (is.na(status) && !redirect_loop) || status %in% c(429L, 500L,
      502L, 503L, 504L)
    if (!transient || attempt == 3L || global_access)
      stop('Collection paused after archived response ',
        metadata$metadata_file,
        '; status=', status, '; ', scalar(metadata$error))
    delay = refresh_retry_delay(metadata$headers, attempt)
    if (delay > 300) stop('Retry-After exceeds bounded local wait: ', delay,
        ' seconds')
    sleep(delay)
    attempt = attempt + 1L
  }
}

refresh_category_identity = \(doc) {
  node = xml_find_first(doc, '//*[@data-listing="true"]')
  if (inherits(node, 'xml_missing')) return(NA_character_)
  options = fromJSON(xml_attr(node, 'data-listing-options'))
  sub('^.*/navigation/', '', scalar(options$dataUrl))
}

refresh_page = \(metadata, name, url, page) {
  if (identical(as.integer(metadata$response_status), 404L))
    return(list(doc = NULL, pagination = NULL, rows = data.frame(),
        links = data.frame(category_name = character(), category_url =
          character(),
          decision = character()), category_id = NA_character_,
        terminal_page_verified = F, unavailable_status =
          'http_404_unresolved_category'))
  parsed = catalog_process_page(metadata, name, url, page)
  if (is.null(parsed)) stop('Missing successful category response')
  parsed$links = catalog_links(parsed$doc)
  # Alcohol-containing calendars are relevant sets, despite the old family-only
  # filter.
  calendar = grepl('adventskalender', parsed$links$category_name,
    ignore.case = T)
  parsed$links$decision[calendar] = 'crawl'
  parsed$category_id = refresh_category_identity(parsed$doc)
  selector = '//*[@data-listing="true"]//*[@data-product-information]'
  if (is.null(parsed$pagination)) {
    selector = '//main//*[@data-product-information]'
    parsed$rows = catalog_parse_cards(metadata$metadata_file, name, url,
      page, selector)
    if (nrow(parsed$rows)) parsed$rows$source_format =
      'html_navigation_product_card'
    # Landing-page tiles may expose relevant category children absent from the
    # sidebar.
    anchors = xml_find_all(parsed$doc,
      paste0("//main//a[@href and not(ancestor::*[@data-produc",
        "t-information])]"))
    extra = data.frame(category_name = clean_text(xml_text(anchors)),
      category_url = vapply(xml_attr(anchors, 'href'), catalog_url,
        character(1)),
      decision = rep('crawl', length(anchors)), stringsAsFactors = F)
    extra = extra[!is.na(extra$category_url) & nzchar(extra$category_name) &
        !extra$category_url %in% c('https://www.conalco.de/', url) &
        !grepl('-vol|[0-9][lL](?:-|$)|/media/|[.](?:pdf|png|jpg|webp)$',
        extra$category_url, perl = T) &
        !grepl(paste0("eingelegte|getrocknete|sirup|tonic|filler|",
          "gläser|glaser|barzubehör|gutschein|magazin|", "^marken$"),
        extra$category_name, ignore.case = T), ]
    parsed$links = bind_rows(parsed$links, extra)
    parsed$links = parsed$links[!duplicated(parsed$links$category_url), ]
  }
  parsed$terminal_page_verified = !is.null(parsed$pagination) &&
    parsed$pagination$current == parsed$pagination$last &&
    !length(xml_find_all(parsed$doc, paste0('//*[@data-listing="true"]//li[',
        class_xpath('page-next'), ']//input[@name="p" and not(@disabled)]')))
  if (nrow(parsed$rows)) {
    cards = xml_find_all(parsed$doc, selector)
    descriptions = vapply(cards, \(card) node_text_verbatim(card,
        paste0('.//*[', class_xpath('product-description'), ']')),
      character(1))
    parsed$rows = attach_conalco_description_evidence(parsed$rows,
      descriptions)
    parsed$rows$detail_coverage_method = ifelse(!is.na(descriptions) &
        nchar(descriptions) >= 80,
      'same_product_full_description_embedded_in_listing',
      'individual_detail_required')
  }
  parsed
}

refresh_scope_seeds = \(directory, dynamic = T) {
  # Exactly one archived navigation response per run (0004 on 2026-10-16).
  navigation = list.files(directory, '^[0-9]+_navigation[.]json$',
    full.names = T)
  if (length(navigation) != 1L) stop(
      'Expected one archived navigation response in ', directory)
  nav = read_html(read_archive(navigation)$body, encoding = 'UTF-8')
  links = catalog_links(nav)
  if (dynamic) {
    nodes = xml_find_all(nav, paste0('//a[',
      class_xpath('main-navigation-link'), ' or ',
      class_xpath('navigation-flyout-link'), ']'))
    # Reuse the family exclusion rules on the current shop navigation.
    wrapper = xml_new_root('div', class = 'category-navigation')
    for (node in nodes) xml_add_child(wrapper, node, .copy = T)
    links = bind_rows(links, catalog_links(wrapper))
    links$decision[grepl('adventskalender', links$category_name,
        ignore.case = T)] = 'crawl'
    links = links[links$decision == 'crawl' &
        links$category_url != 'https://www.conalco.de/',
      c('category_name', 'category_url')]
    links = links[!duplicated(links$category_url), ]
    if (!nrow(links))
      refresh_global_stop('Current navigation exposes no usable categories')
    return(links)
  }
  required = data.frame(category_name = c('Ready to Drink', 'Ready to Serve'),
    category_url = c('https://www.conalco.de/ready-to-drink/',
      'https://www.conalco.de/Ready-to-Serve'), stringsAsFactors = F)
  seeds = bind_rows(required, catalog_seeds(),
    links[links$decision == 'crawl', c('category_name', 'category_url')])
  seeds[!duplicated(seeds$category_url), ]
}

refresh_boundary_ids = \(ids) {
  if (any(vapply(ids, anyDuplicated, 0L) > 0L)) return(NA_character_)
  flat = unlist(ids, use.names = F)
  repeated = unique(flat[duplicated(flat)])
  for (id in repeated) {
    at = which(vapply(ids, \(page) id %in% page, logical(1)))
    if (length(at) != 2L || diff(at) != 1L ||
        !identical(tail(ids[[at[1]]], 1L), id) ||
        !identical(head(ids[[at[2]]], 1L), id)) return(NA_character_)
  }
  repeated
}

refresh_pagination_signature = \(pages, allow_boundary = F,
  allow_overlap = F) {
  if (!length(pages)) return(NULL)
  pages = unname(pages[order(vapply(pages, \(p) as.integer(p$page), 0L))])
  last = pages[[1]]$last
  if (is.null(last) || is.na(last) || last < 1L ||
      !identical(vapply(pages, \(p) as.integer(p$page), 0L), seq_len(last)) ||
      !all(vapply(pages, \(p) identical(as.integer(p$last),
        as.integer(last)) && p$status %in% c('ok', 'empty_category_verified',
          if (allow_boundary || allow_overlap)
            'overlapping_ids_between_pages'),
        logical(1))) || !isTRUE(tail(pages, 1L)[[1]]$terminal_page_verified))
    return(NULL)
  ids = lapply(pages, \(p) as.character(unlist(p$ids, use.names = F)))
  if (any(vapply(ids, anyDuplicated, 0L) > 0L)) return(NULL)
  if (anyDuplicated(unlist(ids, use.names = F)) && !allow_overlap &&
      (!allow_boundary || anyNA(refresh_boundary_ids(ids)))) return(NULL)
  identity = vapply(pages, \(p) scalar(p$category_id), '')
  if (length(unique(identity)) != 1L) return(NULL)
  list(category_id = identity[1], ids = unname(ids))
}

refresh_order_evidence = \(metadata_file, require_selected = F) {
  doc = read_html(read_archive(metadata_file)$body, encoding = 'UTF-8')
  nodes = xml_find_all(doc, paste0('//*[@data-listing="true"]',
    '//*[@data-listing-sorting="true"]'))
  if (length(nodes) != 1L) return(F)
  options = xml_find_all(nodes[[1]], './/select/option[@value="name-asc"]')
  if (length(options) != 1L) return(F)
  selected = fromJSON(xml_attr(nodes[[1]],
    'data-listing-sorting-options'))$sorting
  if (require_selected) identical(selected, 'name-asc') else
    !is.null(selected) && !identical(selected, 'name-asc')
}

refresh_placement_values = \(rows) {
  repeated = unique(rows$retailer_product_id[
    duplicated(rows$retailer_product_id)])
  all(vapply(repeated, \(id) {
    product = rows[rows$retailer_product_id == id, , drop = F]
    all(vapply(c('product_name', 'price_current', 'volume_l',
      'alcohol_content', 'pack_count'), \(field)
      length(unique(product[[field]][!is.na(product[[field]])])) <= 1L,
      logical(1)))
  }, logical(1)))
}

refresh_collect_reference = \(state, url, directory, fetch) {
  review = state$pagination_reviews[[url]]
  discovery = review$passes[[length(review$passes)]][[1]]$metadata_file
  if (!refresh_order_evidence(discovery)) return(list(state = state,
    error = simpleError('No independently selectable Name A-Z order')))
  if (is.null(review$reference)) review$reference = list(order = 'name-asc',
    discovery_metadata = discovery, passes = list())
  state$pagination_reviews[[url]] = review
  for (pass_index in 1:2) {
    existing = state$pagination_reviews[[url]]$reference$passes
    pass = if (length(existing) >= pass_index) existing[[pass_index]] else
      list()
    page = 1L
    last = 1L
    repeat {
      if (length(pass) < page) {
        result = tryCatch({
          request_url = paste0(url, '?p=', page, '&order=name-asc')
          metadata = fetch(request_url, reuse = F)
          parsed = refresh_page(metadata, 'Independent order', url, page)
          if (is.null(parsed$pagination) ||
              parsed$pagination$current != page ||
              !refresh_order_evidence(metadata$metadata_file, T) ||
              !identical(refresh_query_value(metadata$request_url, 'order'),
                'name-asc'))
            stop('Independent listing category/page/order is unproven')
          checks = catalog_page_checks(parsed, page, parsed$pagination$last,
            list(), url)
          entry = list(url = url, page = page, last = parsed$pagination$last,
            category_id = parsed$category_id, ids =
              parsed$rows$retailer_product_id,
            status = catalog_page_status(checks$good, checks$empty_verified,
              parsed), terminal_page_verified =
              parsed$terminal_page_verified,
            metadata_file = metadata$metadata_file)
          list(entry = entry)
        }, error = \(e) list(error = e))
        if (!is.null(result$error))
          return(list(state = state, error = result$error))
        pass[[page]] = result$entry
        state$pagination_reviews[[url]]$reference$passes[[pass_index]] = pass
        refresh_save_state(state, directory)
      }
      last = pass[[1]]$last
      if (pass[[page]]$last != last ||
          !pass[[page]]$status %in% c('ok', 'empty_category_verified'))
        return(list(state = state,
          error = simpleError('Independent pagination failed')))
      if (page >= last) break
      page = page + 1L
    }
    if (is.null(refresh_pagination_signature(pass)))
      return(list(state = state,
        error = simpleError(
          'Independent pagination is incomplete or overlaps')))
  }
  reference = state$pagination_reviews[[url]]$reference
  signatures = lapply(reference$passes, refresh_pagination_signature)
  primary = refresh_pagination_signature(tail(review$passes, 1L)[[1]],
    allow_overlap = T)
  if (!identical(signatures[[1]], signatures[[2]]) ||
      !identical(primary$category_id, signatures[[1]]$category_id) ||
      !all(unlist(primary$ids) %in% unlist(signatures[[1]]$ids)))
    return(list(state = state,
      error = simpleError('Independent ID coverage or repeated values differ')))
  review = state$pagination_reviews[[url]]
  review$verified = T
  review$candidate = NULL
  normal = lapply(tail(review$passes, 2L), refresh_pagination_signature,
    allow_overlap = T)
  if (!setequal(unlist(primary$ids), unlist(signatures[[1]]$ids)) ||
      anyNA(refresh_boundary_ids(primary$ids)) ||
      !identical(normal[[1]], normal[[2]]))
    review$reference$accepted_pass = 1L
  # Re-decode original responses before accepting or importing any product.
  checked = tryCatch({
    refresh_check_pagination_reviews(setNames(list(review), url))
    NULL
  }, error = \(e) e)
  if (!is.null(checked)) return(list(state = state, error = checked))
  if (!is.null(review$reference$accepted_pass)) {
    name = state$queue$category_name[match(url, state$queue$category_url)]
    for (entry in review$reference$passes[[1]]) {
      parsed = refresh_page(read_json(entry$metadata_file), name, url,
        entry$page)
      parsed$doc = NULL
      path = file.path(directory, 'parsed_pages', paste0(substr(
        digest(entry$metadata_file, algo = 'sha256'), 1, 20), '_',
        CATALOG_VERSION, '.rds'))
      if (!file.exists(path)) write_once(path, \(p) saveRDS(parsed, p))
      else stopifnot(identical(readRDS(path), parsed))
      entry$parsed_file = path
      state$pages[[paste(url, entry$page, sep = '|')]] = entry
    }
    # Reference pagination can have a different page count.
    state$pages = Filter(\(p) !identical(p$url, url) ||
      p$page <= length(review$reference$passes[[1]]), state$pages)
  }
  state$pagination_reviews[[url]] = review
  state$failures = Filter(\(failure) failure$kind != 'category' ||
    !identical(failure$url, url), state$failures)
  list(state = state, error = NULL)
}

refresh_review_pagination = \(state, url, directory, fetch = NULL) {
  pages = unname(Filter(\(p) identical(p$url, url), state$pages))
  unstable = any(vapply(pages, \(p) !is.na(p$last) && p$status %in%
    c('overlapping_ids_between_pages', 'pagination_changed_during_collection',
      'duplicate_ids_on_page'), logical(1)))
  review = state$pagination_reviews[[url]]
  if (isTRUE(review$verified)) return(list(state = state, restart = F))
  signature = refresh_pagination_signature(pages, allow_overlap = T)
  structural = any(vapply(pages, \(p) p$status %in%
    c('pagination_changed_during_collection', 'duplicate_ids_on_page'),
    logical(1)))
  if (unstable && is.null(signature) && !structural)
    return(list(state = state, restart = F))
  if (!unstable && (is.null(review) || isTRUE(review$verified) ||
      is.null(signature))) return(list(state = state, restart = F))
  if (is.null(review)) review = list(attempts = 0L, passes = list(),
    verified = F, exhausted = F, candidate = NULL)
  resume_reference = !is.null(review$reference)
  if (!resume_reference) review$passes = c(review$passes, list(pages))
  reference_ready = !is.null(signature) && length(review$passes) >= 2L &&
    anyDuplicated(unlist(signature$ids)) > 0L &&
    !is.null(refresh_pagination_signature(
      review$passes[[length(review$passes) - 1L]], allow_overlap = T))
  if (resume_reference || reference_ready ||
      (!is.null(signature) && identical(signature, review$candidate))) {
    if (resume_reference || anyDuplicated(unlist(signature$ids)) > 0L) {
      state$pagination_reviews[[url]] = review
      result = if (is.null(fetch)) list(state = state,
        error = simpleError('Independent traversal is required')) else
        refresh_collect_reference(state, url, directory, fetch)
      state = result$state
      if (!is.null(result$error)) {
        if (inherits(result$error, 'conalco_global_error'))
          return(list(state = state, restart = F, error = result$error))
        state$pagination_reviews[[url]]$exhausted = T
        state$failures[[paste0(url, '|pagination')]] = refresh_failure(url, 1L,
          'category', conditionMessage(result$error), directory)
      }
      refresh_save_state(state, directory)
      return(list(state = state, restart = F))
    }
    review$verified = T
    review$candidate = NULL
    state$pagination_reviews[[url]] = review
    refresh_save_state(state, directory)
    return(list(state = state, restart = F))
  }
  if (review$attempts >= 3L) {
    review$exhausted = T
    state$pagination_reviews[[url]] = review
    state$failures[[paste0(url, '|pagination')]] = refresh_failure(url, 1L,
      'category', 'pagination_stability_not_verified', directory)
    refresh_save_state(state, directory)
    return(list(state = state, restart = F))
  }
  review$attempts = review$attempts + 1L
  review$candidate = signature
  state$pagination_reviews[[url]] = review
  # Quarantine the whole pass. A new page 2 must never complete an old page 1.
  state$pages = Filter(\(p) !identical(p$url, url), state$pages)
  state$failures = Filter(\(failure) failure$kind != 'category' ||
    !identical(failure$url, url), state$failures)
  refresh_save_state(state, directory)
  log_event(file.path(directory, 'progress.jsonl'), 'pagination_recheck',
    category_url = url, attempt = review$attempts,
    reason = 'Require two consecutive complete identical pagination passes')
  list(state = state, restart = T)
}

refresh_category_proof = \(state) {
  rows = lapply(seq_len(nrow(state$queue)), \(i) {
      name = state$queue$category_name[i]
      url = state$queue$category_url[i]
      pages = Filter(\(p) identical(p$url, url), state$pages)
      review = state$pagination_reviews[[url]]
      placements = isTRUE(review$verified) && !is.null(review$reference)
      independent = placements && !is.null(review$reference$accepted_pass)
      valid = Filter(\(p) p$status %in% c('ok', 'empty_category_verified',
          if (placements) 'overlapping_ids_between_pages'),
        pages)
      expected = if (length(pages)) pages[[1]]$last else NA_integer_
      observed = vapply(valid, \(p) p$page, integer(1))
      received = vapply(Filter(\(p) !is.na(p$last), pages),
        \(p) as.integer(p$page), 0L)
      complete = !is.na(expected) && identical(sort(unique(observed)), seq_len(
          expected))
      stable = is.null(review) || isTRUE(review$verified)
      complete = complete && stable
      data.frame(category_name = name, category_url = url,
        category_id = if (length(pages)) pages[[
              1]]$category_id else NA_character_,
        expected_pages = expected, successful_pages = length(unique(observed)),
        received_listing_pages = length(unique(received)),
        missing_listing_pages = if (is.na(expected)) NA_integer_ else
          length(setdiff(seq_len(expected), received)),
        reported_total_products = NA_integer_,
        total_count_evidence = if (independent) paste0(
          'Two complete disjoint name-asc passes prove the accepted ID set; ',
          'all primary IDs retained; original responses imported') else
          if (placements) paste0(
          'Exact unique ID match with two complete disjoint name-asc passes; ',
          'no retailer-wide stock total supplied') else paste0(
            "Shop HTML exposes pagination; no independent ", "total supplied"),
        independent_unique_product_ids = if (placements)
          length(unique(unlist(lapply(review$reference$passes[[1]],
            `[[`, 'ids')))) else NA_integer_,
        unique_product_ids = length(unique(unlist(lapply(valid, \(p) p$ids)))),
        category_status = if (
          complete) 'complete_observed_pagination' else 'unresolved',
        pagination_stability_status = if (is.null(review)) 'not_rechecked' else
          if (independent) 'verified_independent_reference' else
          if (placements) 'verified_boundary_placements' else
            if (stable) 'verified_consecutive_passes' else 'not_verified',
        duplicate_pagination_placements = if (placements)
          length(unlist(lapply(valid, `[[`, 'ids'))) -
            length(unique(unlist(lapply(valid, `[[`, 'ids')))) else 0L,
        reason = if (complete) paste0(
            "Every declared page archived and parsed; ",
            "terminal page reached") else
          if (!stable) 'pagination_stability_not_verified' else
          if (length(pages)) paste(unique(vapply(pages, \(p) p$status,
                  character(
                    1))),
              collapse = ';') else 'not_yet_requested',
        stringsAsFactors = F)
  })
  bind_rows(rows)
}

refresh_details_needed = \(products, rtd_ids) {
  core_missing = products$detail_coverage_method ==
    'individual_detail_required' |
    is.na(products$price_current) | is.na(products$volume_l) | is.na(
    products$alcohol_content)
  products$retailer_product_id %in% rtd_ids |
    (products$scope_status != 'excluded_nonfamily_product' & core_missing)
}

refresh_current_queue = \(directory, pages) {
  queue = refresh_scope_seeds(directory, dynamic = T)
  i = 1L
  while (i <= nrow(queue)) {
    first = pages[[paste(queue$category_url[i], 1L, sep = '|')]]
    if (!is.null(first)) {
      links = readRDS(first$parsed_file)$links
      new = links[links$decision == 'crawl' &
          !links$category_url %in% queue$category_url,
        c('category_name', 'category_url')]
      queue = bind_rows(queue, new)
    }
    i = i + 1L
  }
  queue[!duplicated(queue$category_url), ]
}

refresh_failure = \(url, page, kind, message, directory) {
  paths = sort(list.files(directory, '^[0-9]+_[a-z]+[.]json$',
    full.names = T))
  metadata = if (length(paths)) read_json(tail(paths, 1)) else list()
  list(url = url, page = page, kind = kind,
    response_status = as.integer(scalar(metadata$response_status)),
    observed_at_utc = scalar(metadata$observed_at_utc),
    recorded_at_utc = utc_now(), metadata_file = scalar(metadata$metadata_file),
    reason = message)
}

run_conalco_refresh = \(id, client = new_http_client(pause_seconds = 3,
    max_tries = 1, follow_redirects = F,
    request_scope =
      'authorized relevant assortment refresh; checkpointed'), dynamic = T,
  max_requests = 1500L, max_hours = 2) {
  stopifnot(length(max_requests) == 1L, is.finite(max_requests),
    max_requests > 0, max_requests == floor(max_requests),
    length(max_hours) == 1L, is.finite(max_hours), max_hours > 0)
  directory = run_dir(id)
  conalco_require_access_review(directory)
  checkpoint = file.path(directory, 'checkpoint.rds')
  if (file.exists(file.path(directory, 'collection_finished.json')))
    stop(paste0("Collection already finished; use the existing ",
        "immutable outputs"))
  if (!file.exists(checkpoint)) {
    state = list(queue = refresh_scope_seeds(directory, dynamic),
      pages = list(),
      details = list(), stage = 'categories', last_error = NULL)
    ensure_dir(file.path(directory, 'parsed_pages'))
    snapshot_source(ensure_dir(file.path(directory, 'collection_source')))
    refresh_save_state(state, directory)
    write_json_once(list(run_id = id, version = CATALOG_REFRESH_VERSION,
        scope = paste0("All drink-family and associated alcoholic ",
          "navigation categories, including RTD and RTS; ",
          "recursively discovered category-navigation links"),
        excluded_navigation = paste0(
          "Ingredients, nonbeverage accessories, editorial ",
          "and brand-only trees per existing catalog_links ", "rule"),
        detail_policy = paste0(
          "Product-specific full descriptions embedded in ",
          "fresh listings for every ID; individual detail ",
          "requests for RTD/RTS, absent descriptions and ",
          "unresolved core fields; no old-run responses"),
        discovery_policy = if (dynamic) 'current_navigation_v1' else 'legacy',
        seeds = state$queue, started_at_utc = utc_now()), file.path(
        directory, 'collection_scope.json'))
  } else state = readRDS(checkpoint)
  if (dynamic) {
    state$dynamic_navigation = T
    state$queue = refresh_current_queue(directory, state$pages)
    if (is.null(state$failures)) state$failures = list()
    state$stage = 'categories'
    state$last_error = NULL
  }
  seeds = refresh_scope_seeds(directory, dynamic)
  state$queue = bind_rows(state$queue,
    seeds[!seeds$category_url %in% state$queue$category_url, ])
  code_hash = substr(digest(vapply(c('R/catalog.R', 'R/catalog_refresh.R',
          'R/http.R',
          'scripts/refresh_conalco.R'), \(p) digest(file = p, algo =
            'sha256'), character(1)),
      algo = 'sha256'), 1, 12)
  source_directory = ensure_dir(file.path(directory, 'collection_source',
      paste0('parser_', CATALOG_VERSION, '_', code_hash)))
  if (!file.exists(file.path(source_directory, 'source_snapshot.json')))
    snapshot_source(source_directory)
  runtime = new.env(parent = emptyenv())
  runtime$cache = refresh_response_cache(directory)
  runtime$error = NULL
  route_paths = list.files(directory, '^category_endpoint_routes.*[.]json$',
    full.names = T)
  runtime$routes = unlist(lapply(route_paths, \(path) read_json(
        path)$routes), recursive = F)
  fetch = \(url, type = 'category', product_id = NA_character_, reuse = T) {
    actual = if (type == 'category') refresh_request_url(url,
        runtime$routes) else url
    tryCatch({
        metadata = refresh_fetch(actual, type, id, runtime, directory, client,
          product_id, max_requests = max_requests, max_hours = max_hours,
          global_access = dynamic, reuse = reuse)
        refresh_verify_route(metadata, url, actual)
      },
      error = \(e) {
        redirect_loop = grepl(
          'redirects hit maximum|Maximum.*redirects followed',
          conditionMessage(e), ignore.case = T)
        if (dynamic || type != 'category' || !identical(actual, url) ||
            !redirect_loop) stop(e)
        tryCatch(refresh_recover_public_endpoint(url, id, runtime, directory,
            client),
          error = \(recovery_error) stop(conditionMessage(e),
            '; documented public endpoint recovery unavailable: ',
            conditionMessage(recovery_error)))
    })
  }
  on.exit({
      if (!is.null(runtime$error)) state$last_error = runtime$error
      refresh_save_state(state, directory)
    }, add = T)
  tryCatch({
      i = 1L
      while (i <= nrow(state$queue)) {
        url = state$queue$category_url[i]
        name = state$queue$category_name[i]
        review = state$pagination_reviews[[url]]
        if (isTRUE(review$exhausted)) {
          i = i + 1L
          next
        }
        page = 1L
        last = 1L
        repeat {
          key = paste(url, page, sep = '|')
          previous = state$pages[[key]]
          if (dynamic && !is.null(previous) && !previous$status %in%
              c('ok', 'empty_category_verified', 'no_primary_listing',
                'duplicate_ids_on_page', 'overlapping_ids_between_pages'))
            previous = NULL
          if (!is.null(previous)) {
            last = previous$last
            if (is.na(last) || page >= last) break
            page = page + 1L
            next
          }
          request_url = if (page == 1L) url else paste0(url, '?p=', page)
          parsed = tryCatch({
              metadata = fetch(request_url, reuse = is.null(review) ||
                isTRUE(review$verified))
              refresh_page(metadata, name, url, page)
            }, error = \(e) {
              if (!dynamic || inherits(e, 'conalco_global_error')) stop(e)
              failure = refresh_failure(url, page, 'category',
                conditionMessage(e), directory)
              runtime$category_failure = failure
              log_event(file.path(directory, 'progress.jsonl'),
                'category_gap', url = url, page = page,
                response_status = failure$response_status,
                observed_at_utc = failure$observed_at_utc,
                metadata_file = failure$metadata_file,
                reason = failure$reason)
              NULL
            })
          if (is.null(parsed)) {
            state$failures[[key]] = runtime$category_failure
            refresh_save_state(state, directory)
            break
          }
          if (dynamic) state$failures[[key]] = NULL
          new = parsed$links[parsed$links$decision == 'crawl' &
              !parsed$links$category_url %in% state$queue$category_url, c(
              'category_name', 'category_url')]
          state$queue = bind_rows(state$queue, new)
          last = if (is.null(
              parsed$pagination)) NA_integer_ else parsed$pagination$last
          # Reuse the existing checks, including explicit empty-category
          # messages.
          members = lapply(Filter(\(p) identical(p$url, url) && p$page < page &&
                p$status == 'ok', state$pages),
            \(p) data.frame(retailer_product_id = p$ids, category_url =
                url))
          checks = catalog_page_checks(parsed, page, last, members, url)
          status = catalog_page_status(checks$good, checks$empty_verified,
            parsed)
          if (!is.null(parsed$unavailable_status)) status =
            parsed$unavailable_status
          if (page > 1L && !is.null(parsed$pagination) &&
              !identical(last, state$pages[[paste(url, 1L,
                    sep = '|')]]$last))
          status = 'pagination_changed_during_collection'
          if (anyDuplicated(parsed$rows$retailer_product_id)) status =
            'duplicate_ids_on_page'
          if (page > 1L && any(parsed$rows$retailer_product_id %in% unlist(
                lapply(members, `[[`, 'retailer_product_id'))))
          status = 'overlapping_ids_between_pages'
          if (dynamic && !status %in% c('ok', 'empty_category_verified',
              'no_primary_listing', 'duplicate_ids_on_page'))
            state$failures[[key]] = refresh_failure(url, page, 'category',
              status, directory)
          identity = if (dynamic) paste(key, metadata$metadata_file) else key
          path = file.path(directory, 'parsed_pages', paste0(substr(
                digest(identity,
                  algo = 'sha256'), 1, 20), '_', CATALOG_VERSION, '.rds'))
          parsed$doc = NULL
          if (!file.exists(path)) write_once(path, \(p) saveRDS(parsed, p))
          else stopifnot(identical(readRDS(path)$rows, parsed$rows))
          state$pages[[key]] = list(url = url, page = page, last = last,
            category_id = parsed$category_id, status = status,
            ids = parsed$rows$retailer_product_id, parsed_file = path,
            metadata_file = metadata$metadata_file)
          state$pages[[key]]$terminal_page_verified =
            parsed$terminal_page_verified
          refresh_save_state(state, directory)
          cat('PAGE', i, '/', nrow(state$queue), name, page, '/', last,
            nrow(parsed$rows), status, '\n')
          flush.console()
          if (is.na(last) || page >= last || !status %in% c('ok',
              'empty_category_verified',
              if (dynamic) 'overlapping_ids_between_pages')) break
          page = page + 1L
        }
        if (dynamic) {
          reviewed = refresh_review_pagination(state, url, directory, fetch)
          state = reviewed$state
          if (!is.null(reviewed$error)) stop(reviewed$error)
          if (reviewed$restart) next
        }
        i = i + 1L
      }
      state$stage = 'details'
      refresh_save_state(state, directory)
      active_pages = if (dynamic) Filter(\(p) p$url %in%
        state$queue$category_url, state$pages) else state$pages
      listings = bind_rows(lapply(active_pages, \(p) readRDS(
            p$parsed_file)$rows))
      if (dynamic && !nrow(listings)) {
        state$stage = 'partial'
        refresh_save_state(state, directory)
        return(invisible(state))
      }
      memberships = unique(listings[c('retailer_product_id',
            'retailer_category',
            'category_url',
            'category_page', 'listing_metadata_file')])
      # Select a single observed offer, retaining every membership separately.
      products = listings[!duplicated(listings$retailer_product_id), ]
      rtd_ids = unique(memberships$retailer_product_id[
          memberships$category_url %in%
            c('https://www.conalco.de/ready-to-drink/',
            'https://www.conalco.de/Ready-to-Serve')])
      needed = refresh_details_needed(products, rtd_ids)
      for (j in which(needed)) {
        product_id = products$retailer_product_id[j]
        if (!is.null(state$details[[product_id]])) next
        detail_key = paste('product', product_id, sep = '|')
        detail = tryCatch({
        metadata = fetch(products$product_url[j], 'product', product_id)
        detail = catalog_detail(metadata$metadata_file, products[j, ])
        doc = read_html(read_archive(metadata$metadata_file)$body,
          encoding = 'UTF-8')
        description = node_text_verbatim(doc,
          paste0('//*[', class_xpath('product-detail-description-text'), ']'))
        if (is.na(description) || nchar(description) < 20 ||
            !identical(detail$retailer_product_id, product_id))
          stop('Unresolved individual product description or identity: ',
            product_id)
        detail = attach_conalco_description_evidence(detail, description)
        detail$detail_coverage_method = 'individual_product_page'
        detail$listing_metadata_file = products$listing_metadata_file[j]
        detail
        }, error = \(e) {
          if (!dynamic || inherits(e, 'conalco_global_error')) stop(e)
          runtime$detail_failure = refresh_failure(
            products$product_url[j], 1L, 'product', conditionMessage(e),
            directory)
          NULL
        })
        if (is.null(detail)) {
          state$failures[[detail_key]] = runtime$detail_failure
          products$detail_coverage_method[j] = 'individual_detail_required'
          refresh_save_state(state, directory)
          next
        }
        if (dynamic) state$failures[[detail_key]] = NULL
        state$details[[product_id]] = detail
        refresh_save_state(state, directory)
        completed_required = sum(names(state$details) %in%
            products$retailer_product_id[needed])
        cat('DETAIL', completed_required, '/', sum(needed), product_id,
          '; stored_total', length(state$details), '\n')
        flush.console()
      }
      for (j in seq_len(nrow(products))) {
        product_id = products$retailer_product_id[j]
        detail = state$details[[product_id]]
        if (!is.null(detail)) {
          for (field in setdiff(names(detail), names(products))) products[[
              field]] = NA
          products[j, names(detail)] = detail
        }
      }
      categories = split(memberships, memberships$retailer_product_id)
      products$retailer_category = vapply(products$retailer_product_id,
        \(key) paste(unique(categories[[key]]$retailer_category), collapse =
            ' | '), character(1))
      products$category_urls = vapply(products$retailer_product_id,
        \(key) paste(unique(categories[[key]]$category_url), collapse = ' | '),
        character(1))
      products$in_rtd_rts_category = products$retailer_product_id %in% rtd_ids
      coverage = refresh_category_proof(state)
      for (j in seq_len(nrow(coverage))) {
        ids = memberships$retailer_product_id[memberships$category_url ==
            coverage$category_url[j]]
        coverage$processed_product_details[j] = sum(
          products$retailer_product_id %in% ids &
            products$detail_coverage_method %in% c(
              'same_product_full_description_embedded_in_listing',
              'individual_product_page'))
        coverage$individual_detail_pages[j] = sum(
          products$retailer_product_id %in% ids &
            products$detail_coverage_method == 'individual_product_page')
        coverage$unresolved_details[j] = length(unique(ids)) -
          coverage$processed_product_details[j]
      }
      # Ordinary listings need pagination; CMS containers are proved separately.
      status = if (all(coverage$category_status ==
            'complete_observed_pagination') &&
          all(coverage$unresolved_details == 0) && !anyDuplicated(
          products$retailer_product_id))
        'complete_pending_scope_validation' else 'partial'
      if (dynamic) {
        review = refresh_auto_navigation_review(state)
        unresolved = coverage$category_url[
          coverage$category_status != 'complete_observed_pagination']
        if (length(state$failures) || !nrow(products) ||
            !all(unresolved %in% review$category_url) ||
            any(coverage$unresolved_details != 0L)) {
          state$stage = 'partial'
          refresh_save_state(state, directory)
          return(invisible(state))
        }
      }
      output = ensure_dir(file.path('data/processed/catalog_refresh', id))
      write_csv_once(products, file.path(output, 'products_collected.csv'))
      write_csv_once(memberships, file.path(output, 'category_memberships.csv'))
      write_csv_once(coverage, file.path(output, 'category_completeness.csv'))
      write_csv_once(data.frame(retailer_product_id =
            products$retailer_product_id),
        file.path(output, 'product_ids.csv'))
      write_json_once(list(run_id = id, status = status, n_products = nrow(
            products),
          n_required_individual_product_details = sum(needed),
          n_stored_individual_product_details = length(state$details),
          ended_at_utc = utc_now(), output_directory = output,
          version = CATALOG_REFRESH_VERSION), file.path(directory,
          'collection_finished.json'))
      state$stage = status
      refresh_save_state(state, directory)
      cat('COLLECTION', status, nrow(products), 'products\n')
    }, error = \(e) {
      runtime$error = conditionMessage(e)
      state$last_error = conditionMessage(e)
      refresh_save_state(state, directory)
      log_event(file.path(directory, 'progress.jsonl'), 'collection_paused',
        'error',
        reason = conditionMessage(e), stage = state$stage)
      stop(e)
  })
  invisible(state)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) DETACHED START OF AN AUTHORIZED REFRESH
# <-----------------------------------------------------------------------------

# R replacement of the former scripts/start_conalco_refresh.py: refuses a
# second collector while
# the recorded PID still exists, appends to the collection log and keeps the
# collector running
# after the starter exits (nohup where available). Collection itself resumes
# from the checkpoint.
refresh_process_alive = \(pid) !is.na(pid) && pid %in% ps::ps_pids()

refresh_launch_process = \(run_id, log_path) {
  rscript = file.path(R.home('bin'), 'Rscript')
  arguments = c('scripts/refresh_conalco.R', paste0('--run-id=', run_id))
  nohup = Sys.which('nohup')
  command = if (nzchar(nohup)) c(nohup, rscript, arguments) else c(rscript,
      arguments)
  process = processx::process$new(command[1], command[-1], stdin = NULL,
    stdout = paste0('>>', log_path), stderr = '2>&1', cleanup = F,
    cleanup_tree = F)
  process$get_pid()
}

refresh_start_detached = \(run_id, log_directory =
  'logs/conalco_refresh_2026-10-16',
  launcher = refresh_launch_process) {
  if (!grepl('^catalog_refresh_[A-Za-z0-9_]+$', run_id))
    stop('Pass the existing authorized catalogue refresh ID')
  ensure_dir(log_directory)
  pid_path = file.path(log_directory, 'collection.pid')
  if (file.exists(pid_path)) {
    recorded = suppressWarnings(as.integer(readLines(pid_path, warn = F)[1]))
    if (refresh_process_alive(recorded))
      stop(paste0("Recorded collector PID still exists; do not ",
          "launch a duplicate"))
  }
  pid = launcher(run_id, file.path(log_directory, 'collection.log'))
  writeLines(as.character(pid), pid_path)
  pid
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) DAILY
# <-----------------------------------------------------------------------------

# Merged on 2026-10-19 from R/daily.R (file created 2026-10-09); content
# unchanged

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) DAILY READINESS
# <-----------------------------------------------------------------------------

daily_readiness = \(config_file = "config/daily_collection.json", today =
  format(Sys.time(),
    "%Y-%m-%d", tz = "Europe/Berlin")) {
  config = read_json(config_file, simplifyVector = T)
  if (today < config$start_date || today > config$end_date_inclusive)
    return(list(run = F, reason = "outside_collection_window", config = config))
  if (!isTRUE(config$enabled))
    stop(paste0("Daily collection is disabled: technical/storage/",
        "access checks are not released"))
  if (!isTRUE(config$access_review_approved) || is.null(
      config$access_review_evidence) ||
      !file.exists(config$access_review_evidence))
    stop("Missing documented access review for daily operation")
  if (!isTRUE(config$backup_restore_verified) || is.null(
      config$backup_directory) ||
      !dir.exists(config$backup_directory))
    stop("Missing verified permanent backup destination")
  if (is.null(config$technical_validation_run))
    stop("Missing validated technical run")
  validation_manifest_path = file.path(run_dir(
      config$technical_validation_run), "manifest.json")
  if (!file.exists(validation_manifest_path) ||
      !isTRUE(read_json(validation_manifest_path)$validation_passed))
    stop("Technical validation has not passed")
  start_paths = list.files("data/raw/conalco", pattern = "^started[.]json$",
    recursive = T,
    full.names = T)
  attempted_today = vapply(start_paths, \(path) {
      start_record = read_json(path)
      identical(start_record$purpose, "catalog") && identical(berlin_date(
          start_record$started_at_utc), today)
    }, logical(1))
  if (any(attempted_today))
    return(list(run = F, reason = "today_already_has_a_catalog_attempt",
        config = config))
  if (!is.null(config$project_contact) && nzchar(config$project_contact))
    Sys.setenv(APM_CONTACT = config$project_contact)
  list(run = T, reason = "ready", config = config)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) VERIFIED LOCAL BACKUP
# <-----------------------------------------------------------------------------

backup_catalog_run = \(manifest, backup_directory) {
  target = file.path(backup_directory, manifest$run_id)
  if (dir.exists(target))
    stop("Refusing to overwrite an existing backup: ", target)
  ensure_dir(target)
  raw_directory = run_dir(manifest$run_id)
  processed_directory = dirname(manifest$observation_file)
  for (source_directory in c(raw_directory, processed_directory)) {
    label = if (identical(source_directory, raw_directory))
      "raw"
    else "processed"
    source_paths = list.files(source_directory, recursive = T, full.names =
      T, all.files = T,
      no.. = T)
    for (source_path in source_paths) {
      relative_path = substring(source_path, nchar(source_directory) + 2L)
      destination = file.path(target, label, relative_path)
      ensure_dir(dirname(destination))
      if (!file.copy(source_path, destination, overwrite = F))
        stop("Backup copy failed: ", source_path)
      if (!identical(digest(file = source_path, algo = "sha256"), digest(
            file = destination,
            algo = "sha256")))
        stop("Backup verification failed: ", destination)
    }
  }
  receipt = list(run_id = manifest$run_id, verified_at_utc = utc_now(),
    backup_directory = target,
    byte_identical = T)
  write_json_once(receipt, file.path(raw_directory, "backup_receipt.json"))
  invisible(receipt)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) EXISTING DAILY PANEL PATH
# <-----------------------------------------------------------------------------

build_catalog_panel = \() {
  manifests = list.files("data/raw/conalco", pattern = "^manifest[.]json$",
    recursive = T,
    full.names = T)
  rows = list()
  sources = list()
  for (manifest_path in manifests) {
    manifest = read_json(manifest_path, simplifyVector = T)
    if (!identical(manifest$purpose, "catalog"))
      next
    reviews = list.files(dirname(manifest_path), pattern =
      "^manifest_conalco-catalog-.*[.]json$",
      full.names = T)
    if (length(reviews))
    manifest = read_json(reviews[which.max(file.info(reviews)$mtime)],
      simplifyVector = T)
    if (is.null(manifest$observation_file) || !file.exists(
        manifest$observation_file))
      next
    observation_file = manifest$observation_file
    expected = manifest$exports[[basename(observation_file)]]
    if (is.null(expected) || !identical(digest(file = observation_file,
          algo = "sha256"),
        expected))
      stop("Catalog export checksum mismatch: ", observation_file)
    observations = read_observations(observation_file)
    observations$run_status = manifest$status
    if (!"alcohol_content" %in% names(observations))
      stop("Missing catalogue schema: ", observation_file)
    observations$alcohol_content = as.numeric(observations$alcohol_content)
    rows[[length(rows) + 1L]] = observations
    sources[[length(sources) + 1L]] = list(run_id = manifest$run_id, path =
      observation_file, sha256 = expected)
  }
  if (!length(rows))
    stop("No finalized true catalog observations")
  observations = do.call(rbind, rows)
  key = paste(observations$run_id, observations$retailer_product_id,
    observations$parser_version, sep = "|")
  if (anyDuplicated(key))
    stop("Duplicate run/product/parser keys")
  observations = observations[order(observations$observed_at,
      observations$retailer_product_id), , drop = F]
  hash = substr(digest(observations, algo = "sha256"), 1, 16)
  path = file.path("data/processed/exports", paste0("catalog_panel_", hash,
      ".csv"))
  if (!file.exists(path)) {
    write_csv_once(observations, path)
    write_json_once(list(created_at_utc = utc_now(), sources = sources,
        sha256 = digest(file = path,
          algo = "sha256")), paste0(path, ".json"))
  }
  list(path = path, n_observations = nrow(observations), n_runs = length(
      unique(observations$run_id)))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) PREPARATION OF A NEW AUTHORIZED REFRESH RUN
# <-----------------------------------------------------------------------------

# Three steps, each recorded in the run folder: an offline draft, exactly
# three archived review pages after the user's go-ahead, and the reviewed
# decision in access_review.json. Only that file unlocks run_conalco_refresh().
CONALCO_HOST = 'https://www.conalco.de'
CONALCO_REVIEW_PAGES = c(robots = '/robots.txt', terms = '/agb',
  navigation = '/')
CONALCO_PAUSE_SECONDS = 3

conalco_new_refresh_run = \() {
  id = new_run_id('catalog_refresh')
  directory = ensure_dir(run_dir(id))
  write_json_once(list(run_id = id, purpose = 'catalog_refresh',
      started_at_utc = utc_now()), file.path(directory, 'started.json'))
  write_json_once(list(run_id = id, created_at_utc = utc_now(),
      review_pages_approved_by_user = F, approval_reference = '',
      review_pages = paste0(CONALCO_HOST, CONALCO_REVIEW_PAGES),
      review_tasks = c(
        paste0("Set review_pages_approved_by_user to true and record who ",
          "approved the three review requests in approval_reference"),
        paste0("After --fetch-review-pages: read the archived terms and ",
          "robots evidence (access_review_pages.json)"),
        paste0("Record the reviewed decision and the explicit user ",
          "authorization with --approve"))),
    file.path(directory, 'access_review_draft.json'))
  id
}

conalco_fetch_review_pages = \(id, client = new_http_client(
    pause_seconds = CONALCO_PAUSE_SECONDS, max_tries = 1,
    follow_redirects = F, request_scope =
      'access review pages before an authorized refresh'), dynamic = T) {
  directory = run_dir(id)
  draft = read_json(file.path(directory, 'access_review_draft.json'))
  if (!isTRUE(draft$review_pages_approved_by_user) || !nzchar(coalesce(
        scalar(draft$approval_reference), '')))
    stop('Review requests blocked: the draft lacks the user approval')
  if (length(list.files(directory, '^[0-9]+_[a-z]+[.]json$')))
    stop('Run already holds archived responses; start a new run')
  pages = list()
  for (i in seq_along(CONALCO_REVIEW_PAGES)) {
    type = names(CONALCO_REVIEW_PAGES)[i]
    response = client(paste0(CONALCO_HOST, CONALCO_REVIEW_PAGES[[i]]))
    pages[[type]] = archive_response(response, id, type, i)
    if (http_access_challenge(response))
      refresh_global_stop('Access challenge on archived review page')
    if (dynamic && !identical(berlin_date(response$observed_at_utc),
        berlin_date(read_json(file.path(directory,
              'started.json'))$started_at_utc)))
      refresh_global_stop('Berlin date changed during access review')
    status = pages[[type]]$response_status
    # A missing robots.txt sets no rule; every other page must be served.
    accepted = if (type == 'robots') c(200L, 404L, 410L) else 200L
    if (is.na(status) || !status %in% accepted) stop('Review page ', type,
        ' not available (HTTP ', status, '); start a new run')
  }
  robots = if (identical(pages$robots$response_status, 200L)) rawToChar(
      read_archive(pages$robots$metadata_file)$body) else ''
  seeds = refresh_scope_seeds(directory, dynamic)
  allowed = vapply(seeds$category_url, \(url) robots_allowed(robots, url),
    logical(1))
  delays = regmatches(robots, gregexpr(
      '(?im)^crawl-delay:\\s*[0-9]+(?:[.][0-9]+)?', robots, perl = T))[[1]]
  terms = read_archive(pages$terms$metadata_file)
  terms_text = node_text(read_html(terms$body, encoding = 'UTF-8'), '//main')
  if (is.na(terms_text)) terms_text = node_text(read_html(terms$body,
      encoding = 'UTF-8'), '//body')
  passages = regmatches(terms_text, gregexpr(paste0(
        '.{0,120}(?:scrap|crawl|robot|automatisiert|datenbank).{0,120}'),
      terms_text, ignore.case = T, perl = T))[[1]]
  evidence = list(run_id = id, fetched_at_utc = utc_now(),
    robots_status = pages$robots$response_status,
    robots_metadata = pages$robots$metadata_file,
    robots_disallowed_seeds = unname(seeds$category_url[!allowed]),
    robots_crawl_delay = unname(delays),
    terms_metadata = pages$terms$metadata_file,
    terms_raw_sha256 = pages$terms$raw_sha256,
    terms_passages_to_review = passages,
    navigation_metadata = pages$navigation$metadata_file,
    n_scope_seeds = nrow(seeds))
  write_json_once(evidence, file.path(directory, 'access_review_pages.json'))
  evidence
}

conalco_approve_refresh = \(id, decision, user_authorization) {
  directory = run_dir(id)
  pages = file.path(directory, 'access_review_pages.json')
  if (!file.exists(pages)) stop(
      'Approval blocked: run --fetch-review-pages first')
  evidence = read_json(pages)
  for (text in c(decision, user_authorization)) if (length(text) != 1L ||
        is.na(text) || nchar(trimws(text)) < 10L)
    stop('Approval needs a written decision and user authorization')
  if (length(evidence$robots_disallowed_seeds))
    stop('robots.txt disallows scope seeds; no refresh can be approved')
  delay = suppressWarnings(as.numeric(sub('^[^:]*:\\s*', '',
        unlist(evidence$robots_crawl_delay))))
  if (any(delay > CONALCO_PAUSE_SECONDS, na.rm = T))
    stop('robots.txt crawl-delay exceeds the fixed pause of ',
      CONALCO_PAUSE_SECONDS, ' s')
  # Same fields as the reviewed 2026-10-16 run; read_archive checks the hash.
  read_archive(evidence$terms_metadata)
  write_json_once(list(reviewed_at_utc = utc_now(),
      robots_status = evidence$robots_status,
      terms_metadata = evidence$terms_metadata,
      terms_raw_sha256 = evidence$terms_raw_sha256, decision = decision,
      pause_seconds = CONALCO_PAUSE_SECONDS,
      user_authorization = user_authorization),
    file.path(directory, 'access_review.json'))
  invisible(file.path(directory, 'access_review.json'))
}

conalco_require_access_review = \(directory) {
  path = file.path(directory, 'access_review.json')
  if (!file.exists(path)) stop('Conalco refresh blocked: no access_review.json')
  review = read_json(path)
  texts = c(scalar(review$decision, ''), scalar(review$user_authorization, ''))
  terms = scalar(review$terms_metadata, '')
  if (any(!nzchar(trimws(texts))) || !nzchar(terms) || !file.exists(terms) ||
      !identical(read_archive(terms)$meta$raw_sha256,
        scalar(review$terms_raw_sha256)))
    stop(paste0('Conalco refresh blocked: access_review.json lacks the ',
        'reviewed decision, authorization or archived terms'))
  invisible(review)
}
