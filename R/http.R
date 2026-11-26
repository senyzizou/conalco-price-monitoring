# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261007

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) HTTP
# <-----------------------------------------------------------------------------

new_http_client = \(pause_seconds = 3, timeout_seconds = 30, max_tries = 3,
  request_scope =
  "sequential limited pilot",
  allowed_host = "www.conalco.de", follow_redirects = T) {
  state = new.env(parent = emptyenv())
  state$last_finished = as.POSIXct(NA)
  contact = Sys.getenv("APM_CONTACT", unset = "")
  ua = paste0("UKE-ZIS-AlcoholPricePilot/", SCRAPER_VERSION, paste0(
      " (scientific research; ",
      request_scope), if (nzchar(contact))
      paste0("; contact: ", contact)
    else "", ")")
  \(url) {
    if (!startsWith(url, paste0("https://", allowed_host, "/")))
      stop("Pilot host restriction: ", url)
    if (!is.na(state$last_finished)) {
      elapsed = as.numeric(difftime(Sys.time(), state$last_finished,
          units = "secs"))
      if (elapsed < pause_seconds)
        Sys.sleep(pause_seconds - elapsed)
    }
    started = utc_now()
    result = tryCatch({
        req = request(url)
        req = req_user_agent(req, ua)
        req = req_timeout(req, timeout_seconds)
        req = req_options(req, accept_encoding = 'gzip, deflate',
          followlocation = follow_redirects)
        req = req_retry(req, max_tries = max_tries, max_seconds = 90,
          retry_on_failure = T,
          is_transient = \(resp) resp_status(resp) %in% c(429L, 500L,
            502L, 503L, 504L))
        req = req_error(req, is_error = \(resp) F)
        resp = req_perform(req)
        list(request_url = url, final_url = resp$url,
          request_started_at_utc = started,
          observed_at_utc = utc_now(), response_status = resp_status(resp),
          headers = as.list(unclass(resp_headers(resp))),
          # An empty body (e.g. an empty robots.txt) keeps its HTTP status.
          body = if (resp_has_body(resp)) resp_body_raw(resp) else raw(),
          error = NA_character_, user_agent = ua,
          request_accept_encoding = 'gzip, deflate',
          archived_body_representation = 'decoded_http_entity')
      }, error = \(e) list(request_url = url, final_url = NA_character_,
        request_started_at_utc = started,
        observed_at_utc = utc_now(), response_status = NA_integer_, headers =
        list(),
        body = NULL, error = conditionMessage(e), user_agent = ua))
    state$last_finished = Sys.time()
    result
  }
}
# A small, conservative robots evaluator for this fixed-host pilot. Longest
# matching Allow/Disallow wins; wildcard and terminal $ are supported.

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) ROBOTS ACCESS RULES
# <-----------------------------------------------------------------------------

robots_allowed = \(text, url, agent = "UKE-ZIS-AlcoholPricePilot") {
  lines = trimws(sub("#.*$", "", strsplit(text, "\n", fixed = T)[[1]]))
  groups = list()
  agents = character()
  rules = list()
  had_rule = F
  flush = \(groups, agents, rules) {
    if (length(agents))
    groups[[length(groups) + 1L]] = list(agents = agents, rules = rules)
    groups
  }
  for (line in lines[nzchar(lines)]) {
    if (!grepl(":", line, fixed = T))
      next
    key = tolower(trimws(sub(":.*$", "", line)))
    value = trimws(sub("^[^:]*:", "", line))
    if (key == "user-agent") {
      if (had_rule) {
        groups = flush(groups, agents, rules)
        agents = character()
        rules = list()
        had_rule = F
      }
      agents = c(agents, tolower(value))
    }
    else if (key %in% c("allow", "disallow")) {
        had_rule = T
        rules[[length(rules) + 1L]] = list(kind = key, path = value)
      }
  }
  groups = flush(groups, agents, rules)
  specific = vapply(groups, \(g) any(vapply(g$agents, \(a) a != "*" &&
          grepl(a, tolower(agent), fixed = T), logical(1))), logical(1))
  chosen = if (any(specific))
    groups[specific]
  else groups[vapply(groups, \(g) "*" %in% g$agents, logical(1))]
  path = sub("^https://[^/]+", "", url)
  matches = list()
  for (g in chosen) for (rule in g$rules) {
      if (!nzchar(rule$path))
        next
      parts = strsplit(rule$path, "*", fixed = T)[[1]]
      escape = \(x) gsub("([][{}()+?.^|\\\\])", "\\\\\\1", x, perl = T)
      terminal = endsWith(rule$path, "$")
      pattern = sub("\\$$", "", rule$path)
      parts = strsplit(pattern, "*", fixed = T)[[1]]
      regex = paste0("^", paste(vapply(parts, escape, character(1)),
          collapse =
            ".*"),
        if (terminal)
          "$"
        else "")
      if (grepl(regex, path, perl = T))
      matches[[length(matches) + 1L]] = c(length = nchar(gsub("*", "",
            pattern,
            fixed = T)), allow = as.integer(rule$kind == "allow"))
    }
  if (!length(matches))
    return(T)
  m = do.call(rbind, matches)
  best = m[m[, "length"] == max(m[, "length"]), , drop = F]
  any(best[, "allow"] == 1)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) CONTROLLED SAME-ORIGIN REDIRECTS
# <-----------------------------------------------------------------------------

http_url_identity = \(url) {
  # xml2 also exports url_parse; use httr2's parser with its URL builder.
  parts = httr2::url_parse(url)
  parts$scheme = tolower(parts$scheme)
  parts$hostname = tolower(parts$hostname)
  parts$fragment = NULL
  if (parts$scheme == 'https' && length(parts$port) == 1L &&
      parts$port %in% c(443L, '443'))
    parts$port = NULL
  url_build(parts)
}

http_redirect_url = \(url, headers, visited = character()) {
  locations = headers[tolower(names(headers)) == 'location']
  if (length(locations) != 1L || length(locations[[1]]) != 1L)
    stop('HTTP redirect blocked: missing or ambiguous Location')
  location = scalar(locations[[1]], '')
  if (!nzchar(location) || grepl('[[:cntrl:]]', location))
    stop('HTTP redirect blocked: invalid Location')
  target = http_url_identity(url_absolute(location, url))
  origin = httr2::url_parse(http_url_identity(url))
  destination = httr2::url_parse(target)
  if (destination$scheme != 'https' ||
      !identical(destination$hostname, origin$hostname) ||
      !identical(destination$port, origin$port) ||
      !is.null(destination$username) || !is.null(destination$password))
    stop('HTTP redirect blocked: target is not the same HTTPS origin')
  if (identical(target, http_url_identity(url)))
    stop('HTTP redirect blocked: self redirect at ', url)
  previous = vapply(visited, http_url_identity, character(1))
  if (target %in% previous)
    stop('HTTP redirect blocked: redirect loop to ', target)
  target
}

http_access_challenge = \(response) {
  headers = response$headers
  if (identical(scalar(headers[['cf-mitigated']], ''), 'challenge')) return(T)
  if (is.null(response$body)) return(F)
  text = iconv(rawToChar(head(response$body, 16000L)),
    from = 'UTF-8', to = 'UTF-8', sub = '')
  grepl(paste0('<title>\\s*(Access denied|Just a moment|Forbidden)',
      '|<form[^>]+(?:id|action)=["\x27][^"\x27]*captcha'),
    text, ignore.case = T, perl = T)
}
