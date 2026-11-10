# ---- Co-detections, moved moorings and the checks against the field ----

# Pairs of detections of one transmitter by two receivers within `window`
# seconds: one transmission heard twice. Built once per run and kept in the
# context, because two checks read it. Only the columns the checks need are
# kept; the transmitter and the serials are looked up for flagged rows only.
codetections <- function(ctx) {
  if (!is.null(ctx$codet)) return(ctx$codet)
  d <- ctx$det
  k <- which(!is.na(d$.dep))
  pr <- codetect_pairs(d$transmitter[k], as.numeric(d$datetime[k]), d$receiver_serial[k],
                       ctx$settings$codetect_s)
  a <- k[pr$a]
  b <- k[pr$b]
  p <- data.table::data.table(t = as.numeric(d$datetime[a]), dep_a = d$.dep[a], row_a = d$.row[a],
                              dep_b = d$.dep[b], row_b = d$.row[b])
  ctx$codet <- p
  p
}

# Index pairs (a earlier, b later) of rows of one transmitter at two
# receivers within `window` seconds. The rows are sorted by transmitter and
# time, and each row is compared with the next row, then the one after, for
# as long as some row still has a partner inside the window. It holds two
# integer vectors per lag and only the rows still in reach, where a
# self-join would hold every row with each of its partners before the
# filter.
codetect_pairs <- function(transmitter, t, receiver, window) {
  n <- length(t)
  empty <- data.table::data.table(a = integer(), b = integer())
  if (n < 2) return(empty)
  tx <- data.table::chmatch(transmitter, unique(transmitter))
  rx <- data.table::chmatch(receiver, unique(receiver))
  o <- order(tx, t, rx, method = "radix")
  tx <- tx[o]
  t <- t[o]
  rx <- rx[o]
  cand <- seq_len(n - 1L)
  out <- list()
  lag <- 1L
  while (length(cand)) {
    cand <- cand[cand + lag <= n]
    j <- cand + lag
    near <- tx[j] == tx[cand] & t[j] - t[cand] <= window
    cand <- cand[near]
    j <- j[near]
    hit <- rx[j] != rx[cand]
    out[[lag]] <- list(a = o[cand[hit]], b = o[j[hit]])
    lag <- lag + 1L
  }
  res <- data.table::rbindlist(out)
  if (!nrow(res)) empty else res
}

# ---- Moved moorings ----
# For each deployment, the partner stations of its co-detections, by day.
# At every day boundary with at least `move_min` co-detections on each side
# the total variation distance between the partner shares before and after
# is computed; its maximum is the statistic. A mooring dragged or moved to
# another place hears a new set of neighbours, so the shares change almost
# wholly (distance near 1). Animals that shift their range change the
# shares too: a range shift can score 0.75 or more on animal co-detections
# alone. Reference tags do not move, so their
# co-detections are tested on their own, and they overrule an animal flag when they show no
# change at its date with enough co-detections on each side.
chk_moved_mooring <- function(ctx) {
  s <- ctx$settings
  tested <- partner_splits(ctx)
  hit <- tested[flag == TRUE]
  rows <- dep_rows(ctx, hit$.dep, split_date = as.Date(hit$split_day, origin = "1970-01-01"),
                   tv = hit$tv, basis = hit$basis, before = hit$before, after = hit$after)
  check_result(nrow(hit), nrow(tested), sprintf(paste(
    "partner shares before and after a date differ by a total variation distance of %g or",
    "more; %s flagged on reference-tag co-detections and %s on animal co-detections, which",
    "are candidates to confirm against field logs because a shift in the animals' range also",
    "changes the partners; %s overruled by reference tags; moves shorter than",
    "about two receiver spacings score below the threshold"
  ), s$move_tv, fmt_n(sum(hit$basis == "reference tags")), fmt_n(sum(hit$basis == "animals")),
  n_of(sum(tested$overruled), "animal flag")), rows)
}

# The split statistic of every deployment with enough co-detections, kept in
# the context: the field check reads it to leave moved moorings to this one.
partner_splits <- function(ctx) {
  if (!is.null(ctx$splits)) return(ctx$splits)
  p <- codetections(ctx)
  s <- ctx$settings
  is_ref <- if (is.null(ctx$ref)) {
    rep(FALSE, nrow(p))
  } else {
    ctx$det$transmitter[p$row_a] %in% ctx$ref$transmitter
  }
  ref_ev <- partner_events(ctx, p[is_ref])
  by_ref <- split_statistic(ref_ev, s$move_min)
  by_ani <- split_statistic(partner_events(ctx, p[!is_ref]), s$move_min)
  by_ani[, ref_tv := as.numeric(unlist(mapply(function(dep, split) {
    k <- ref_ev$.dep == dep
    tv_at(ref_ev$day[k], ref_ev$partner[k], split, s$move_min)
  }, .dep, split_day)))]
  by_ani[, overruled := tv >= s$move_tv & !is.na(ref_tv) & ref_tv < s$move_tv]
  by_ani[, flag := tv >= s$move_tv & !overruled]
  by_ani[, basis := rep("animals", .N)]
  by_ref[, c("ref_tv", "overruled", "basis") := list(tv, FALSE, rep("reference tags", .N))]
  by_ref[, flag := tv >= s$move_tv]
  # One row per deployment: a reference flag, else the animal result, else
  # the reference result.
  out <- rbind(by_ref[flag == TRUE], by_ani[!.dep %in% by_ref$.dep[by_ref$flag]],
               by_ref[flag == FALSE & !.dep %in% by_ani$.dep])
  data.table::setorderv(out, ".dep")
  ctx$splits <- out
  out
}

# One row per co-detection and deployment: the partner station and the day.
partner_events <- function(ctx, p) {
  st <- ctx$dep$station_id
  ev <- data.table::rbindlist(list(
    p[, list(.dep = dep_a, partner = st[dep_b], day = floor(t / 86400))],
    p[, list(.dep = dep_b, partner = st[dep_a], day = floor(t / 86400))]
  ))
  ev[st[.dep] != partner]
}

split_statistic <- function(ev, min_n) {
  if (!nrow(ev)) {
    return(data.table::data.table(.dep = integer(), tv = numeric(), split_day = numeric(),
                                  before = character(), after = character()))
  }
  ev[, partner_split(day, partner, min_n), by = ".dep"][!is.na(tv)]
}

# The total variation distance of partner shares before and after one day
# boundary; NA without `min_n` events on each side.
tv_at <- function(day, partner, split_day, min_n) {
  before <- day < split_day
  if (sum(before) < min_n || sum(!before) < min_n) return(NA_real_)
  lv <- unique(partner)
  a <- table(factor(partner[before], lv))
  b <- table(factor(partner[!before], lv))
  0.5 * sum(abs(a / sum(a) - b / sum(b)))
}

partner_split <- function(day, partner, min_n) {
  tab <- table(factor(day, levels = sort(unique(day))), factor(partner))
  days <- as.numeric(rownames(tab))
  cum <- apply(tab, 2, cumsum)
  if (is.null(dim(cum))) cum <- matrix(cum, nrow = 1, dimnames = list(NULL, colnames(tab)))
  tot <- colSums(tab)
  n_before <- rowSums(cum)
  n_after <- sum(tot) - n_before
  ok <- which(n_before >= min_n & n_after >= min_n)
  if (!length(ok)) return(list(tv = NA_real_, split_day = NA_real_, before = NA_character_,
                               after = NA_character_))
  tv <- vapply(ok, function(k) {
    0.5 * sum(abs(cum[k, ] / n_before[k] - (tot - cum[k, ]) / n_after[k]))
  }, numeric(1))
  k <- ok[which.max(tv)]
  top <- function(v) {
    paste(names(sort(v[v > 0], decreasing = TRUE))[seq_len(min(3, sum(v > 0)))], collapse = ", ")
  }
  list(tv = max(tv), split_day = days[k] + 1, before = top(cum[k, ]), after = top(tot - cum[k, ]))
}

# ---- The detection field ----
# What the field says each receiver key can hear: the form of `field` decides
# how. `pair_ok(a, b)` is TRUE where some position lets both receivers hear
# one transmission; `hears(key, lat, lon)` is TRUE where the receiver can
# hear a transmitter at the position.
field_reach <- function(field, ctx) {
  if (is.numeric(field) && length(field) == 1 && field > 0) return(reach_range(field, ctx))
  if (inherits(field, "de_field")) {
    th <- stats::qlogis(ctx$settings$p_min)
    sets <- lapply(colnames(field$eta), function(k) field$cells[field$eta[, k] >= th])
    names(sets) <- colnames(field$eta)
    return(reach_cells(sets, field$grid, field$key))
  }
  if (is.list(field) && all(c("distances", "range_m") %in% names(field))) {
    d <- field$distances
    key <- attr(d, "key")
    near <- d[d$distance_m <= field$range_m, ]
    sets <- split(near$cell, near[[key]])
    return(reach_cells(sets, attr(d, "grid"), key))
  }
  stop_detqc("`field` must be a range in metres, a de_field or list(distances = , range_m = ).")
}

reach_range <- function(range_m, ctx) {
  pos <- ctx$dep[, list(k = station_id, lat = deploy_lat, lon = deploy_lon)]
  pos <- pos[!duplicated(pos$k)]
  list(key = "station_id", keys = pos$k,
       pair_ok = function(a, b) {
         1000 * haversine_km(pos$lat[match(a, pos$k)], pos$lon[match(a, pos$k)],
                             pos$lat[match(b, pos$k)], pos$lon[match(b, pos$k)]) <= 2 * range_m
       },
       hears = function(k, lat, lon) {
         1000 * haversine_km(pos$lat[match(k, pos$k)], pos$lon[match(k, pos$k)], lat, lon) <=
           range_m
       })
}

reach_cells <- function(sets, grid, key) {
  if (!requireNamespace("terra", quietly = TRUE)) stop_detqc("The terra package is needed.")
  if (inherits(grid, "PackedSpatRaster")) grid <- terra::unwrap(grid)
  list(key = key, keys = names(sets),
       pair_ok = function(a, b) {
         mapply(function(x, y) length(intersect(sets[[x]], sets[[y]])) > 0,
                as.character(a), as.character(b))
       },
       hears = function(k, lat, lon) {
         cell <- raster_values(grid, lat, lon)$cell
         mapply(function(x, cl) !is.na(cl) && cl %in% sets[[x]], as.character(k), cell)
       })
}

# Co-detections that another check explains: a deployment whose position a
# position or station check flagged, a deployment that overlaps another of
# its receiver (detections of the overlap go to the later position), or one
# after the date its mooring moved.
explained_codetections <- function(ctx, p) {
  bad <- c(suspect_positions(ctx), overlapping_deps(ctx)$idx)
  sp <- partner_splits(ctx)
  sp <- sp[flag == TRUE]
  moved_after <- function(dep, t) {
    k <- match(dep, sp$.dep)
    !is.na(k) & floor(t / 86400) >= sp$split_day[k]
  }
  p$dep_a %in% bad | p$dep_b %in% bad | moved_after(p$dep_a, p$t) | moved_after(p$dep_b, p$t)
}

# Deployments whose stated position another check doubts.
suspect_positions <- function(ctx) {
  if (!is.null(ctx$suspect)) return(ctx$suspect)
  s <- ctx$settings
  out <- which(ctx$position != "ok")
  if (!is.null(ctx$water)) {
    ok <- which(ctx$position == "ok")
    wet <- in_water(ctx$water, ctx$dep$deploy_lat[ok], ctx$dep$deploy_lon[ok])
    out <- c(out, ok[wet %in% FALSE])
  }
  d <- station_spread(ctx)
  out <- sort(unique(c(out, d$.dep[d$spread_m > s$station_tol_m])))
  ctx$suspect <- out
  out
}

# The receiver key of each deployment, in the form the field uses.
field_keys <- function(reach, ctx, dep_idx) {
  if (reach$key == "station_id") return(ctx$dep$station_id[dep_idx])
  split_receiver(ctx$dep$receiver_serial[dep_idx])$serial
}

chk_codetection_out_of_range <- function(ctx) {
  if (is.null(ctx$field)) return(not_run("no `field` supplied"))
  reach <- field_reach(ctx$field, ctx)
  p <- codetections(ctx)
  skip <- explained_codetections(ctx, p)
  p <- p[!skip]
  p[, c("key_a", "key_b") := list(field_keys(reach, ctx, dep_a), field_keys(reach, ctx, dep_b))]
  # A receiver the field does not hold cannot be judged by it: on a network
  # export, most stations lie outside a small field.
  outside <- !(p$key_a %in% reach$keys & p$key_b %in% reach$keys)
  p <- p[!outside]
  pairs <- unique(p[key_a != key_b, list(key_a, key_b)])
  pairs[, ok := reach$pair_ok(key_a, key_b)]
  bad <- p[pairs[ok == FALSE], on = c("key_a", "key_b"), nomatch = NULL]
  rx <- ctx$det$receiver_serial
  rows <- bad[, list(transmitter = ctx$det$transmitter[row_a], datetime = num_utc(t), row_a,
                     row_b, receiver_a = rx[row_a], receiver_b = rx[row_b], key_a, key_b)]
  check_result(nrow(bad), nrow(p), sprintf(paste(
    "%s heard one transmission within %g s where the field allows no shared",
    "position; %s left to the position, overlap and moved-mooring checks;",
    "%s not tested, at a receiver the field does not hold"
  ), n_of(nrow(pairs[ok == FALSE]), "receiver pair"), ctx$settings$codetect_s,
  n_of(sum(skip), "co-detection"), fmt_n(sum(outside))), rows)
}

chk_reference_position <- function(ctx) {
  if (is.null(ctx$field)) return(not_run("no `field` supplied"))
  ref <- ctx$ref
  if (is.null(ref) || !all(c("deploy_lat", "deploy_lon") %in% names(ref))) {
    return(not_run("no `reference_tags` with deploy_lat and deploy_lon"))
  }
  reach <- field_reach(ctx$field, ctx)
  d <- ctx$det[transmitter %in% ref$transmitter & !is.na(.dep)]
  d <- d[!.dep %in% suspect_positions(ctx)]
  d[, key := field_keys(reach, ctx, .dep)]
  outside <- sum(!d$key %in% reach$keys)
  d <- d[key %in% reach$keys]
  pk <- unique(d[, list(transmitter, key)])
  m <- match(pk$transmitter, ref$transmitter)
  pk[, ok := reach$hears(key, ref$deploy_lat[m], ref$deploy_lon[m])]
  bad <- d[pk[ok == FALSE], on = c("transmitter", "key"), nomatch = NULL]
  check_result(nrow(bad), nrow(d), sprintf(
    paste("%s where the field gives under %g at the stated",
          "position; %s not tested, at a receiver the field does not hold"),
    n_of(nrow(pk[ok == FALSE]), "reference tag and receiver pair"), ctx$settings$p_min,
    n_of(outside, "detection")
  ), det_rows(ctx, bad$.row))
}
