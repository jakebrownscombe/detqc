# ---- The receiver's own transmitter, from the deployment sheet ----
# OTN's instrument deployment short form has a TRANSMITTER column: for a
# transceiver, the pinger built into the receiver; otherwise a sentinel tag
# on the same mooring, as the sheet's data dictionary defines it.
# read_deployments() reads it as `receiver_transmitter`. When the user gives
# no `reference_tags`, check_detset() and audit_listening() take these ids
# as the reference tags.

# A transmitter id is a code space and an ID joined by dashes, ending in
# digits: "A69-1601-12345". Case and spaces at the ends are tidied; a
# placeholder ("-", "NA", "none") is a blank; anything else that does not
# match, such as a bare "12345" with no code space, cannot be matched to a
# detection and becomes NA with a warning.
tx_id_pattern <- "^[A-Z0-9]+(-[A-Z0-9]+)*-[0-9]+$"

clean_receiver_transmitter <- function(df, file) {
  if (!"receiver_transmitter" %in% names(df)) return(list(df = df, malformed = 0L))
  v <- toupper(trimws(as.character(df$receiver_transmitter)))
  v[!nzchar(v) | v %in% na_text] <- NA_character_
  bad <- !is.na(v) & !grepl(tx_id_pattern, v)
  if (any(bad)) {
    one <- sum(bad) == 1
    warn_detqc(paste("%s: %s %s (code space and ID, as \"A69-1601-12345\"), such as \"%s\";",
                     "%s read as NA, and no reference tag is taken from %s."),
               basename(file), n_of(sum(bad), "receiver_transmitter value"),
               if (one) "is not a transmitter id" else "are not transmitter ids",
               df$receiver_transmitter[bad][1], if (one) "it was" else "they were",
               if (one) "it" else "them")
    v[bad] <- NA_character_
  }
  df$receiver_transmitter <- v
  list(df = df, malformed = sum(bad))
}

# The reference tags the deployments name: one row per transmitter, with
# the position of its latest deployment. A transmitter that the animals
# table also lists is left out with a warning: its detections would count
# both as an animal's and as a receiver's. NULL when no deployment names one.
deployment_reference_tags <- function(dep, ani, caller) {
  if (is.null(dep) || !"receiver_transmitter" %in% names(dep)) return(NULL)
  d <- as_dt(dep)[!is.na(receiver_transmitter) & nzchar(receiver_transmitter)]
  if (!nrow(d)) return(NULL)
  d <- d[order(-as.numeric(deploy_datetime))]
  d <- d[!duplicated(receiver_transmitter)]
  both <- intersect(d$receiver_transmitter, ani$transmitter)
  if (length(both)) {
    one <- length(both) == 1
    warn_detqc(paste("%s(): %s of the deployments %s also in the animals table, such as %s;",
                     "%s not used as %s. Correct whichever table is wrong, or give",
                     "`reference_tags` yourself."),
               caller, n_of(length(both), "receiver_transmitter id"), if (one) "is" else "are",
               both[1], if (one) "it is" else "they are",
               if (one) "a reference tag" else "reference tags")
    d <- d[!receiver_transmitter %in% both]
  }
  if (!nrow(d)) return(NULL)
  one <- nrow(d) == 1
  message(sprintf("%s(): no `reference_tags` given, so the %s of the deployments %s.", caller,
                  n_of(nrow(d), "receiver_transmitter id"),
                  if (one) "is the reference tag" else "are the reference tags"))
  out <- as_df(d[, list(transmitter = receiver_transmitter, deploy_lat, deploy_lon)])
  attr(out, "from_deployments") <- TRUE
  out
}

# How the log names the reference tags: a data frame as its rows, as
# deparse_parameters() writes any table, or where they came from.
reference_parameter <- function(ref) {
  if (isTRUE(attr(ref, "from_deployments"))) {
    return(sprintf("receiver_transmitter of the deployments (%s)", fmt_n(nrow(ref))))
  }
  ref
}

utils::globalVariables("receiver_transmitter")
