# ---- Hard cases of the checks and the mortality rules ----
# Two synthetic benches, built here at test time. Each animal is one hard
# case the tests pin a band, a rule or a check result on. The datasets
# detqc_example and example_mortality hold the Stoney Lake study, which the
# examples and help pages read. Building the benches here keeps them out of
# the package tarball.

# Evaluates `code` in an environment whose parent is the data.table
# namespace, so data.table syntax works without attaching it, and with the
# global random seed restored afterwards.
hard_build <- function(seed, code) {
  env <- new.env(parent = asNamespace("data.table"))
  withr::with_preserve_seed({
    set.seed(seed)
    eval(code, env)
  })
}

# The 20 hard cases of the checks and the mortality triage (seed 20261029).
hard_cases <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- hard_build(20261029, quote({

      # ---- Study frame ----
      t_start <- as.POSIXct("2023-05-01 00:00:00", tz = "UTC")
      t_end <- as.POSIXct("2023-10-31 23:59:59", tz = "UTC")
      days <- seq(as.Date(t_start), as.Date(t_end), by = "day")
      lat0 <- 44.55
      lon0 <- -78.10
      km_lat <- 1 / 110.574
      km_lon <- 1 / (111.320 * cos(lat0 * pi / 180))

      # ---- Stations: a 4 by 5 grid at 1.5 km, and two close pairs ----
      grid <- CJ(row = 1:4, col = 1:5)
      grid[, station_id := sprintf("G%02d", .I)]
      grid[, `:=`(deploy_lat = lat0 + (row - 2.5) * 1.5 * km_lat,
                  deploy_lon = lon0 + (col - 3) * 1.5 * km_lon)]
      pairs <- data.table(
        station_id = c("P1A", "P1B", "P2A", "P2B"),
        deploy_lat = lat0 + c(-4.5, -4.5, 4.5, 4.5) * km_lat,
        deploy_lon = lon0 + c(0, 0.4, 0, 0.9) * km_lon
      )
      stations <- rbind(grid[, .(station_id, deploy_lat, deploy_lon)], pairs)

      # ---- Deployments: one receiver per station; G10 swaps receivers; G20 still deployed ----
      swap_time <- as.POSIXct("2023-08-01 12:00:00", tz = "UTC")
      dep <- stations[, .(
        receiver_serial = sprintf("VR2Tx-48%04d", .I), station_id, deploy_lat, deploy_lon,
        deploy_datetime = as.POSIXct("2023-04-25 12:00:00", tz = "UTC"),
        recover_datetime = as.POSIXct("2023-11-05 12:00:00", tz = "UTC"), deploy_z = 4
      )]
      dep[station_id == "G10", recover_datetime := swap_time]
      dep <- rbind(dep, dep[station_id == "G10"][, `:=`(
        receiver_serial = "VR2Tx-489999", deploy_datetime = swap_time,
        recover_datetime = as.POSIXct("2023-11-05 12:00:00", tz = "UTC")
      )])
      dep[station_id == "G20", recover_datetime := as.POSIXct(NA, tz = "UTC")]
      setorder(dep, station_id, deploy_datetime)

      # ---- Movement helpers ----
      neighbours <- function(s) {
        g <- grid[station_id == s]
        if (!nrow(g)) return(s)
        grid[abs(row - g$row) <= 1 & abs(col - g$col) <= 1, station_id]
      }
      walk <- function(d0, d1, start = "G08", p_detect = 0.85) {
        dd <- days[days >= as.Date(d0) & days <= as.Date(d1)]
        s <- start
        out <- list()
        for (k in seq_along(dd)) {
          s <- sample(neighbours(s), 1)
          if (runif(1) < p_detect) {
            out[[length(out) + 1]] <- data.table(date = dd[k], station_id = s)
          }
        }
        rbindlist(out)
      }
      stay <- function(d0, d1, st, p_detect = 1) {
        dd <- days[days >= as.Date(d0) & days <= as.Date(d1)]
        dd <- dd[runif(length(dd)) < p_detect]
        data.table(date = dd, station_id = rep_len(st, length(dd)))
      }
      alternate <- function(d0, d1, st) {
        dd <- days[days >= as.Date(d0) & days <= as.Date(d1)]
        data.table(date = dd, station_id = rep_len(st, length(dd)))
      }
      to_detections <- function(plan, lambda) {
        plan[, n := pmax(1L, rpois(.N, lambda))]
        # as.POSIXct() drops the time zone of a Date in R 4.2, so it is set here.
        out <- plan[rep(seq_len(.N), n)]
        out[, datetime := as.POSIXct(as.numeric(date) * 86400 + round(runif(.N, 0, 86399)),
                                     origin = "1970-01-01", tz = "UTC")]
        out[, n := NULL]
        out[]
      }

      # ---- The twenty cases ----
      cases <- list(
        list("dead_one_station", "dead", "2023-07-15", function() {
          rbind(walk("2023-05-03", "2023-07-14"), stay("2023-07-15", "2023-10-31", "G07"))
        }),
        list("dead_between_pair_400m", "dead", "2023-08-10", function() {
          rbind(walk("2023-05-03", "2023-08-09", "G02"),
                alternate("2023-08-10", "2023-10-31", c("P1A", "P1B")))
        }),
        list("dead_accel_flatline", "dead", "2023-08-20", function() {
          rbind(walk("2023-05-03", "2023-08-19", "G12"), stay("2023-08-20", "2023-10-31", "G13"))
        }),
        list("dormant_then_active", "review", NA, function() {
          rbind(walk("2023-05-03", "2023-05-31", "G05"), stay("2023-06-01", "2023-07-31", "G04"),
                walk("2023-08-01", "2023-10-31", "G04"))
        }),
        list("resident_two_sites", "review", NA, function() {
          alternate("2023-05-03", "2023-10-31", c("G06", "G06", "G07"))
        }),
        list("resident_one_site_active", "review", NA, function() {
          stay("2023-05-03", "2023-10-31", "G16")
        }),
        list("wide_ranger_brief_end", "review", NA, function() {
          rbind(walk("2023-05-03", "2023-10-20", "G01", 0.9),
                stay("2023-10-25", "2023-10-27", "G19"))
        }),
        list("record_ends_at_tag_life", "review", NA, function() {
          walk("2023-05-18", "2023-09-15", "G09")
        }),
        list("dead_from_first_detection", "dead", "2023-05-20", function() {
          stay("2023-05-20", "2023-10-31", "G03")
        }),
        list("dead_across_receiver_swap", "dead", "2023-07-10", function() {
          rbind(walk("2023-05-03", "2023-07-09", "G14"), stay("2023-07-10", "2023-10-31", "G10"))
        }),
        list("stray_detection_in_run", "dead", "2023-07-01", function() {
          rbind(walk("2023-05-03", "2023-06-30", "G11"), stay("2023-07-01", "2023-07-31", "G17"),
                data.table(date = as.Date("2023-08-01"), station_id = "G19"),
                stay("2023-08-02", "2023-10-31", "G17"))
        }),
        list("moored_reference_tag", "dead", "2023-05-02", function() {
          stay("2023-05-02", "2023-10-31", "G18")
        }),
        list("accel_flat_from_release", "dead", "2023-06-05", function() {
          stay("2023-06-05", "2023-10-31", "G15")
        }),
        list("single_detection", "review", NA, function() {
          data.table(date = as.Date("2023-06-10"), station_id = "G08")
        }),
        list("dead_between_pair_900m", "dead", "2023-08-25", function() {
          rbind(walk("2023-05-03", "2023-08-24", "G20"),
                alternate("2023-08-25", "2023-10-31", c("P2A", "P2B")))
        }),
        list("two_transmitters_alive", "review", NA, function() {
          walk("2023-05-03", "2023-10-31", "G13")
        }),
        list("accel_raw_uncalibrated", "review", NA, function() {
          walk("2023-05-03", "2023-10-31", "G02")
        }),
        list("dead_depth_tag", "dead", "2023-09-01", function() {
          rbind(walk("2023-05-03", "2023-08-31", "G18"), stay("2023-09-01", "2023-10-31", "G19"))
        }),
        list("sparse_alive", "review", NA, function() {
          walk("2023-05-03", "2023-10-31", "G05", 0.045)
        }),
        list("silent_gap_then_stationary", "dead", "2023-09-15", function() {
          rbind(walk("2023-05-03", "2023-07-15", "G01"), stay("2023-09-15", "2023-10-31", "G02"))
        })
      )
      case_tab <- data.table(
        animal_id = sprintf("EX-%02d", seq_along(cases)),
        case = vapply(cases, `[[`, "", 1), expected_band = vapply(cases, `[[`, "", 2),
        true_death_date = as.Date(vapply(cases, function(z) as.character(z[[3]]), ""))
      )
      case_tab[case == "moored_reference_tag", animal_id := "REF-01"]

      # ---- Transmitters and sensor calibration ----
      accel_cases <- c("dead_accel_flatline", "resident_one_site_active", "accel_flat_from_release",
                       "two_transmitters_alive", "accel_raw_uncalibrated")
      ani <- case_tab[, .(animal_id, transmitter = sprintf("A69-1604-%d", 5000 + .I),
                          case)]
      ani[case %in% accel_cases, transmitter := sprintf("A69-9004-%d", 7000 + .I)]
      extra <- ani[case %in% c("two_transmitters_alive", "dead_depth_tag")]
      extra[, transmitter := sprintf("A69-9004-%d", 8000 + .I)]
      ani <- rbind(ani, extra)
      ani[, sensor_type := NA_character_]
      ani[transmitter %like% "A69-9004-7", sensor_type := "acceleration"]
      ani[transmitter %like% "A69-9004-8", sensor_type := "depth"]
      ani[case == "dead_depth_tag" & !transmitter %like% "A69-9004-8", sensor_type := NA_character_]
      ani[, `:=`(sensor_unit = fifelse(sensor_type == "acceleration", "m/s2", "m"),
                 sensor_slope = fifelse(sensor_type == "acceleration", 0.013588, 0.1504),
                 sensor_intercept = fifelse(sensor_type == "acceleration", 0, -0.6018))]
      ani[is.na(sensor_type), `:=`(sensor_unit = NA, sensor_slope = NA, sensor_intercept = NA)]
      # The uncalibrated accelerometer: its type is known, its slope is not.
      ani[case == "accel_raw_uncalibrated", `:=`(sensor_slope = NA, sensor_intercept = NA)]

      # ---- Detections ----
      det <- rbindlist(lapply(seq_along(cases), function(k) {
        plan <- cases[[k]][[4]]()
        dead_from <- case_tab$true_death_date[k]
        plan[, dead := !is.na(dead_from) & date >= dead_from]
        if (case_tab$case[k] == "resident_one_site_active") plan[, dead := FALSE]
        d <- to_detections(plan, 14)
        d[, animal_id := case_tab$animal_id[k]]
        d
      }))
      # A two-transmitter tag alternates its transmitters: each detection takes one.
      tx <- ani[, .(transmitter = list(transmitter)), by = animal_id]
      det <- tx[det, on = "animal_id"]
      det[, transmitter := vapply(transmitter, function(v) v[sample.int(length(v), 1)], "")]
      det <- ani[, .(transmitter, sensor_type)][det, on = "transmitter"]
      det <- unique(det, by = c("transmitter", "datetime"))

      adc_accel <- function(value) pmin(255, pmax(0, round(value / 0.013588)))
      det[, sensor_raw := NA_real_]
      det[sensor_type == "acceleration" & !dead, sensor_raw := adc_accel(rgamma(.N, 2, 4))]
      det[sensor_type == "acceleration" & dead,
          sensor_raw := adc_accel(abs(rnorm(.N, 0.02, 0.012)))]
      det[sensor_type == "depth" & !dead, sensor_raw := round((runif(.N, 2, 9) + 0.6018) / 0.1504)]
      det[sensor_type == "depth" & dead,
          sensor_raw := round((6.1 + rnorm(.N, 0, 0.05) + 0.6018) / 0.1504)]
      # Shed at release: flat from its first day.
      flat_id <- case_tab[case == "accel_flat_from_release", animal_id]
      det[animal_id == flat_id & sensor_type == "acceleration",
          sensor_raw := adc_accel(abs(rnorm(.N, 0.02, 0.012)))]

      # One detection lands on the swap instant and belongs to the new receiver.
      swap_id <- case_tab[case == "dead_across_receiver_swap", animal_id]
      det[animal_id == swap_id & station_id == "G10" & date == as.Date(swap_time),
          datetime := c(swap_time, datetime[-1])]

      # ---- Receivers from the station and time ----
      dep_dt <- copy(dep)
      dep_dt[, end := fifelse(is.na(recover_datetime), as.POSIXct("2100-01-01", tz = "UTC"),
                              recover_datetime)]
      det[, row := .I]
      hit <- dep_dt[det, on = .(station_id, deploy_datetime <= datetime, end > datetime),
                    .(row = i.row, receiver_serial = x.receiver_serial)]
      det[hit, on = "row", receiver_serial := i.receiver_serial]
      stopifnot(!anyNA(det$receiver_serial))
      det[, sensor_value := NA_real_][, sensor_unit := NA_character_]
      setorder(det, animal_id, datetime)
      detections <- as.data.frame(det[, .(datetime, receiver_serial, transmitter, animal_id,
                                          station_id, sensor_raw, sensor_value, sensor_unit)])
      deployments <- as.data.frame(dep[, .(receiver_serial, station_id, deploy_datetime,
                                           recover_datetime, deploy_lat, deploy_lon, deploy_z)])
      animals <- as.data.frame(ani[, .(
        animal_id, transmitter, species = "Walleye",
        release_datetime = as.POSIXct("2023-05-01 15:00:00", tz = "UTC"),
        tag_model = fifelse(is.na(sensor_type), "V13-1x", "V13AP-1x"),
        tag_life_days = fifelse(case == "record_ends_at_tag_life", 137, 900),
        sensor_type, sensor_unit, sensor_slope, sensor_intercept
      )])
      cases_df <- as.data.frame(case_tab)
      detqc_example <- list(detections = detections, deployments = deployments, animals = animals,
                            cases = cases_df)
      detqc_example
    }))
    cache
  }
})

# The 20 hard cases of the mortality rule streams (seed 20261030).
hard_mortality <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- hard_build(20261030, quote({

      # ---- Study frame ----
      t0 <- as.Date("2024-05-01")
      horizon <- as.Date("2024-12-31")
      days <- seq(t0, horizon, by = "day")
      lat0 <- 44.50
      lon0 <- -78.20
      km_lat <- 1 / 110.574
      km_lon <- 1 / (111.320 * cos(lat0 * pi / 180))
      at <- function(x_km, y_km) list(lat = lat0 + y_km * km_lat, lon = lon0 + x_km * km_lon)

      # ---- Stations ----
      # A 5 by 5 grid at 1.2 km, so neighbours are separate 1 km sites; a pair 400 m
      # apart; a line of three 0.8 km apart (two sites within 1.6 km); a pair
      # 2.5 km apart; a line at 0.3 to 0.45 km spacings (unequal, so complete
      # linkage has no ties) for a site boundary; and M1, a mooring
      # dragged 1.5 km near the end of the record.
      grid <- CJ(row = 1:5, col = 1:5)
      grid[, station_id := sprintf("G%02d", .I)]
      grid[, c("deploy_lat", "deploy_lon") := at((col - 3) * 1.2, (row - 3) * 1.2)]
      extra <- data.table(
        station_id = c("P1A", "P1B", "N1", "N2", "N3", "F1", "F2", sprintf("L%d", 0:6)),
        x = c(0, 0.4, 6, 6.8, 7.6, -1.25, 1.25, -9 + c(0, 0.35, 0.65, 1.05, 1.4, 1.8, 2.25)),
        y = c(-4, -4, 0, 0, 0, 5, 5, rep(-6, 7))
      )
      extra[, c("deploy_lat", "deploy_lon") := at(x, y)]
      stations <- rbind(grid[, .(station_id, deploy_lat, deploy_lon)],
                        extra[, .(station_id, deploy_lat, deploy_lon)])
      m1 <- rbindlist(list(at(-5, 0), at(-5, 1.5)))

      # ---- Deployments: one receiver per station; M1 is redeployed after a drag ----
      drag <- as.POSIXct("2024-12-24 12:00:00", tz = "UTC")
      dep <- stations[, .(receiver_serial = sprintf("VR2Tx-49%04d", .I), station_id, deploy_lat,
                          deploy_lon,
                          deploy_datetime = as.POSIXct("2024-04-28 12:00:00", tz = "UTC"),
                          recover_datetime = as.POSIXct("2025-01-05 12:00:00", tz = "UTC"),
                          deploy_z = 6)]
      moved <- data.table(
        receiver_serial = c("VR2Tx-499001", "VR2Tx-499002"), station_id = "M1",
        deploy_lat = m1$lat, deploy_lon = m1$lon,
        deploy_datetime = c(as.POSIXct("2024-04-28 12:00:00", tz = "UTC"), drag),
        recover_datetime = c(drag, as.POSIXct("2025-01-05 12:00:00", tz = "UTC")), deploy_z = 6
      )
      dep <- rbind(dep, moved)
      setorder(dep, station_id, deploy_datetime)

      # The 0.3 km line is cut into sites by the same complete linkage the package
      # uses; the case planted across a boundary needs a pair in two sites.
      lpos <- extra[grepl("^L", station_id)]
      lsite <- cutree(hclust(dist(cbind(lpos$x, lpos$y)), "complete"), h = 1)
      cut_at <- which(diff(lsite) != 0)[1]
      boundary <- lpos$station_id[c(cut_at, cut_at + 1)]

      # ---- Paths: which station hears the animal on which day ----
      neighbours <- function(s) {
        g <- grid[station_id == s]
        grid[abs(row - g$row) <= 1 & abs(col - g$col) <= 1, station_id]
      }
      walk <- function(d0, d1, start = "G13", p = 0.9) {
        dd <- days[days >= as.Date(d0) & days <= as.Date(d1)]
        s <- start
        out <- vector("list", length(dd))
        for (k in seq_along(dd)) {
          s <- sample(neighbours(s), 1)
          if (runif(1) < p) out[[k]] <- data.table(date = dd[k], station_id = s)
        }
        rbindlist(out)
      }
      stay <- function(d0, d1, st, p = 1) {
        dd <- days[days >= as.Date(d0) & days <= as.Date(d1)]
        dd <- dd[runif(length(dd)) < p]
        data.table(date = rep(dd, each = length(st)), station_id = rep(st, length(dd)))
      }
      # A tour sweeps the grid row by row, three days at each station, so an
      # animal that is moving has a terminal window that spans the array and a
      # core of stations that shifts week by week, whatever the random draws.
      serpentine <- grid[order(row, fifelse(row %% 2 == 1, col, -col)), station_id]
      tour <- function(d0, d1, from = 1L) {
        dd <- days[days >= as.Date(d0) & days <= as.Date(d1)]
        k <- ((from - 1L + (seq_along(dd) - 1L) %/% 3L) %% length(serpentine)) + 1L
        out <- data.table(date = dd, station_id = serpentine[k])
        out[runif(.N) < 0.9]
      }
      some <- function(d0, d1, st, share) {
        s <- stay(d0, d1, st)
        s[runif(.N) < share]
      }

      # ---- The cases ----
      # Each: name, expected band, expected deciding rule, true death date (NA if
      # alive), sensor ("accel", "depth", "accel_raw" or none), tag life in days,
      # a second transmitter, and the path.
      case <- function(name, band, rule, death, sensor, life, dual, path) {
        list(name = name, band = band, rule = rule, death = as.Date(death), sensor = sensor,
             life = life, dual = dual, path = path)
      }
      cases <- list(
        case("accel_flatline_dead", "dead", "accel_flatline", "2024-08-01", "accel", 900, FALSE,
             function() {
               rbind(walk("2024-05-02", "2024-07-31", "G13"),
                     stay("2024-08-01", "2024-12-31", "G13"))
             }),
        case("resident_alive", "review", "terminal_run_near_stationary", NA, "accel", 900, FALSE,
             function() {
               rbind(some("2024-05-02", "2024-10-15", "G07", 0.7),
                     some("2024-05-02", "2024-10-15", "G08", 0.6))
             }),
        case("dormant_winter_alive", "alive", "none", NA, "accel", 900, FALSE,
             function() {
               rbind(walk("2024-05-02", "2024-08-31", "G19"),
                     stay("2024-09-01", "2024-10-20", "G19"),
                     tour("2024-10-21", "2024-11-20"))
             }),
        case("wide_ranger_alive", "alive", "none", NA, "", 900, FALSE,
             function() {
               rbind(tour("2024-05-02", "2024-10-10"), stay("2024-10-11", "2024-10-13", "G25"))
             }),
        case("record_ends_at_tag_life", "review", "record_end", NA, "", 150, FALSE,
             function() tour("2024-05-02", "2024-09-25")),
        case("record_ends_at_horizon", "review", "record_end", NA, "", 900, FALSE,
             function() tour("2024-05-02", "2024-12-31")),
        case("shed_at_release", "dead", "terminal_run", "2024-05-01", "", 900, FALSE,
             function() stay("2024-05-01", "2024-12-31", "G03")),
        case("shed_mid_record", "dead", "terminal_run", "2024-07-15", "", 900, FALSE,
             function() {
               rbind(walk("2024-05-02", "2024-07-14", "G23"),
                     stay("2024-07-15", "2024-12-31", c("P1A", "P1B")))
             }),
        case("dragged_mooring_dead", "review", "terminal_run_moved_station", "2024-08-01", "", 900,
             FALSE, function() {
               rbind(walk("2024-05-02", "2024-07-31", "G11"),
                     stay("2024-08-01", "2024-12-31", "M1"))
             }),
        case("one_receiver_only", "dead", "terminal_run", "2024-06-01", "", 900, FALSE,
             function() stay("2024-06-01", "2024-08-31", "G21")),
        case("dual_code_alive", "alive", "none", NA, "accel", 900, TRUE,
             function() tour("2024-05-02", "2024-11-01")),
        case("depth_sensor_only_dead", "dead", "terminal_run", "2024-09-01", "depth", 900, FALSE,
             function() {
               rbind(walk("2024-05-02", "2024-08-31", "G17"),
                     stay("2024-09-01", "2024-12-31", "G17"))
             }),
        case("accel_flat_then_moved", "review", "movement_resumption", "2024-10-01", "accel", 900,
             FALSE, function() {
               rbind(walk("2024-05-02", "2024-09-30", "G05"),
                     stay("2024-10-01", "2024-10-14", "G05"),
                     tour("2024-10-15", "2024-12-20", 16L))
             }),
        case("near_stationary_dead", "review", "terminal_run_near_stationary", "2024-07-01", "",
             900, FALSE, function() {
               rbind(walk("2024-05-02", "2024-06-30", "G15"),
                     stay("2024-07-01", "2024-12-31", "N2"),
                     some("2024-07-01", "2024-12-31", c("N1", "N3"), 0.5))
             }),
        case("sparse_record", "review", "sparse_record", NA, "", 900, FALSE,
             function() {
               data.table(date = as.Date(c("2024-05-20", "2024-06-11", "2024-07-02",
                                           "2024-07-30", "2024-08-19", "2024-09-09")),
                          station_id = c("G02", "G07", "G12", "G13", "G18", "G24"))
             }),
        case("persistent_far_pair_dead", "review", "persistence", "2024-05-11", "", 900, FALSE,
             function() {
               rbind(walk("2024-05-02", "2024-05-10", "G22"),
                     stay("2024-05-11", "2024-11-01", c("F1", "F2")))
             }),
        case("moored_reference_tag", "dead", "terminal_run", "2024-05-01", "", 900, FALSE,
             function() stay("2024-05-01", "2024-12-31", "G13")),
        case("accel_uncalibrated_alive", "alive", "none", NA, "accel_raw", 900, FALSE,
             function() tour("2024-05-02", "2024-11-01")),
        case("death_near_tag_life", "review", "record_end", "2024-08-06", "", 120, FALSE,
             function() {
               rbind(walk("2024-05-02", "2024-08-05", "G10"),
                     stay("2024-08-06", "2024-08-28", "G09"))
             }),
        case("dead_across_site_boundary", "review", "terminal_run_split_site", "2024-08-15", "",
             900, FALSE, function() {
               rbind(walk("2024-05-02", "2024-08-14", "G06"),
                     stay("2024-08-15", "2024-12-31", boundary))
             })
      )
      case_tab <- data.table(
        animal_id = sprintf("MX-%02d", seq_along(cases)),
        case = vapply(cases, `[[`, "", "name"), expected_band = vapply(cases, `[[`, "", "band"),
        expected_rule = vapply(cases, `[[`, "", "rule"),
        true_death_date = as.Date(vapply(cases, function(z) as.character(z$death), ""))
      )
      case_tab[case == "moored_reference_tag", animal_id := "REF-01"]

      # ---- Transmitters and calibration ----
      sensor <- vapply(cases, `[[`, "", "sensor")
      ani <- case_tab[, .(animal_id, case, transmitter = sprintf("A69-1604-%d", 61000 + .I),
                          sensor = sensor, tag_life_days = vapply(cases, `[[`, 0, "life"))]
      ani[sensor != "", transmitter := sprintf("A69-9004-%d", 62000 + .I)]
      dual <- ani[case == "dual_code_alive"][, `:=`(transmitter = "A69-1604-61999", sensor = "")]
      ani <- rbind(ani, dual)
      ani[, sensor_type := fcase(sensor %in% c("accel", "accel_raw"), "acceleration",
                                 sensor == "depth", "depth")]
      ani[, sensor_unit := fcase(sensor_type == "acceleration", "m/s2",
                                 sensor_type == "depth", "m")]
      ani[, sensor_slope := fcase(sensor == "accel", 0.013588, sensor == "depth", 0.1504)]
      ani[, sensor_intercept := fcase(sensor == "accel", 0, sensor == "depth", -0.6018)]

      # ---- Detections: bursts 60 to 180 s apart ----
      burst <- function(plan) {
        plan[, k := sample(3:6, .N, replace = TRUE)]
        out <- plan[rep(seq_len(.N), k)]
        by_day <- c("date", "station_id")
        out[, start := runif(1, 0, 80000), by = by_day]
        out[, gap := c(0, runif(.N - 1, 60, 180)), by = by_day]
        out[, datetime := as.POSIXct(as.numeric(date) * 86400 + round(start + cumsum(gap)),
                                     origin = "1970-01-01", tz = "UTC"), by = by_day]
        out[, c("k", "start", "gap") := NULL]
        out[]
      }
      det <- rbindlist(lapply(seq_along(cases), function(i) {
        d <- burst(unique(cases[[i]]$path()))
        d[, animal_id := case_tab$animal_id[i]]
        d[, dead := !is.na(case_tab$true_death_date[i]) & date >= case_tab$true_death_date[i]]
        d
      }))
      tx <- ani[, .(txs = list(transmitter)), by = animal_id]
      det <- tx[det, on = "animal_id"]
      det[, transmitter := vapply(txs, function(v) v[sample.int(length(v), 1)], "")][, txs := NULL]
      det <- ani[, .(transmitter, sensor)][det, on = "transmitter"]

      ## Sensor readings, in raw counts
      counts <- function(v, slope, icpt) pmin(255, pmax(0, round((v - icpt) / slope)))
      det[, sensor_raw := NA_real_]
      acc <- det$sensor %in% c("accel", "accel_raw")
      det[acc & !dead, sensor_raw := counts(rgamma(.N, 2, 2), 0.013588, 0)]
      det[acc & dead, sensor_raw := counts(abs(rnorm(.N, 0.02, 0.012)), 0.013588, 0)]
      # The dormant animal's accelerometer is flat while it rests, then active again.
      dorm <- case_tab[case == "dormant_winter_alive", animal_id]
      det[animal_id == dorm & date >= as.Date("2024-09-01") & date <= as.Date("2024-10-20"),
          sensor_raw := counts(abs(rnorm(.N, 0.02, 0.012)), 0.013588, 0)]
      moved <- case_tab[case == "accel_flat_then_moved", animal_id]
      det[animal_id == moved & date >= as.Date("2024-10-01"),
          sensor_raw := counts(abs(rnorm(.N, 0.02, 0.012)), 0.013588, 0)]
      dp <- det$sensor == "depth"
      det[dp & !dead, sensor_raw := counts(runif(.N, 2, 9), 0.1504, -0.6018)]
      det[dp & dead, sensor_raw := counts(6.1 + rnorm(.N, 0, 0.05), 0.1504, -0.6018)]

      ## Receivers from the station and time
      dep_dt <- copy(dep)
      det[, row := .I]
      hit <- dep_dt[det,
                    on = .(station_id, deploy_datetime <= datetime, recover_datetime > datetime),
                    .(row = i.row, receiver_serial = x.receiver_serial)]
      det[hit, on = "row", receiver_serial := i.receiver_serial]
      stopifnot(!anyNA(det$receiver_serial))
      setorder(det, animal_id, datetime)
      detections <- as.data.frame(det[, .(datetime, receiver_serial, transmitter, animal_id,
                                          station_id, sensor_raw)])
      deployments <- as.data.frame(dep)
      released <- as.POSIXct("2024-05-01 12:00:00", tz = "UTC")
      animals <- as.data.frame(ani[, .(animal_id, transmitter, species = "Walleye",
                                       release_datetime = released,
                                       tag_model = fifelse(is.na(sensor_type), "V13-1x",
                                                           "V13AP-1x"),
                                       tag_life_days, min_delay = 60, max_delay = 180,
                                       sensor_type, sensor_unit, sensor_slope, sensor_intercept)])
      example_mortality <- list(detections = detections, deployments = deployments,
                                animals = animals, cases = as.data.frame(case_tab))
      example_mortality
    }))
    cache
  }
})
