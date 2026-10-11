# 규칙: docs/03-recommendation.md
# 날씨·알림 API는 호출하지 않는다. 화면에서 시각을 직접 받는다.

SEAT_FULL_CONGESTION <- 34

sample_stations <- data.frame(
  station_id = c("221", "222", "226", "232"),
  station_name = c("강남", "역삼", "사당", "신도림"),
  stringsAsFactors = FALSE
)

weekday_slots <- c("07:30", "08:00", "08:30", "09:00", "18:00", "18:30", "19:00", "21:30")
weekday_congestion <- list(
  "221" = c(92, 110, 98, 71, 60, 45, 30, 20),
  "222" = c(88, 105, 95, 68, 50, 38, 28, 18),
  "226" = c(80, 120, 101, 74, 55, 40, 32, 22),
  "232" = c(76, 99, 90, 65, 48, 36, 29, 21)
)

build_congestion <- function() {
  rows <- lapply(names(weekday_congestion), function(station_id) {
    base <- weekday_congestion[[station_id]]
    rbind(
      data.frame(station_id = station_id, day_type = "평일", slot = weekday_slots, congestion = base),
      data.frame(station_id = station_id, day_type = "토요일", slot = weekday_slots, congestion = round(base * 0.7)),
      data.frame(station_id = station_id, day_type = "일요일", slot = weekday_slots, congestion = round(base * 0.5))
    )
  })
  out <- do.call(rbind, rows)
  out$direction <- "내선"
  out$station_id <- as.character(out$station_id)
  rownames(out) <- NULL
  out
}

sample_congestion <- build_congestion()

trip_row <- function(origin_id, dest_id, depart, arrive, day_type, path) {
  data.frame(
    origin_id = origin_id,
    dest_id = dest_id,
    origin_name = sample_stations$station_name[match(origin_id, sample_stations$station_id)],
    dest_name = sample_stations$station_name[match(dest_id, sample_stations$station_id)],
    direction = "내선",
    day_type = day_type,
    depart = depart,
    arrive = arrive,
    path_key = paste(path, collapse = ">"),
    stringsAsFactors = FALSE
  )
}

expand_days <- function(row) {
  rbind(
    transform(row, day_type = "평일"),
    transform(row, day_type = "토요일"),
    transform(row, day_type = "일요일")
  )
}

sample_trips <- rbind(
  expand_days(trip_row("226", "221", "07:40", "07:52", "평일", c("226", "221"))),
  expand_days(trip_row("226", "221", "08:10", "08:22", "평일", c("226", "221"))),
  expand_days(trip_row("226", "221", "08:40", "08:52", "평일", c("226", "221"))),
  expand_days(trip_row("226", "221", "09:10", "09:22", "평일", c("226", "221"))),
  expand_days(trip_row("226", "221", "18:10", "18:22", "평일", c("226", "221"))),
  expand_days(trip_row("226", "221", "19:10", "19:22", "평일", c("226", "221"))),
  expand_days(trip_row("226", "221", "21:40", "21:52", "평일", c("226", "221"))),
  expand_days(trip_row("221", "226", "18:40", "18:52", "평일", c("221", "226"))),
  expand_days(trip_row("221", "226", "19:20", "19:32", "평일", c("221", "226"))),
  expand_days(trip_row("221", "226", "19:25", "19:37", "평일", c("221", "226"))),
  expand_days(trip_row("221", "226", "21:50", "22:02", "평일", c("221", "226"))),
  expand_days(trip_row("232", "221", "07:50", "08:12", "평일", c("232", "221"))),
  expand_days(trip_row("232", "221", "08:20", "08:42", "평일", c("232", "221"))),
  expand_days(trip_row("232", "221", "18:20", "18:42", "평일", c("232", "221"))),
  expand_days(trip_row("221", "232", "08:15", "08:35", "평일", c("221", "232"))),
  expand_days(trip_row("221", "232", "09:05", "09:25", "평일", c("221", "232")))
)
rownames(sample_trips) <- NULL

time_to_minutes <- function(hhmm) {
  hhmm <- trimws(hhmm)
  if (!grepl("^\\d{2}:\\d{2}$", hhmm)) {
    return(NA_integer_)
  }
  parts <- as.integer(strsplit(hhmm, ":", fixed = TRUE)[[1]])
  if (any(is.na(parts)) || parts[[1]] > 27 || parts[[2]] > 59) {
    return(NA_integer_)
  }
  parts[[1]] * 60L + parts[[2]]
}

minutes_to_time <- function(total) {
  sprintf("%02d:%02d", total %/% 60, total %% 60)
}

time_to_slot <- function(hhmm) {
  minutes <- time_to_minutes(hhmm)
  minutes_to_time((minutes %/% 30) * 30)
}

path_ids <- function(path_key) {
  strsplit(path_key, ">", fixed = TRUE)[[1]]
}

path_max_congestion <- function(path_key, day_type, direction, slot, congestion) {
  station_ids <- path_ids(path_key)
  rows <- congestion[
    congestion$station_id %in% station_ids &
      congestion$day_type == day_type &
      congestion$direction == direction &
      congestion$slot == slot,
  ]
  if (nrow(rows) < length(unique(station_ids))) {
    return(NA_real_)
  }
  max(rows$congestion)
}

score_trips <- function(trips, congestion) {
  if (nrow(trips) == 0) {
    trips$slot <- character()
    trips$path_max <- numeric()
    trips$verdict <- character()
    return(trips)
  }
  trips$slot <- vapply(trips$depart, time_to_slot, character(1))
  trips$path_max <- mapply(
    path_max_congestion,
    trips$path_key,
    trips$day_type,
    trips$direction,
    trips$slot,
    MoreArgs = list(congestion = congestion)
  )
  trips$verdict <- "후보"
  trips
}

empty_result <- function(message) {
  list(message = message, table = NULL)
}

holiday_result <- function() {
  empty_result("공휴일 혼잡도 열이 없습니다. 혼잡도 추천은 하지 않습니다. 막차는 시간표가 들어온 뒤에 계산합니다.")
}

route_trips <- function(origin_id, dest_id, day_type, trips) {
  trips[trips$origin_id == origin_id & trips$dest_id == dest_id & trips$day_type == day_type, , drop = FALSE]
}

mark_pick <- function(trips, pick_index) {
  if (nrow(trips) == 0) {
    return(trips)
  }
  trips$verdict[trips$verdict == "추천"] <- "후보"
  if (!is.null(pick_index)) {
    trips$verdict[[pick_index]] <- "추천"
  }
  trips[order(vapply(trips$depart, time_to_minutes, numeric(1))), , drop = FALSE]
}

pick_lowest <- function(kept) {
  if (nrow(kept) == 0) {
    return(NULL)
  }
  order_idx <- order(kept$path_max, -vapply(kept$depart, time_to_minutes, numeric(1)))
  kept[order_idx[[1]], , drop = FALSE]
}

pick_latest <- function(kept) {
  if (nrow(kept) == 0) {
    return(NULL)
  }
  order_idx <- order(-vapply(kept$depart, time_to_minutes, numeric(1)))
  kept[order_idx[[1]], , drop = FALSE]
}

result_from_pick <- function(scored, pick, empty_message) {
  if (is.null(pick)) {
    scored$verdict[scored$verdict == "후보"] <- "탈락"
    return(list(message = empty_message, table = display_table(mark_pick(scored, NULL))))
  }
  pick_key <- paste(pick$depart, pick$arrive)
  keys <- paste(scored$depart, scored$arrive)
  list(
    message = sprintf(
      "%s %s에 출발하면 %s에 도착합니다. 경로 혼잡도 최댓값은 %s%%입니다.",
      pick$origin_name,
      pick$depart,
      pick$arrive,
      pick$path_max
    ),
    table = display_table(mark_pick(scored, which(keys == pick_key)[[1]]))
  )
}

display_table <- function(trips) {
  if (nrow(trips) == 0) {
    return(data.frame(출발 = character(), 도착 = character(), 혼잡도 = character(), 판정 = character()))
  }
  data.frame(
    출발 = trips$depart,
    도착 = trips$arrive,
    혼잡도 = ifelse(is.na(trips$path_max), "없음", paste0(trips$path_max, "%")),
    판정 = trips$verdict,
    stringsAsFactors = FALSE
  )
}

recommend_on_time <- function(origin_id, dest_id, arrive_by, day_type,
                              trips = sample_trips, congestion = sample_congestion) {
  if (day_type == "공휴일") {
    return(holiday_result())
  }
  arrive_limit <- time_to_minutes(arrive_by)
  if (is.na(arrive_limit)) {
    return(empty_result("도착 시각은 09:00처럼 입력합니다."))
  }
  candidates <- score_trips(route_trips(origin_id, dest_id, day_type, trips), congestion)
  if (nrow(candidates) == 0) {
    return(empty_result("이 구간의 샘플 시각표가 없습니다."))
  }
  late <- vapply(candidates$arrive, time_to_minutes, numeric(1)) > arrive_limit
  candidates$verdict[late] <- "탈락: 도착 시각을 넘김"
  candidates$verdict[is.na(candidates$path_max)] <- "탈락: 혼잡도 없음"
  kept <- candidates[candidates$verdict == "후보", , drop = FALSE]
  result_from_pick(candidates, pick_lowest(kept), "제시간에 도착하면서 혼잡도를 알 수 있는 출발이 없습니다.")
}

recommend_delay_departure <- function(origin_id, dest_id, earliest, latest, day_type,
                                      trips = sample_trips, congestion = sample_congestion) {
  if (day_type == "공휴일") {
    return(holiday_result())
  }
  earliest_min <- time_to_minutes(earliest)
  latest_min <- time_to_minutes(latest)
  if (is.na(earliest_min) || is.na(latest_min)) {
    return(empty_result("출발 구간은 18:00, 22:00처럼 입력합니다."))
  }
  candidates <- score_trips(route_trips(origin_id, dest_id, day_type, trips), congestion)
  if (nrow(candidates) == 0) {
    return(empty_result("이 구간의 샘플 시각표가 없습니다."))
  }
  depart_min <- vapply(candidates$depart, time_to_minutes, numeric(1))
  candidates$verdict[depart_min < earliest_min | depart_min > latest_min] <- "탈락: 출발 구간 밖"
  candidates$verdict[is.na(candidates$path_max) & candidates$verdict == "후보"] <- "탈락: 혼잡도 없음"
  kept <- candidates[candidates$verdict == "후보", , drop = FALSE]
  result_from_pick(candidates, pick_lowest(kept), "그 구간에 혼잡도를 알 수 있는 출발이 없습니다.")
}

recommend_before_rain <- function(origin_id, dest_id, arrive_by, day_type, rain_from,
                                  trips = sample_trips, congestion = sample_congestion) {
  if (!nzchar(trimws(rain_from))) {
    base <- recommend_on_time(origin_id, dest_id, arrive_by, day_type, trips, congestion)
    base$message <- paste0("예보가 없습니다. ", base$message)
    return(base)
  }
  base <- recommend_on_time(origin_id, dest_id, arrive_by, day_type, trips, congestion)
  rain_min <- time_to_minutes(rain_from)
  if (is.na(rain_min)) {
    return(empty_result("비가 시작되는 시각은 18:00처럼 입력합니다."))
  }
  if (is.null(base$table)) {
    return(base)
  }
  # 제시간 도착 결과 표를 다시 계산해 비 시각 이전만 남긴다.
  candidates <- score_trips(route_trips(origin_id, dest_id, day_type, trips), congestion)
  arrive_limit <- time_to_minutes(arrive_by)
  late <- vapply(candidates$arrive, time_to_minutes, numeric(1)) > arrive_limit
  rainy <- vapply(candidates$depart, time_to_minutes, numeric(1)) >= rain_min
  candidates$verdict[late] <- "탈락: 도착 시각을 넘김"
  candidates$verdict[candidates$verdict == "후보" & rainy] <- "탈락: 비 시작 이후"
  candidates$verdict[is.na(candidates$path_max) & candidates$verdict == "후보"] <- "탈락: 혼잡도 없음"
  kept <- candidates[candidates$verdict == "후보", , drop = FALSE]
  result_from_pick(candidates, pick_lowest(kept), "비 오기 전에 도착하는 출발이 없습니다.")
}

recommend_seated <- function(origin_id, dest_id, arrive_by, day_type,
                             trips = sample_trips, congestion = sample_congestion) {
  if (day_type == "공휴일") {
    return(holiday_result())
  }
  arrive_limit <- time_to_minutes(arrive_by)
  if (is.na(arrive_limit)) {
    return(empty_result("도착 시각은 22:00처럼 입력합니다."))
  }
  candidates <- score_trips(route_trips(origin_id, dest_id, day_type, trips), congestion)
  if (nrow(candidates) == 0) {
    return(empty_result("이 구간의 샘플 시각표가 없습니다."))
  }
  late <- vapply(candidates$arrive, time_to_minutes, numeric(1)) > arrive_limit
  candidates$verdict[late] <- "탈락: 도착 시각을 넘김"
  crowded <- !is.na(candidates$path_max) & candidates$path_max >= SEAT_FULL_CONGESTION
  candidates$verdict[candidates$verdict == "후보" & crowded] <- "탈락: 좌석이 가득 참"
  candidates$verdict[is.na(candidates$path_max) & candidates$verdict == "후보"] <- "탈락: 혼잡도 없음"
  kept <- candidates[candidates$verdict == "후보", , drop = FALSE]
  result_from_pick(
    candidates,
    pick_latest(kept),
    "앉을 자리가 있으면서 제시간에 도착하는 출발이 없습니다. 34%는 좌석이 가득 찬 상태입니다."
  )
}

recommend_last_quiet <- function(origin_id, dest_id, day_type, congestion_cap,
                                 trips = sample_trips, congestion = sample_congestion) {
  if (day_type == "공휴일") {
    return(holiday_result())
  }
  if (is.na(congestion_cap) || congestion_cap <= 0) {
    return(empty_result("혼잡도 상한은 0보다 큰 숫자입니다."))
  }
  candidates <- score_trips(route_trips(origin_id, dest_id, day_type, trips), congestion)
  if (nrow(candidates) == 0) {
    return(empty_result("이 구간의 샘플 시각표가 없습니다."))
  }
  candidates$verdict[is.na(candidates$path_max)] <- "탈락: 혼잡도 없음"
  over_cap <- !is.na(candidates$path_max) & candidates$path_max > congestion_cap
  candidates$verdict[over_cap] <- paste0("탈락: 혼잡도 상한 ", congestion_cap, "% 초과")
  kept <- candidates[candidates$verdict == "후보", , drop = FALSE]
  latest_any <- pick_latest(candidates)
  result <- result_from_pick(
    candidates,
    pick_latest(kept),
    sprintf(
      "혼잡도 %s%% 이하인 차가 없습니다. 탈 수 있는 마지막 차는 %s 출발입니다.",
      congestion_cap,
      if (is.null(latest_any)) "없음" else latest_any$depart
    )
  )
  result
}

recommend_home_roundtrip <- function(here_id, home_id, free_from, next_event, day_type, congestion_cap,
                                     trips = sample_trips, congestion = sample_congestion) {
  if (day_type == "공휴일") {
    return(holiday_result())
  }
  free_min <- time_to_minutes(free_from)
  next_min <- time_to_minutes(next_event)
  if (is.na(free_min) || is.na(next_min)) {
    return(empty_result("비는 시각과 다음 일정은 18:00, 21:00처럼 입력합니다."))
  }
  going <- score_trips(route_trips(here_id, home_id, day_type, trips), congestion)
  returning <- score_trips(route_trips(home_id, here_id, day_type, trips), congestion)
  if (nrow(going) == 0 || nrow(returning) == 0) {
    return(empty_result("갔다 오는 샘플 시각표가 없습니다. 사당과 강남 구간으로 보면 됩니다."))
  }
  go_min <- vapply(going$depart, time_to_minutes, numeric(1))
  go_arrive <- vapply(going$arrive, time_to_minutes, numeric(1))
  back_depart <- vapply(returning$depart, time_to_minutes, numeric(1))
  back_arrive <- vapply(returning$arrive, time_to_minutes, numeric(1))
  pairs <- list()
  for (i in seq_len(nrow(going))) {
    for (j in seq_len(nrow(returning))) {
      quiet <- !is.na(going$path_max[[i]]) && !is.na(returning$path_max[[j]]) &&
        going$path_max[[i]] <= congestion_cap && returning$path_max[[j]] <= congestion_cap
      fits <- go_min[[i]] >= free_min && back_depart[[j]] >= go_arrive[[i]] && back_arrive[[j]] <= next_min
      if (quiet && fits) {
        pairs[[length(pairs) + 1]] <- list(i = i, j = j, worst = max(going$path_max[[i]], returning$path_max[[j]]))
      }
    }
  }
  congestion_label <- function(value) ifelse(is.na(value), "없음", paste0(value, "%"))
  leg_verdict <- function(frame) {
    verdict <- ifelse(is.na(frame$path_max), "탈락: 혼잡도 없음",
      ifelse(frame$path_max > congestion_cap,
        paste0("탈락: 혼잡도 상한 ", congestion_cap, "% 초과"),
        "탈락: 왕복 시간에 안 맞음"))
    verdict
  }
  going_verdict <- leg_verdict(going)
  returning_verdict <- leg_verdict(returning)
  if (length(pairs) > 0) {
    for (pair in pairs) {
      going_verdict[[pair$i]] <- "후보"
      returning_verdict[[pair$j]] <- "후보"
    }
  }
  best <- NULL
  if (length(pairs) > 0) {
    worst <- vapply(pairs, function(pair) pair$worst, numeric(1))
    best <- pairs[[which.min(worst)]]
    going_verdict[[best$i]] <- "추천"
    returning_verdict[[best$j]] <- "추천"
  }
  table <- rbind(
    data.frame(방향 = "가는 편", 출발 = going$depart, 도착 = going$arrive, 혼잡도 = congestion_label(going$path_max), 판정 = going_verdict, stringsAsFactors = FALSE),
    data.frame(방향 = "오는 편", 출발 = returning$depart, 도착 = returning$arrive, 혼잡도 = congestion_label(returning$path_max), 판정 = returning_verdict, stringsAsFactors = FALSE)
  )
  if (is.null(best)) {
    return(list(message = "갔다 오지 않습니다. 왕복이 다음 일정 안에 들어가고 두 편 모두 혼잡도 상한 이하인 조합이 없습니다.", table = table))
  }
  list(
    message = sprintf(
      "%s에 나가 %s에 집에 도착하고, %s에 다시 출발해 %s에 돌아옵니다. 두 편의 혼잡도는 %s%%, %s%%입니다.",
      going$depart[[best$i]],
      going$arrive[[best$i]],
      returning$depart[[best$j]],
      returning$arrive[[best$j]],
      going$path_max[[best$i]],
      returning$path_max[[best$j]]
    ),
    table = table
  )
}

recommend_meet <- function(origin_a, origin_b, meet_id, meet_at, day_type,
                           trips = sample_trips, congestion = sample_congestion) {
  person_a <- recommend_on_time(origin_a, meet_id, meet_at, day_type, trips, congestion)
  person_b <- recommend_on_time(origin_b, meet_id, meet_at, day_type, trips, congestion)
  name_a <- sample_stations$station_name[match(origin_a, sample_stations$station_id)]
  name_b <- sample_stations$station_name[match(origin_b, sample_stations$station_id)]
  list(
    message = paste0("A (", name_a, ") ", person_a$message, "  B (", name_b, ") ", person_b$message),
    table = rbind(
      if (is.null(person_a$table)) NULL else cbind(사람 = paste0("A ", name_a), person_a$table),
      if (is.null(person_b$table)) NULL else cbind(사람 = paste0("B ", name_b), person_b$table)
    )
  )
}

recommend_after_delay <- function(origin_id, dest_id, arrive_by, day_type, alert_ends,
                                  trips = sample_trips, congestion = sample_congestion) {
  if (!nzchar(trimws(alert_ends))) {
    return(recommend_on_time(origin_id, dest_id, arrive_by, day_type, trips, congestion))
  }
  alert_min <- time_to_minutes(alert_ends)
  if (is.na(alert_min)) {
    return(empty_result("알림 종료 시각은 08:30처럼 입력합니다."))
  }
  if (day_type == "공휴일") {
    return(holiday_result())
  }
  arrive_limit <- time_to_minutes(arrive_by)
  if (is.na(arrive_limit)) {
    return(empty_result("도착 시각은 09:30처럼 입력합니다."))
  }
  candidates <- score_trips(route_trips(origin_id, dest_id, day_type, trips), congestion)
  if (nrow(candidates) == 0) {
    return(empty_result("이 구간의 샘플 시각표가 없습니다."))
  }
  late <- vapply(candidates$arrive, time_to_minutes, numeric(1)) > arrive_limit
  during_alert <- vapply(candidates$depart, time_to_minutes, numeric(1)) < alert_min
  candidates$verdict[late] <- "탈락: 도착 시각을 넘김"
  candidates$verdict[candidates$verdict == "후보" & during_alert] <- "탈락: 지연 종료 전"
  candidates$verdict[is.na(candidates$path_max) & candidates$verdict == "후보"] <- "탈락: 혼잡도 없음"
  kept <- candidates[candidates$verdict == "후보", , drop = FALSE]
  result_from_pick(candidates, pick_lowest(kept), "지연이 끝난 뒤 제시간에 도착하는 출발이 없습니다.")
}

fetch_forecast <- function(nx, ny) {
  stop("날씨 API는 호출하지 않습니다. 강수 시각은 화면에서 직접 받습니다.")
}

fetch_alerts <- function(line) {
  stop("알림 API는 호출하지 않습니다. 종료 시각은 화면에서 직접 받습니다.")
}
