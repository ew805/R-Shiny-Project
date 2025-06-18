library(dplyr)
library(ggplot2)
library(RSQLite)
library(tidyr)

subscribes <- function(end, paywall, p_subscribes) {
  if (paywall < end) {
    rbinom(1, 1, p_subscribes)
  } else {
    0
  }
}

create_subscription_decision <- function(p_subscribes){
  sub <- function(user_paywall, user_ends) {
    mapply(subscribes, user_ends, user_paywall, p_subscribes)
  }
  return(sub)
}


day_sim <- function(
    n, time_to_paywall, time_to_leave, today, group, sub) {
  x <- today + floor(rexp(n, 1 / time_to_paywall))
  y <- today + floor(rexp(n, 1 / time_to_leave))
  z <- ifelse(sub(x, y), x, NA)
  data.frame(user_starts = today, user_leaves = y,
             user_subscribes = z, grouping = group)
}

ramp <- function(n_days, n_start, n_end) {
  floor(seq(n_start, n_end, length.out = n_days))
}

daily_query <- "
WITH RECURSIVE all_days(day) AS (
  SELECT ?
  UNION ALL
  SELECT day + 1
  FROM all_days
  WHERE day <= ?
),
users (day, active_users, grouping) AS (
  SELECT day, COUNT(DISTINCT rowid) AS active_users, grouping
  FROM all_days
  JOIN sim
  ON day BETWEEN user_starts AND user_leaves
  GROUP BY day, grouping),
subscribers (day, subscribers, grouping) AS (
  SELECT day, COUNT(DISTINCT rowid) AS subscribers, grouping
  FROM all_days
  JOIN sim
  ON day BETWEEN user_subscribes AND user_leaves
  WHERE user_subscribes IS NOT NULL
  GROUP BY day, grouping
)
SELECT u.day, u.grouping, active_users, subscribers
FROM users u
LEFT JOIN subscribers s
ON u.day = s.day AND u.grouping = s.grouping
ORDER BY s.grouping, s.day;"

weekly_query <- "
WITH RECURSIVE all_days(day) AS (
  SELECT ?
  UNION ALL
  SELECT day + 1
  FROM all_days
  WHERE day + 1 <= ?
),
users(week_number, grouping, active_users) AS (
  SELECT CAST( (day + 365) / 7 AS INTEGER ) AS week_number, grouping, COUNT(DISTINCT rowid) AS active_users
  FROM  all_days
  JOIN sim
  ON day BETWEEN user_starts AND user_leaves
  WHERE grouping in ('default', 'test')
  GROUP BY week_number, grouping),
subscribers(week_number, grouping, subscribers) AS (
  SELECT CAST( (day + 365 ) / 7 AS INTEGER) AS week_number, grouping, COUNT(DISTINCT rowid) AS subscribers
  FROM  all_days
  JOIN sim
  ON day BETWEEN user_subscribes AND user_leaves
  WHERE grouping in ('default', 'test')
  GROUP BY week_number, grouping)
SELECT u.week_number, u.grouping, active_users, subscribers
FROM users u
LEFT JOIN subscribers s
ON u.week_number = s.week_number AND u.grouping = s.grouping
ORDER BY u.grouping, u.week_number;"


