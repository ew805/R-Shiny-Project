source("Rutils.R")

#### The first chunk of code runs the simulation
## Maybe we should save these for now?

new_users <- ramp(365 * 3, 300, 1200)
## Create or connect to an SQLite database
conn <- dbConnect(SQLite(), "sims.sqlite")
dbExecute(conn, "DROP TABLE IF EXISTS simulation")

## "Pretrial" period simulation
for (i in c(-364:0)){
  dayta <- day_sim(users[i + 366], 60, 180, i, "pretrial", 
                   create_subscription_decision(0.4))
  dbWriteTable(conn, "sim", dayta, append = TRUE)
}

## Test period simulation
for (i in c(1:7 * 6)){
  dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "test", 
                   create_subscription_decision(0.6))
  dbWriteTable(conn, "sim", dayta, append = TRUE)
}

for (i in c(1:7 * 6)){
  dayta <- day_sim(floor(users[i] / 2), 60, 180, i, "default", 
                   create_subscription_decision(0.1))
  dbWriteTable(conn, "sim", dayta, append = TRUE)
}

## Post test period simulation
for (i in c(1:7 * 6)){
  dayta <- day_sim(users[i], 60, 180, i, "implemented_test", 
                   create_subscription_decision(0.6))
  dbWriteTable(conn, "sim", dayta, append = TRUE)
}

for (i in c(1:7 * 6)){
  dayta <- day_sim(users[i], 60, 180, i, "implemented_default", 
                   create_subscription_decision(0.1))
  dbWriteTable(conn, "sim", dayta, append = TRUE)
}

dbDisconnect(conn)

#### The second thing to do is extract data

conn <- dbConnect(SQLite(), "sims.sqlite")

## Get pre-trial data (for scoping purposes)
df_wide <- dbGetQuery(conn, daily_query, list(-365, -1))

df_wide %>%
  pivot_longer(
    cols = c(active_users, subscribers),
    names_to = "metric",
    values_to = "count"
  ) %>%
  mutate(count = coalesce(count, 0)) %>%
  mutate(day = as.Date(Sys.Date() + day)) %>%
  ggplot(aes(x = day, y = count, col = metric)) + 
  geom_line() +
  theme_bw() + 
  labs(title = "Fictional company: Number of users and subscribers in previous year") +
  xlab("Date") + 
  ylab("Number") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_bw()

df_wide %>%
  mutate(conversion = subscribers / active_users) %>%
  mutate(day = as.Date(Sys.Date() + day)) %>%
  ggplot(aes(x = day, y = conversion)) +
  geom_line() +
  theme_bw() + 
  labs(title = "Fictional company: Conversion rate") +
  xlab("Date") + 
  ylab("Number") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_bw()

dbDisconnect(conn)

#### And critically, we want to extract results for the A/B test period

## Open database connection
conn <- dbConnect(SQLite(), "sims.sqlite")
## Extract results for first 7 weeks 

query_days_given_weeks <- function(number_of_weeks) {
  days_in_week <- 7
  (number_of_weeks - 1) * days_in_week 
}

result <- dbGetQuery(conn, weekly_query, params = list(0, query_days_given_weeks(7)))

w = 2

n <- result[result$week_number == w + 52,]$active_users
x <- result[result$week_number == w + 52,]$subscribers
test_result <- prop.test(x, n)

test_result$p.value
test_result$conf.int


dbDisconnect(conn)

