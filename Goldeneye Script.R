library(tidyverse)
library(readxl)
library(DT)
library(knitr)

setwd("C:\\Users\\bcurry\\OneDrive - FREEDOM PAY, INC\\Online Desktop")
sample <- read_xlsx("Goldeneye - CHOP.xlsx")
sample <- sample[1:3]

sample <- drop_na(sample)

grouped_sessions <- sample %>%
  group_by(Session) %>%
  summarise(Successes = sum(Win),
            Attempts = n())

grouped_sessions <- as.matrix(t(grouped_sessions))
colnames(grouped_sessions) <- NULL
datatable(grouped_sessions)

avg <- mean(sample$Win)

sample_grouped <- sample %>%
  group_by(Session) %>%
  summarise(`Mission Success %` = mean(Win))

sample_count <- sample %>%
  group_by(Session) %>%
  summarise(`Mission Attempts` = n())

sample_grouped <- left_join(sample_grouped, sample_count)

sample_length <- length(sample_grouped$Session)

for (i in 1:sample_length) {
  sample_grouped$UCL[i] <- round(avg + 3 * sqrt(avg*(1 - avg)/sample_grouped$`Mission Attempts`[i]), 3)
  sample_grouped$LCL[i] <- round(avg - 3 * sqrt(avg*(1 - avg)/sample_grouped$`Mission Attempts`[i]), 3)
  sample_grouped$Centerline[i] <- avg
}

sample_original <- sample_grouped[1:5]
sample_original$`Mission Success %` <- round(sample_original$`Mission Success %`, 3)

colnames(sample_original) <- c("Session", "Mission Success Rate", "Mission Attempts", "UCL", "LCL")

sample_grouped$UCL <- ifelse(sample_grouped$UCL < 1, sample_grouped$UCL, 1)
sample_grouped$LCL <- ifelse(sample_grouped$LCL > 0, sample_grouped$LCL, 0)

sample_long <- sample_grouped[c(1, 2, 4, 5, 6)] %>%
  gather(key = "Type", value = "Value", -Session)

sample_long$Type <- factor(sample_long$Type, levels = c("Mission Success %", "Centerline", "UCL", "LCL"))

sample_long <- left_join(sample_long, sample_grouped[1:2])

chart1 <- sample_long %>%
  filter(Type == "Centerline") %>%
  ggplot(aes(x = Session, y = Value), color = "steelblue") +
  geom_line(linetype = "dotted", color = "steelblue") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  labs(x = "Session", y = "Session Win Percentage", fill = NULL, title = "Centerline for Mission Success") +
  theme_classic()

chart2 <- sample_long %>%
  filter(Type == "Centerline" | Type == "UCL") %>%
  ggplot(aes(x = Session, y = Value, group = Type), color = "steelblue") +
  geom_line(color = "steelblue", aes(linetype = Type)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  labs(x = "Session", y = "Session Win Percentage", linetype = NULL, title = "Centerline and UCL for Mission Success") +
  theme_classic()

chart3 <- sample_long %>%
  filter(Type == "Centerline" | Type == "UCL" | Type == "LCL") %>%
  ggplot(aes(x = Session, y = Value, group = Type), color = "steelblue") +
  geom_line(color = "steelblue", aes(linetype = Type)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_linetype_manual(values = c("dotted", "dashed", "dashed")) +
  labs(x = "Session", y = "Session Win Percentage", linetype = NULL, title = "Centerline, LCL and UCL for Mission Success") +
  theme_classic()

chart4 <- ggplot(sample_long, aes(x = Session, y = Value, color = Type)) +
  geom_line(aes(linetype = Type)) +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  scale_color_manual(values = c("navyblue", "steelblue", "steelblue", "steelblue")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "dashed")) +
  scale_y_continuous(labels = scales::percent) +
  geom_point(aes(y = `Mission Success %`), color = "navyblue") +
  labs(x = "Session", y = "Session Win Percentage", linetype = NULL,  title = "P Chart - Percentage of Successful Missions") +
  guides(color = FALSE)

kable(grouped_sessions)

chart1
chart2
chart3
chart4


