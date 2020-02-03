library(tidyverse)
library(readxl)
library(DT)
library(knitr)

setwd("C:\\Users\\bcurry\\OneDrive - FREEDOM PAY, INC\\Online Desktop")

dog_time <- read_xlsx("Dog Eating - Chop.xlsx")

dog_length <- length(dog_time$Day)

dog_time$UCL <- mean(dog_time$Sample_Average) + 1.880 * mean(dog_time$Range)
dog_time$LCL <- mean(dog_time$Sample_Average) - 1.880 * mean(dog_time$Range)
dog_time$Centerline <- mean(dog_time$Sample_Average)

dog_grouped <- dog_time[c(1, 4, 5, 6, 7, 8)] %>%
  gather(key = "Type", value = "Value", -Day)
  
dog_grouped$Type <- factor(dog_grouped$Type, levels = c("Sample_Average", "Centerline", "UCL", "LCL", "Range"))
 

chart1 <- dog_grouped %>%
  filter(Type == "Centerline") %>%
  ggplot(aes(x = Day, y = Value), color = "steelblue") +
  geom_line(linetype = "dotted", color = "steelblue") +
  labs(x = "Day", y = "Time to Finish Bowl of Food (Secs)", title = "Centerline for Finishing Food", color = NULL, linetype = NULL) +
  theme_classic()

chart2 <- dog_grouped %>%
  filter(Type == "Centerline" | Type == "UCL" | Type == "LCL") %>%
  ggplot(aes(x = Day, y = Value), color = "steelblue") +
  geom_line(aes(linetype = Type), color = "steelblue") +
  labs(x = "Day", y = "Time to Finish Bowl of Food (Secs)", title = "Centerline, UCL, and LCL for Finishing Food", color = NULL, linetype = NULL) +
  theme_classic() +
  scale_linetype_manual(values = c("dotted", "dashed", "dashed"))

chart3 <- dog_grouped %>%
  filter(Type != "Range") %>%
  ggplot(aes(x = Day, y = Value, color = Type)) +
  geom_line(aes(linetype = Type)) +
  labs(x = "Day", y = "Time to Finish Bowl of Food (Secs)", title = "XBar Chart to Finish Food", color = NULL, linetype = NULL) +
  theme_classic() +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "dashed")) +
  scale_color_manual(values = c("navyblue", "steelblue", "steelblue", "steelblue"))


dog_ranges <- dog_time[c(1, 5)]

Centerline <- mean(dog_ranges$Range)

dog_ranges$UCL <- Centerline*3.267
dog_ranges$Centerline <- Centerline

dog_ranges <- dog_ranges %>%
  gather(key = "Type", value = "Value", -Day)

dog_ranges$Type <- factor(dog_ranges$Type, levels = c("Range", "Centerline", "UCL"))

chart4 <- dog_ranges %>%
  filter(Type == "Centerline") %>%
  ggplot(aes(x = Day, y = Value), color = "steelblue") +
  geom_line(linetype = "dotted", color = "steelblue") +
  labs(x = "Day", y = "Range of Time to Finish (Secs)", title = "Average Sample Range", color = NULL, linetype = NULL) +
  theme_classic()

chart5 <- dog_ranges %>%
  filter(Type == "Centerline") %>%
  ggplot(aes(x = Day, y = Value), color = "steelblue") +
  geom_line(linetype = "dotted", color = "steelblue") +
  labs(x = "Day", y = "Range of Time to Finish (Secs)", title = "Sample Range Centerline", color = NULL, linetype = NULL) +
  theme_classic()

chart5 <- dog_ranges %>%
  filter(Type != "Range") %>%
  ggplot(aes(x = Day, y = Value), color = "steelblue") +
  geom_line(aes(linetype = Type), color = "steelblue") +
  labs(x = "Day", y = "Range of Time to Finish (Secs)", title = "Centerline and UCL", linetype = NULL, color = NULL) +
  theme_classic() +
  scale_linetype_manual(values = c("dotted", "dashed"))

chart6 <- dog_ranges %>%
  ggplot(aes(x = Day, y = Value)) +
  geom_line(aes(linetype = Type, color = Type)) +
  labs(x = "Day", y = "Range of Time to Finish (Secs)", title = "R Chart for Sample ", linetype = NULL, color = NULL) +
  theme_classic() +
  scale_linetype_manual(values = c("solid","dotted", "dashed")) +
  scale_color_manual(values = c("navyblue", "steelblue", "steelblue"))



chart1
chart2
chart3
chart4
chart5
chart6
