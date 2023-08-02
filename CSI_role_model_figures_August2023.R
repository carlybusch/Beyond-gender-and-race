### Beyond gender and race: The representation of concealable identities among college science instructors
### Busch, Cooper, and Brownell
### August 2, 2023

### Script to generate figures in manuscript
### csv files located in GitHub repository <https://github.com/carlybusch/Beyond-gender-and-race>

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(PNWColors)

### Figure 1: Extent of revealing CSIs to undergraduates -------

fig1table <- read.csv("extent_out_table.csv")

fig1 <- fig1table %>% 
  reshape2::melt(id.vars = c("csi", "reveal_sum"), 
                 variable.name = "extent_out", value.name = "count") %>%
  mutate(extent_out = str_replace(extent_out, "reveal_none", "none")) %>%
  mutate(extent_out = str_replace(extent_out, "reveal_some", "some")) %>%
  mutate(extent_out = str_replace(extent_out, "out_total", "all*")) %>%
  mutate(extent_out=factor(extent_out, levels=c("none", "some", "all*")))%>%
  ggplot(aes(x = reorder(csi, count))) +
  geom_col(aes(y = round(count/1248, 3), fill = extent_out), position = "stack", width = 0.75) +
  scale_fill_manual(breaks = c("all*", "some", "none"),
                    values = c("none" = pnw_palette("Cascades")[6],
                               "some" = pnw_palette("Cascades")[2],
                               "all*" = pnw_palette("Cascades")[1])) +
  geom_text(aes(y = reveal_sum/1248,
                label = scales::percent(reveal_sum/1248,
                                        accuracy = .1),
                hjust = -.02,
                size = 12), color = "black",
            show.legend = F) +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1),
                     breaks = seq(0,.35, by = .05), limits = c(0, .4)) +
  scale_x_discrete(labels =  c("lowses" = "low SES growing up", 
                               "disability" = "has a disability",
                               "fgen" = "first-gen",
                               "struggle" = "struggled academically",
                               "transfer" = "CC transfer student",
                               "depress" = "depression", "lgbq" = "LGBQ+",
                               "tgnc" = "trans/nonbinary")) +
  labs(y = "Percent of all instructors", x = "", fill = "Extent of reveal\nto undergraduates") +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 14),
        axis.title = element_text(family="Helvetica", color = "black", size = 14, face = "bold"),
        legend.position = "right",
        legend.text = element_text(family="Helvetica", color = "black", size = 14),
        legend.title = element_text(family="Helvetica", color = "black", size = 14)) +
  coord_flip()

fig1

### Figure 2: Compositional and realized mismatch between instructors and undergraduates ------

fig2table <- read.csv("comparison_df.csv")

fig2a <- fig2table %>%
  mutate(identities = factor(identities, levels = c("fgen","lowses",
                                                    "transfer", "ugexp", 
                                                    "lgbq",
                                                    "anxiety","struggle", "depression", 
                                                    "cognition", "disability", 
                                                    "tgnc", "addiction"))) %>%
  ggplot(aes(y = reorder(identities, instr_count))) +
  geom_col(aes(x = instr_pct_total/100), fill = pnw_palette("Cascades")[6], width = 0.75) +
  geom_col(aes(x = instr_pct_out/100), fill = pnw_palette("Cascades")[1], width = 0.75) +
  geom_segment(aes(yend = reorder(identities, instr_count), xend = ug_pct/100, x = instr_pct_total/100),
               linewidth = 1, color = "grey25") +
  geom_segment(aes(yend = reorder(identities, instr_count), xend = instr_pct_total/100, x = instr_pct_out/100),
               linewidth = 1, color = "grey25", linetype = 2) +
  geom_point(aes(x = ug_pct/100), color = pnw_palette("Cascades")[5], size = 6) +
  labs(x = "Percent overall", y = "") +
  scale_x_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = .1),
                     limits = c(0, .65), breaks = seq(0:.6, by = .2)) +
  scale_y_discrete(labels = c("tgnc" = "trans/nonbinary",
                              "struggle" = "struggled academically",
                              "lgbq" = "LGBQ+",
                              "ugexp" = "undergrad experience",
                              "lowses" = "low SES growing up",
                              "fgen" = "first-gen",
                              "disability" = "has a disability",
                              "transfer" = "CC transfer student")) +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 14),
        axis.title = element_text(family="Helvetica", color = "black", size = 14, face = "bold"))

fig2a

### Figure 3: Reasons instructors reveal or conceal identities ----
### Figure 3a: Top five reasons to conceal ------

select_conceal <- read.csv("why_conceal.csv")

fig3a <- select_conceal %>%
  filter(pct >.392) %>%
  mutate(reason = str_replace(reason, "I typically do not share", "Typically do not share the identity in any context")) %>%
  mutate(reason = str_replace(reason, "was relevant to the course content",
                              "Perceives as not relevant to content")) %>%
  mutate(reason = str_replace(reason, "I had never thought about",
                              "Had never thought about revealing the identity")) %>%
  mutate(reason = str_replace(reason, "was relevant to the students in this course",
                              "Perceives as not relevant to students")) %>%
  mutate(reason = str_replace(reason, "in this course was inappropriate",
                              "Perceives as inappropriate")) %>%
  ggplot(aes(x = reorder(reason, pct), y = pct, fill = reason)) +
  geom_col(width = .5) +
  geom_text(aes(y = pct,
                label = scales::percent(pct,
                                        accuracy = .1)),
            hjust = -.02,
            size = 4, color = "black",
            show.legend = F) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1),
                     breaks = seq(0,.6, by = .1), limits = c(0, .65)) +
  scale_fill_manual(values = c("Typically do not share the identity in any context" = pnw_palette("Cascades")[3],
                               "Perceives as not relevant to content" = pnw_palette("Cascades")[2],
                               "Had never thought about revealing the identity" = pnw_palette("Cascades")[3],
                               "Perceives as not relevant to students" = pnw_palette("Cascades")[2],
                               "Perceives as inappropriate" = pnw_palette("Cascades")[3])) +
  labs(y = "Percent of instructors who selected\nthe reason to conceal CSI", x = "") +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 12),
        axis.title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.position = "none") +
  coord_flip()

fig3a

### Figure 3b: Categories of reasons to conceal -----

conceal_categories <- read.csv("conceal_categories.csv")

fig3b <- conceal_categories |>
  mutate(category=factor(category, levels=c("concerns.count", "norms.count", "no.benefit.count"))) |>
  ggplot() +
  geom_col(aes(x = 1, y = total/5117, fill = category), position = "stack", width = 0.75) +
  geom_col(aes(x = 2, y = option_distribution, fill = category), position = "stack", width = 0.75) +
  scale_fill_manual(breaks = c("no.benefit.count", "norms.count", "concerns.count"),
                    values = c("concerns.count" = pnw_palette("Cascades")[5],
                               "norms.count" = pnw_palette("Cascades")[3],
                               "no.benefit.count" = pnw_palette("Cascades")[2]),
                    labels = c("No student\nbenefits", "Norms", "Instructor\nconsequences")) +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1),
                     breaks = seq(0,1, by = .2), limits = c(0, 1)) +
  scale_x_discrete(limits = c("Distribution of\nreasons selected\nby participants", "Distribution of\nlisted options")) + 
  labs(y = "Percent", x = "", fill = "Category") +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 12),
        axis.title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.position = "right",
        legend.text = element_text(family = "Helvetica", color = "black", size = 10),
        legend.title = element_text(family = "Helvetica", color = "black", size = 10)) +
  coord_flip()

fig3b

### Figure 3c: Top five reasons to reveal ------

select_reveal <- read.csv("why_reveal.csv")

fig3c <- select_reveal %>%
  filter(pct >.51) %>%
  mutate(reason = str_replace(reason, "I wanted to be an example to my students",
                              "To be an example to students")) %>%
  mutate(reason = str_replace(reason, "I wanted to be known as a supporter",
                              "To be a known supporter")) %>%
  mutate(reason = str_replace(reason, "students in this course was appropriate",
                              "Perceives revealing was appropriate")) %>%
  mutate(reason = str_replace(reason, "relevant to the students in this course",
                              "Perceives as  relevant to students")) %>%
  mutate(reason = str_replace(reason, "I wanted to serve as a mentor",
                              "To serve as a mentor to students with the identity")) %>%
  ggplot(aes(x = reorder(reason, pct), y = pct, fill = reason)) +
  geom_col(width = .5) +
  geom_text(aes(y = pct,
                label = scales::percent(pct,
                                        accuracy = .1)),
            hjust = -.02,
            size = 3.5, color = "black",
            show.legend = F) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1),
                     breaks = seq(0,.8, by = .15), limits = c(0, .89)) +
  scale_fill_manual(values = c("To be an example to students" = pnw_palette("Cascades")[2],
                               "To be a known supporter" = pnw_palette("Cascades")[2],
                               "Perceives revealing was appropriate" = pnw_palette("Cascades")[3],
                               "Perceives as  relevant to students" = pnw_palette("Cascades")[2],
                               "To serve as a mentor to students with the identity" = pnw_palette("Cascades")[2])) +
  labs(y = "Percent of instructors who selected\nthe reason to reveal CSI", x = "") +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 12),
        axis.title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.position = "none") +
  coord_flip()

fig3c

### Figure 3d: Categories of reasons to reveal ----

reveal_categories <- read.csv("reveal_categories.csv")

fig3d <- reveal_categories |>
  mutate(category=factor(category, levels=c("personal.benefit.count", "norms.count", "benefit.count"))) |>
  ggplot() +
  geom_col(aes(x = 1, y = total/1344, fill = category), position = "stack", width = 0.75) +
  geom_col(aes(x = 2, y = option_distribution, fill = category), position = "stack", width = 0.75) +
  scale_fill_manual(breaks = c("benefit.count", "norms.count", "personal.benefit.count"),
                    values = c("personal.benefit.count" = pnw_palette("Cascades")[5],
                               "norms.count" = pnw_palette("Cascades")[3],
                               "benefit.count" = pnw_palette("Cascades")[2]),
                    labels = c("Student\nbenefits", "Norms", "Instructor\nbenefits")) +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1),
                     breaks = seq(0,1, by = .2), limits = c(0, 1)) +
  scale_x_discrete(limits = c("Distribution of\nreasons selected\nby participants", "Distribution of\nlisted options")) + 
  labs(y = "Percent", x = "", fill = "Category") +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 12),
        axis.title = element_text(family="Helvetica", color = "black", size = 12, face = "bold"),
        legend.position = "right",
        legend.text = element_text(family = "Helvetica", color = "black", size = 10),
        legend.title = element_text(family = "Helvetica", color = "black", size = 10),
        axis.ticks.y = element_blank()) +
  coord_flip()

fig3d
