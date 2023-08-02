### Beyond gender and race: The representation of concealable identities among college science instructors
### Busch, Cooper, and Brownell
### August 2, 2023

### Note: In order to ensure participants' anonymity we have provided the data in long form, where each row is a particular identity, rather than wide form (where each row is a participant) so that the combinations of CSIs held by a given individual are not discernible and no CSIs are linked to gender, race, or age.

### The following script was used to generate tables, auxillary data files, and data analysis.

library(tidyverse)

### Demographic distribution of instructors -----

demo_data <- read.csv("general_demographic_data.csv")

demo_fxn <- function(x){
  tmp <- data.frame(demo_data[[x]])
  demo <- merge(data.frame(table(tmp)), 
                data.frame(round((table(tmp)/sum(table(tmp)))*100, 2)), 
                "demo_data..x..")
  colnames(demo)<-c("demo","count","perc")
  demo$`Percent (n)` <- paste0(demo$perc, " (", demo$count, ")")
  demo <- arrange(demo, desc(count))
  demo$demo <- stringr::str_to_title(demo$demo)
  demo$name <- x
  return(demo)
}

general_demos <- do.call(rbind, lapply(c("race", "gender", "age"),
                                    demo_fxn))

# blanks included in decline to state

### Finding 1: Instructors who report CSIs and extent out -----

extent_out_table <- read.csv("beyond_gender_race_extent_out_data.csv")

extent_out_table$out_total <- extent_out_table$reveal_all + (extent_out_table$total_count - extent_out_table$concealable_count)
extent_out_table$reveal_sum <-  rowSums(extent_out_table[,c("out_total", "reveal_some", "reveal_none")])

extent_out_table <- extent_out_table |> dplyr::select(csi, reveal_sum, out_total, reveal_some, reveal_none)

head(extent_out_table)

# output used for Fig 1

### Finding 2: Calculating the compositional and realized mismatches ---- 

comparison_df <- read.csv("beyond_gender_race_comparison_data.csv")

comparison_df$compmismatch <- comparison_df$instr_pct_total - comparison_df$ug_pct
#Realized mismatch percent difference
comparison_df$realmismatch <- comparison_df$instr_pct_out - comparison_df$ug_pct
#Compositional mismatch magnitude difference
comparison_df$comp_mult <- (comparison_df$ug_pct)/(comparison_df$instr_pct_total)
#Realized mismatch magnitude difference
comparison_df$real_mult <- (comparison_df$ug_pct)/(comparison_df$instr_pct_out)

# output used for Fig 2

### Finding 3: Reasons to 
### Finding 3: Reasons to conceal or reveal CSIs -----

# Calculating aggregate counts and pct for reasons to conceal

conceal_count_df <- read.csv("reason_conceal_by_csi.csv")

aggregate_select_conceal <- conceal_count_df |>
  group_by(reason) |>
  summarise(total_select=sum(count, na.rm = T)) |>
  arrange(desc(total_select))

aggregate_select_conceal$saw_question <- sum(conceal_count_df[conceal_count_df$reason == "would waste class time",]$saw_question)

aggregate_select_conceal$pct <- aggregate_select_conceal$total_select/aggregate_select_conceal$saw_question

# output used for Fig 3b

# Calculating aggregate counts and pct for reasons to reveal

reveal_count_df <- read.csv("reason_reveal_by_csi.csv")

aggregate_select_reveal <- reveal_count_df |>
  group_by(reason) |>
  summarise(total_select=sum(count, na.rm = T)) |>
  arrange(desc(total_select))

aggregate_select_reveal$saw_question <- sum(reveal_count_df[reveal_count_df$reason == "I typically share",]$saw_question)

aggregate_select_reveal$pct <- aggregate_select_reveal$total_select/aggregate_select_reveal$saw_question

# output used for Fig 3d

