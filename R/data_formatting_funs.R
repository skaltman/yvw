#' Format choice/behavior data so that variables names and factor levels are
#' consistent across all three experiments.
format_data <- function(data) {
  data %>%
    rename_all(funs(str_replace(., "1st", "first"))) %>%
    rename_all(funs(str_replace_all(., " ", "_"))) %>%
    rename(exclude = `exclude?`) %>%
    mutate(condition = case_when(condition == "wrong action" ~ "Broken Button",
                                 condition == "broken toy" ~ "Broken Toy",
                                 condition == "person" ~ "Broken Button",
                                 condition == "toy" ~ "Broken Toy"),
           condition = fct_relevel(condition, "Broken Toy", "Broken Button"),
           #condition = fct_rev(condition),
           HelpfulCategory = str_to_title(HelpfulCategory) %>%
             str_trim(side = "both"),
           firstBehaviorCode = str_to_lower(firstBehaviorCode),
           firstChoice = case_when(firstChoice == "confed" ~ "Confederate's toy",
                                   firstChoice == "other" ~ "Other toy",
                                   firstChoice == "tray" ~ "Other toy",
                                   firstChoice == "toy" ~ "Confederate's toy"),
           firstChoiceNum = as.integer(firstChoice == "Confederate's toy"),
           flip = str_detect(firstBehaviorCode, "flip"),
           age = as.double(age))
}

format_sr_data <- function(data, original_data) {
  data %>%
    filter_at(vars(orient_body, orient_toy, gaze, point, verbal),
              all_vars(!is.na(.))) %>%
    left_join(original_data %>% select(videoName, condition, age),
              by = "videoName") %>%
    group_by(condition) %>%
    mutate(condition_count = n()) %>%
    mutate_at(vars(verbal, gaze, point, contains("orient")), funs(as.numeric)) %>%
    ungroup() %>%
    gather(key = "behavior",
           value = "demonstrates",
           verbal,
           gaze,
           point,
           orient_body,
           orient_toy) %>%
    select(videoName,
           age,
           condition,
           behavior,
           demonstrates,
           condition_count) %>%
    group_by(condition, condition_count, behavior) %>%
    summarise(num_demonstrates = sum(demonstrates)) %>%
    ungroup() %>%
    mutate(percentage = num_demonstrates / condition_count)
}
