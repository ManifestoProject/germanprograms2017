library(stringr)
library(tidyr)
library(dplyr)
library(purrr)
library(magrittr)

scales = list(
  eco = list(pos = c("per401","per402","per407","per410","per414","per505","per507", "per702"),
             neg = c("per403","per404", "per405", "per406", "per409","per412","per413","per415","per504","per506", "per701")),
  soc = list(pos = c("per601","per603","per608","per605","per704"),
             neg = c("per201","per202","per503","per602","per604","per607"))
)

add_scales = function(data, scales) {
  for (i in names(scales)) {
    scale = scales[[i]]
    data[[i]] = scale_bipolar(data, scale$pos, scale$neg)
    data = aggregate_pers(data, list() %>% { .[[paste0("sal_", i)]] = c(scale$pos, scale$neg); . }, keep = TRUE)
    data = aggregate_pers(data, list() %>% { .[[paste0("pos_", i)]] = scale$pos; . }, keep = TRUE)
    data = aggregate_pers(data, list() %>% { .[[paste0("neg_", i)]] = scale$neg; . }, keep = TRUE)
  }
  data
}

line_plot = function(data, value) {
  value = enquo(value)
  ggplot(data, aes(x = date, y = eval(rlang::`!!`(value)), group = party, color = party)) +
    geom_point(alpha = .5) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_line()
}

line_plot_matrix = function(data, ...) {
  data %>% 
    select(date, party, ...) %>%
    gather(key, value, -date, -party) %>%
    ggplot(aes(x = date, y = value, group = party, color = party)) +
      geom_point(alpha = .5) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_line() +
      facet_wrap(~key)
}

trend_2d_plot = function(data, date, x_indicator, y_indicator) {
  x_indicator = enquo(x_indicator)
  y_indicator = enquo(y_indicator)
  dates = sort(unique(data$date)) %>% { x <- .; index = detect_index(x, function(x) x == date); x[c(index, index - 1)] }
  data %>%
    filter(date %in% dates) %>%
    arrange(party, date) %>%
    ggplot(aes(x = eval(rlang::`!!`(x_indicator)), y = eval(rlang::`!!`(y_indicator)), group = party, color = party)) +
      geom_path(arrow = arrow(length = unit(0.35, "cm")), size = 2, alpha = .6) +
      geom_point(alpha = .9, size = 3) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      xlab(x_indicator) + ylab(y_indicator) +
      theme(legend.key.width = unit(1, "line"))
}

scales_plot = function(data, date) {
  data %>% 
    gather(key, value, -party, -date, -countryname) %>%
    filter(date == !!date) %>%
    mutate(scale = str_extract(key, "(?<=^(sal_|pos_|neg_){0,1})[a-zA-Z0-9]+$"),
           value_kind = case_when(
             str_detect(key, "^sal_") ~ "salience",
             str_detect(key, "^pos_") ~ "positive",
             str_detect(key, "^neg_") ~ "negative",
             !str_detect(key, "_") ~ "position"
           )) %>%
    select(-key) %>%
    spread(value_kind, value) %>%
    mutate(mid_saliency = if_else(abs(negative) > abs(positive), -1*(negative - positive)/2, (positive - negative)/2)) %>%
    ggplot(aes(x = party)) +
    geom_bar(mapping = aes(y = positive), stat = "identity") +
    geom_bar(mapping = aes(y = -1 * negative), stat = "identity") +
    geom_point(mapping = aes(y = mid_saliency), shape = 108, size = 4, color = "white") +
    geom_hline(yintercept = 0, alpha = .9, size = 1) +
    coord_flip() +
    facet_wrap(~scale, ncol = 2) +
    xlab("") + ylab("") +
    theme(legend.position = "none") +
    theme(panel.grid.major.y = element_blank()) +
    theme(strip.text.x = element_text(size = 11), axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11))
}

issues_with_previous_context_plot = function(data, date, ..., facet_col = 5, labels = c(), scales_free = "free_y") {
  date = enquo(date)
  data %>% 
    select(date, party, ...) %>%
    gather(key, value, -party, -date) %>%
    ggplot(aes(x = party, y = value)) +
      geom_point(data = function(x) { filter(x, date == !!date) }, size = 3, alpha = .5, color = "red") +
      geom_point(data = function(x) { filter(x, date != !!date) }, size = 1, alpha = .5) +
      coord_flip() +
      facet_wrap(~key, scales = scales_free, ncol = facet_col, labeller = as_labeller(labels))
}

most_important_issues = function(data, date, n = 5, labels = NULL) {
  date = enquo(date)
  data %>%
    select(party, date, matches("^per[0-9]")) %>%
    filter(date == !!date) %>%
    gather(key, value, -party, -date) %>%
    filter(!is.na(value)) %>%
    mutate_at(vars(value), funs(as.numeric)) %>%
    group_by(party, date) %>%
      top_n(n, value) %>%
      arrange(desc(value)) %>%
    ungroup() %>%
    mutate(key = str_sub(key, 4)) %>%
    { if (!is.null(labels)) { mutate(., label = do.call(recode, c(list(".x" = key), labels))) } else { . } } %>%
    select(party, key, matches("label"), value) %>%
    mutate_at(vars(value), funs(round(., 3)))
}

most_important_issue_bars = function(data, date, n = 5, labels = NULL) {
  date = enquo(date)
  issues = most_important_issues(data = data, date = !!date, n = n, labels = labels) %>%
    arrange(party) %>%
    group_by(key) %>%
    summarise(parties = paste0(party, collapse = " + ")) %>%
    ungroup()
  data %>%
    filter(date == !!date) %>%
    gather(key, value, -party, -date) %>%
    mutate(key = str_sub(key, 4)) %>%
    left_join(issues, by = "key") %>%
    filter(!is.na(parties)) %>%
    mutate(value = as.numeric(value)) %>%
    { if (!is.null(labels)) { mutate(., label = do.call(recode, c(list(".x" = key), labels))) } else { . } } %>%
    ggplot(aes(x = label, y = value, fill = party)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    coord_flip() + 
    facet_wrap(~parties, scales = "free_y", ncol = 1) +
    xlab("") + ylab("")
}

most_important_issue_bars_2parties = function(data, date, n = 5, labels = NULL, bar_alpha = 1) {
  date = enquo(date)
  issues = most_important_issues(data = data, date = !!date, n = n, labels = labels) %>%
    arrange(party) %>%
    mutate(parties = party) %>% select(key, parties)
  data = data %>%
    filter(date == !!date) %>%
    gather(key, value, -party, -date) %>%
    mutate(key = str_sub(key, 4)) %>%
    left_join(issues, by = c("key" = "key", "party" = "parties")) %>%
    mutate(parties = party) %>%
    mutate(value = as.numeric(value)) %>%
    { if (!is.null(labels)) { mutate(., label = do.call(recode, c(list(".x" = key), labels))) } else { . } } %>%
    group_by(key) %>% arrange(parties) %>% mutate(value_other = if_else(row_number() == 1, lead(value), if_else(row_number() == 2, lag(value), NA_real_))) %>% ungroup() %>%
    group_by(parties) %>% top_n(n, value) %>% ungroup() %>%
    arrange(desc(party), value) %>% mutate(`_order` = row_number())
  ggplot(data, aes(x = `_order`)) +
    geom_bar(data = function(data) { data %>% filter(party == sort(unique(party))[1]) }, aes(y = value_other), stat = "identity", position = "identity", fill = "#000000", alpha = 0.25) +
    geom_bar(data = function(data) { data %>% filter(party == sort(unique(party))[1]) }, aes(y = value), stat = "identity", position = "identity", alpha = bar_alpha, size = 0.8, fill = "#000000", color = "black") +
    geom_bar(data = function(data) { data %>% filter(party == sort(unique(party))[2]) }, aes(y = value_other), stat = "identity", position = "identity", fill = "#000000", alpha = 0.25) +
    geom_bar(data = function(data) { data %>% filter(party == sort(unique(party))[2]) }, aes(y = value), stat = "identity", position = "identity", alpha = bar_alpha, size = 0.8, fill = "#ff0000", color = "black") +
    scale_x_continuous(breaks = data$`_order`, labels = data$label) +
    coord_flip() + 
    facet_wrap(~parties, ncol = 1, scales = "free_y") +
    xlab("") + ylab("")
}

most_important_issues_with_previous_context = function(data, date, n = 5, labels = NULL) {
  date = enquo(date)
  issues = most_important_issues(data = data, date = !!date, n = n, labels = labels) %>%
    arrange(party) %>%
    .$key
  do.call(issues_with_previous_context_plot, c(list("data" = data, "date" = date), issues %>% paste0("per", .)))
}

most_important_issue_changes = function(data, date, n = 5, 
                                        fun_change = function(t1, t2) { t2 - t1 },
                                        labels = NULL) {
  dates = sort(unique(data$date)) %>% { x <- .; index = detect_index(x, function(x) x == date); x[c(index, index - 1)] }
  data %>%
    select(party, date, matches("^per[0-9]")) %>%
    filter(date %in% dates) %>%
    gather(key, value, -party, -date) %>%
    filter(!is.na(value)) %>%
    mutate_at(vars(value), funs(as.numeric)) %>%
    arrange(date) %>%
    group_by(party, key) %>%
      mutate(lagged_value = lag(value), diff = fun_change(lag(value), value)) %>%
      slice(n()) %>%
    ungroup() %>%
    group_by(party) %>%
      top_n(n, abs(diff)) %>%
      arrange(desc(abs(diff))) %>%
    ungroup() %>%
    mutate(key = str_sub(key, 4)) %>%
    mutate(direction = if_else(diff > 0, "+", "-")) %>%
    { if (!is.null(labels)) { mutate(., label = do.call(recode, c(list(".x" = key), labels))) } else { . } } %>%
    select(party, key, matches("label"), direction, diff, value, lagged_value) %>%
    mutate_at(vars(diff, value, lagged_value), funs(round(., 3)))
}

most_important_differences = function(data, date, inclusion_threshold = 5, lighthouseness_threshold = 1, 
                                      fun_lighthouseness = function(values) { (values - mean(values)) / sd(values) }, 
                                      labels = NULL) {
  data %>%
    select(party, date, matches("^per[0-9]")) %>%
    filter(date == !!date) %>%
    gather(key, value, -party, -date) %>%
    filter(!is.na(value)) %>%
    mutate_at(vars(value), funs(as.numeric)) %>%
    arrange(desc(date)) %>%
    group_by(key) %>%
      mutate(mean_value = mean(value), 
             lighthouseness = fun_lighthouseness(value)) %>%
    ungroup() %>%
    filter(value >= inclusion_threshold & lighthouseness >= lighthouseness_threshold) %>%
    arrange(desc(lighthouseness)) %>%
    mutate(key = str_sub(key, 4)) %>%
    { if (!is.null(labels)) { mutate(., label = do.call(recode, c(list(".x" = key), labels))) } else { . } } %>%
    select(party, key, matches("label"), lighthouseness, value, mean_value) %>%
    mutate_at(vars(lighthouseness, value, mean_value), funs(round(., 3)))
}

most_important_differences_within = function(data, date, inclusion_threshold = 5, lighthouseness_threshold = 1, 
                                             fun_lighthouseness = function(values) { (values - mean(values)) / sd(values) }, 
                                             labels = NULL) {
  data %>%
    select(party, date, matches("^per[0-9]")) %>%
    gather(key, value, -party, -date) %>%
    filter(!is.na(value)) %>%
    mutate_at(vars(value), funs(as.numeric)) %>%
    group_by(key, party) %>%
      mutate(mean_value = mean(value), 
             sd_value = sd(value),
             min_value = min(value), max_value = max(value), 
             rank_value = min_rank(-1 * value),
             lighthouseness = fun_lighthouseness(value)) %>%
    ungroup() %>%
    filter(date == !!date) %>%
    filter(value >= inclusion_threshold & lighthouseness >= lighthouseness_threshold) %>%
    arrange(desc(lighthouseness)) %>%
    mutate(key = str_sub(key, 4)) %>%
    { if (!is.null(labels)) { mutate(., label = do.call(recode, c(list(".x" = key), labels))) } else { . } } %>%
    select(party, key, matches("label"), lighthouseness, matches("value")) %>%
    mutate_at(vars(lighthouseness, matches("value")), funs(round(., 3)))
}

all_issues_stacked = function(data, date, labels = NULL) {
  date = enquo(date)
  data %>%
    select(party, date, matches("^per[0-9]")) %>%
    filter(date == !!date) %>%
    gather(key, value, -party, -date) %>%
    filter(!is.na(value)) %>%
    mutate_at(vars(value), funs(round(as.numeric(.), 3))) %>%
    arrange(party, desc(value)) %>%
    mutate(key = str_sub(key, 4)) %>%
    { if (!is.null(labels)) { mutate(., label = do.call(recode, c(list(".x" = key), labels))) } else { . } }
}

issues_distance_plot = function(data, issues = matches("^per[0-9]{3}$"), 
                                method = "manhattan", fun_modify_distance = function(x) { x / 2 }, 
                                only_upper = TRUE, only_base_plot = FALSE) {
  distances = dist(data %>% select(issues) %>% as.matrix(), diag = FALSE, upper = TRUE, method = method) %>% as.matrix %>% fun_modify_distance()
  if (only_upper & only_base_plot) stop("only upper is not possible for only base plot")
  entries = data %>%
    select(party, date) %>%
    mutate(party_date = paste0(party, "_", date)) %>%
    .$party_date
  rownames(distances) <- entries
  colnames(distances) <- entries
  data = distances %>% as.data.frame() %>%
    rownames_to_column("key") %>%
    mutate(party = str_extract(key, "^[^_]+"), date = str_extract(key, "[0-9]+$")) %>%
    select(-key) %>%
    gather(key, value, -party, -date) %>%
    mutate(party2 = str_extract(key, "^[^_]+"), date2 = str_extract(key, "[0-9]+$")) %>%
    select(-key)
  data %>%
    mutate(value = round(value, 0)) %>%
    { if (only_upper) { filter(., date2 >= date) } else { . } } %>%
    { plot_data = .;
      plot = ggplot(plot_data, aes(x = date, y = date2, fill = value)) +
        labs("x" = "", "y" = "", "fill" = "") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      if (only_base_plot) {
        plot
      } else {
        plot + 
          geom_tile(data = function(x) { filter(x, party == party2) } ) +
          geom_text(data = function(x) { filter(x, party == party2 & date != date2) }, mapping = aes(label = value), color = "white", size = 3.5, alpha = .7) +
          facet_wrap(~party)
      }
    }
}

issues_distance_plot_2parties = function(...) {
  issues_distance_plot(..., only_base_plot = TRUE, only_upper = FALSE) +
    geom_tile(data = function(x) { filter(x, date >= date2 & party == party2 & party == sort(unique(party))[1] & date != date2) }) +
    geom_tile(data = function(x) { filter(x, date <= date2 & party == party2 & party == sort(unique(party))[2] & date != date2) }) +
    geom_tile(data = function(x) { filter(x, party != party2 & date == date2) }, color = "white", size = 1) +
    geom_text(data = function(x) { filter(x, party != party2 & date == date2) }, mapping = aes(label = value), color = "white", size = 3, alpha = .7, fontface = "bold") +
    geom_text(data = function(x) { filter(x, date >= date2 & party == party2 & party == unique(party)[2] & date != date2) }, mapping = aes(label = value), color = "black", size = 3, alpha = 1.0, fontface = "bold") +
    geom_text(data = function(x) { filter(x, date <= date2 & party == party2 & party == unique(party)[1] & date != date2) }, mapping = aes(label = value), color = "red", size = 3, alpha = 1.0, fontface = "bold") +
    scale_fill_continuous(limits = c(0, 60))
}
