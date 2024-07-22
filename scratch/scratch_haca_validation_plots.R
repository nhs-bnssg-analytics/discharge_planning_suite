out_drain <- readRDS("data/validation_drain_out.RDS")

(drain_plot <- out_drain %>%
  reduce(bind_rows) %>%
  filter(site == "system") %>%
  # filter(id %in% c(1, 2, 7, 9)) %>%
  filter(id %in% c(8, 9)) %>%
  pivot_longer(cols = -c(id, site, day, date, source), names_to = "metric", values_to = "value") %>%
  group_by(source, id, site, metric) %>%
  mutate(prop = value/sum(value)) %>%
  mutate(cum_prop = cumsum(prop)) %>%
  pivot_longer(cols = -c(day, site, date, source, id, metric), names_to = "calc", values_to = "value") %>%
  unite("metric", metric, calc, sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  filter(day <= 9) %>%
    mutate(source = recode(source, simulated = "Simulated", empirical = "Actual")) %>%
  ggplot(aes(x = factor(day+1), y = mean_value)) +
  geom_col(position = "dodge", aes(fill = source)) +
  # scale_x_date(labels = date_format(format = "%a"), date_breaks = "2 days") +
  # geom_line(aes(col = source)) +
  geom_errorbar(aes(ymin = l95_value, ymax = u95_value), position = "dodge") +
  facet_wrap(vars(id), scales = "free") +
  bnssgtheme() +
  # scale_fill_manual(values = unname(bnssgcols[c(3, 7)])) +
    scale_fill_manual(values = c("#8c96c6", "#88419d")) +
  labs(title = "Current inpatients",
       y = "Number of patients becoming ready for discharge",
       x = "")
)

out_accum <- readRDS("data/validation_accum_out.RDS")

(accum_plot <- bind_rows(map(out_accum, "plot_out"), .id = "id") %>%
  filter(id %in% c(7, 9), !is.na(day_end)) %>%
  mutate(source = recode(source, actual = "Actual", sim = "Simulated")) %>%
  mutate(day_end = factor(as.numeric(as.character(day_end)))) %>%
  filter(!day_end %in% -1:0) %>%
  ggplot(aes(x = day_end, y = n, fill = source)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(x = day_end, ymin = l95, ymax = u95)) +
  facet_wrap(vars(id)) +
  bnssgtheme() +
  # scale_fill_manual(values = unname(bnssgcols[c(3, 7)])) +
    scale_fill_manual(values = c("#8c96c6", "#88419d")) +
  labs(title = "New admits",
       y = "Number of patients becoming ready for discharge",
       x = "")
)

patchwork::wrap_plots(drain_plot, accum_plot, ncol = 1, axes = "collect", guides = "collect") &
  theme(legend.position = "bottom",
        strip.text = element_blank())

ggsave("validation/haca_dis_rdy_plot.png",
       last_plot(),
       height = 7.5,
       width = 7.5, 
       scale = 0.9)


(validation_plot_pathway <-
    roc_df %>%
    mutate(class = recode(class, Other = "P0")) %>%
    ggplot(aes(
      x = 1 - specificity,
      y = sensitivity,
      col = glue::glue("{class}\nAUC: {round(.estimate, 2)}")
    )) +
    geom_line() +
    ggplot2::geom_abline(lty = 3) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    theme(legend.position = "bottom") +
    labs(colour = ""))

ggsave(validation_plot_pathway,
       filename = "validation/validation_plot_pathway.png",
       bg = "white",
       width = 10,
       height = 10,
       scale = 0.45)


roc_df <- readRDS("data/roc_df.RDS")

(validation_plot_pathway <-
    roc_df %>%
    mutate(class = recode(class, Other = "P0")) %>%
    ggplot(aes(
      x = 1 - specificity,
      y = sensitivity,
      col = glue::glue("{class}\nAUC: {round(.estimate, 2)}")
    )) +
    geom_line() +
    ggplot2::geom_abline(lty = 3) +
    ggplot2::coord_equal() +
    bnssgtheme() +
    scale_colour_brewer(palette = "Set1") +
    theme(legend.position = "right", legend.justification = "center") +
    labs(colour = "") +
  guides(col=guide_legend(ncol=1,byrow=TRUE))
  )

ggsave(validation_plot_pathway,
       filename = "validation/validation_plot_pathway_haca.png",
       bg = "white",
       width = 10,
       height = 7.5,
       scale = 0.45)


out_pathway_model <- readRDS("data/validation_pathway_model_out.RDS")

(pathway_model_plot <- out_pathway_model %>%
  bind_rows(.id = "id") %>%
  filter(id %in% c(7, 9)) %>%
  mutate(pathway = recode(pathway, Other = "P0")) %>%
  select(id, pathway, n, mean, u95, l95) %>%
  pivot_longer(cols = -c(id, pathway, u95, l95)) %>%
  mutate(name = recode(name, n = "Actual", mean = "Simulated")) %>%
  ggplot(aes(x = pathway, y = value, fill = name)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = l95, ymax = u95)) +
  facet_wrap(vars(id)) +
  bnssgtheme() +
  # scale_fill_manual(values = unname(bnssgcols[c(3, 7)])) +
  scale_fill_manual(values = c("#8c96c6", "#88419d")) +
  labs(y = "Number of patients requiring pathway",
       x = "D2A pathway") +
  theme(legend.position = "bottom",
        strip.text = element_blank(),
        legend.justification = "center"))


ggsave("validation/haca_dis_rdy_plot.png",
       pathway_model_plot,
       height = 7.5,
       width = 7.5, 
       scale = 0.9)

