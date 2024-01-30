

p_pred <- data_dpp %>%
  # mutate(pathway = fct_relevel(pathway, names(levels), after = Inf)) %>%
  mutate(day = report_date + ddays(day+1)) %>%
  filter(ctr == "Y", source == "model_pred") %>%
  # ggplot(aes(x = fct_recode(ctr, "NCTR (with NCTR status)" = "N", "CTR (with CTR status)" = "Y"), y = n, fill = fct_rev(pathway))) +
  ggplot(aes(x = day, y = n, fill = pathway, group = pathway)) +
  geom_col_interactive(aes(tooltip = tooltip_n)) +
  geom_errorbar_interactive(aes(ymin = l95, ymax = u95, tooltip = tooltip_errorbar), width = 0.5)  +
  ggh4x::facet_grid2(site~pathway, independent = "y", scales = "free_y") +
  bnssgtheme() +
  scale_fill_bnssg(breaks = cols_curr) +
  # theme(legend.position = "bottom") +
  labs(title = "NCTR patient forecasts, per site/pathway",
       x = "",
       y = "")


p_curr <- data_dpp %>%
  mutate(pathway = fct_recode(pathway,
                              "P1 queue" = "Additional P1",
                              "P2 queue" = "Additional P2",
                              "P3 queue" = "Additional P3"
                              )) %>%
  mutate(pathway = fct_relevel(pathway, "P2 queue", "P1 queue", after = Inf)) %>%
  # remake tooltip as levels changed
  mutate(tooltip_n = glue::glue("{pathway} = {round(n, 0)}")) %>%
  filter(source == "current_ctr_data", ctr == "N") %>%
  ggplot(aes(x = site, y = round(n, 0), 
             fill = pathway, 
             group = pathway)) +
  geom_col_interactive(aes(tooltip = tooltip_n))  +
  bnssgtheme() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = cols_curr) +
  labs(title = "NCTR patients* in BNSSG system today",
       #subtitle = str_wrap(glue::glue("CTR is broken down into those who we predict will be NCTR {report_date + ddays(1)} and not", 50)),
       fill = "D2A queue",
       x = "",
       y = ""
  )


ptc <- patchwork::wrap_plots(p_curr, p_pred, widths = c(0.25, 0.75)) +
  patchwork::plot_layout(guides = "collect") &
  patchwork::plot_annotation(caption = "*Meant to include all patients with LOS over 24 hrs. Data supplied by trusts in daily flows, however we are aware of DQ issues and some patients will be missing.") &
  theme(legend.position = 'bottom',
        plot.caption = element_text(hjust = 0, size = rel(1.1)))

girafe(ggobj = ptc, 
       width_svg = 24, 
       height_svg = 12,
       options = list(
         opts_hover(css = "fill: black;"),
         opts_hover_inv(css = "opacity: 0.1;")
       ))

