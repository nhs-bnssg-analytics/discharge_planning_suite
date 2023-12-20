
  funs <- imap(fit_lnorm$fit, ~function(x) dlnorm(x, meanlog = mean(.x[['meanlog']], na.rm = TRUE),
                                           sdlog = mean(.x[['sdlog']], na.rm = TRUE)))
  
  x_max <- 500
  x <- seq(0, x_max, 0.1)
  
  
  tibble(x = list(x)) %>%
    mutate(y = map(x, function(x) imap(funs, ~.x(x)))) %>%
    unnest_longer(col = c(y), indices_to = "leaf") %>%
    mutate(mean = map_dbl(leaf, ~exp(mean(fit_lnorm$fit[[.x]][['meanlog']], na.rm = TRUE) + ((mean(fit_lnorm$fit[[.x]][['sdlog']], na.rm = TRUE))^2/2)))) %>%
    mutate(median = map_dbl(leaf, ~exp(mean(fit_lnorm$fit[[.x]][['meanlog']], na.rm = TRUE)))) %>%
    unnest(cols = c(x, y)) %>%
    mutate(y_min = 0,
           day = x/24,
           mean = mean/24,
           median = median/24)%>%
    # filter(leaf == !!leaf) %>%
    ggplot(aes(day)) +
    geom_vline(aes(xintercept = median, col = "Median"), linetype = 2, size = 1.1) +
    geom_vline(aes(xintercept = mean, col = "Mean"), linetype = 2, size = 1.1) +
    # geom_text(aes(x = mean, y = 0.1, label = "mean")) +
    geom_line(aes(y = y)) +
    geom_ribbon(aes(ymax = y, ymin = y_min), alpha = 0.25) +
    scale_colour_discrete(guide = guide_legend(direction = "horizontal")) +
    scale_x_continuous("LOS (days)", limits = c(0, 10), breaks = seq(0, 10, 2)) +
    facet_wrap(vars(leaf), ncol = 1) +
    labs(title = "Length of stay distribution",
         y = "",
         col = "") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          # axis.text.x = element_text(size = rel(1)),
          # axis.title.x = element_text(size = rel(1)),
          legend.position = c(0.75, 0.80),
          legend.justification = 0.5,
          legend.background = element_rect(fill="white",
                                           colour = "white",
                                           size=2, linetype="solid"),
          legend.key.size = unit(3, "line"),
          # legend.text = element_text(size = rel(1))
    )
  
