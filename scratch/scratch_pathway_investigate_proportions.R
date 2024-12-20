n_rep <- 1E3

preds <- rf_fit %>%
  collect_predictions() %>%
  sample_frac(0.005)

preds %>%
  mutate(pathways = pmap(list(.pred_Other,
                            .pred_P1,
                            .pred_P2,
                            .pred_P3),
                       ~factor(sample(c("Other", "P1", "P2", "P3"),
                                      n_rep,
                                      prob = c(..1, ..2, ..3, ..4),
                                      replace = TRUE)),
                       levels = c("Other", "P1", "P2", "P3")),
         rep = list(1:n_rep))%>%
  select(pathway = pathways, rep) %>%
  unnest(cols = c(pathway, rep)) %>%
  count(pathway, rep) %>%
  summarise(mean_n = mean(n),
            u95 = quantile(n, 0.975), 
            l95 = quantile(n, 0.025),
            .by = pathway) %>%
  left_join(preds %>%
              count(pathway)) %>%
  mutate(pathway = factor(pathway, levels = c("Other", "P1", "P2", "P3"))) %>%
  ggplot(aes(x = pathway, y = n)) + 
  geom_point() +
  geom_errorbar(aes(ymin = l95, ymax = u95)) +
  facet_wrap(vars(pathway), scales = "free", nrow = 1)
