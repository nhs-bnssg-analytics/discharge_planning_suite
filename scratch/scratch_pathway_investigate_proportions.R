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




bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  filter(site != "system") %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, pathway)
  ) %>%
  mutate(
    sim_prop = simulated/sum(simulated),
    obs_prop = observed/sum(observed),
    .by = c(site, id)) %>%
  summarise(sim_prop = mean(sim_prop), obs_prop = mean(obs_prop), .by = c(site, pathway)) %>%
  mutate(ratio = obs_prop/sim_prop) %>%
  ggplot(aes(x = pathway, y = ratio)) +
  geom_col() +
  geom_hline(aes(yintercept = 1)) +
  facet_wrap(vars(site))
  
