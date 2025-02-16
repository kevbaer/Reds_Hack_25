library(DALEXtra)

ggplot_imp <- function(...) {
  {
    obj <- list(...)
    metric_name <- attr(obj[[1]], "loss_name")
    metric_lab <- paste(
      metric_name,
      "after permutations\n(higher indicates more important)"
    )

    full_vip <- bind_rows(obj) %>%
      filter(variable != "_baseline_")

    perm_vals <- full_vip %>%
      filter(variable == "_full_model_") %>%
      group_by(label) %>%
      summarise(dropout_loss = mean(dropout_loss))

    p <- full_vip %>%
      filter(variable != "_full_model_") %>%
      mutate(variable = fct_reorder(variable, dropout_loss)) %>%
      filter(variable %in% c(
        "PENULT", "birthYear", "penult_double", "penult_est_ra",
        "penult_launch_speed", "penult_K", "penult_hr",
        "penult_triple", "antepenult_est_ra", "weight"
      )) |>
      ggplot(aes(dropout_loss, variable))

    if (length(obj) > 1) {
      p <- p +
        facet_wrap(vars(label)) +
        geom_vline(
          data = perm_vals, aes(xintercept = dropout_loss, color = label),
          linewidth = 1.4, lty = 2, alpha = 0.7
        ) +
        geom_boxplot(aes(color = label, fill = label), alpha = 0.2)
    } else {
      p <- p +
        geom_vline(
          data = perm_vals, aes(xintercept = dropout_loss),
          linewidth = 1.4, lty = 2, alpha = 0.7
        ) +
        # geom_boxplot(fill = "#91CBD765", alpha = 0.4)
        geom_boxplot(fill = "#B80A50", alpha = 0.4)
    }
    p +
      theme(legend.position = "none") +
      labs(
        x = metric_lab,
        y = NULL, fill = NULL, color = NULL
      )
  } +
    theme_bw() +
    theme(axis.text.y = element_text(size = 13))
}

Hitter_explainer_xgb <-
  explain_tidymodels(
    hitter_mod,
    data = hitter_train |> select(-ACTUAL_TIME),
    y = hitter_train$ACTUAL_TIME,
    label = "xgb",
    verbose = FALSE
  ) |> model_parts()

Hitter_Explainer <- ggplot_imp(Hitter_explainer_xgb) +
  ggtitle("Hitter Variable Importance Plot (Top 10 Features)") +
  theme(plot.title = element_text(size = 22))
Hitter_Explainer
