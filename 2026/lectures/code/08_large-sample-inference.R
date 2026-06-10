########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "purrr", "tidyr"))

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

################
### SETTINGS ###
################

set.seed(8)

figure_path = "2026/lectures/figures/08_wald-vs-agresti-coull.png"
wald_focus_figure_path = "2026/lectures/figures/08_wald-coverage-curry.png"
sample_sizes = c(20, 50, 100, 200, 500, 1000)
p_grid = seq(0.01, 0.99, length.out = 199)
focus_sample_sizes = c(100, 200, 500, 1000)
p_focus_grid = seq(0.80, 0.99, by = 0.001)
n_sims = 5000
z = qnorm(0.975)

##########################
### HELPER FUNCTIONS #####
##########################

wald_interval = function(successes, n, z) {
    phat = successes / n
    half_width = z * sqrt(phat * (1 - phat) / n)

    tibble(
        lower = phat - half_width,
        upper = phat + half_width
    )
}

agresti_coull_interval = function(successes, n, z) {
    n_tilde = n + z^2
    p_tilde = (successes + z^2 / 2) / n_tilde
    half_width = z * sqrt(p_tilde * (1 - p_tilde) / n_tilde)

    tibble(
        lower = p_tilde - half_width,
        upper = p_tilde + half_width
    )
}

wald_exact_coverage = function(p, n, z) {
    successes = 0:n
    intervals = wald_interval(successes, n, z)

    tibble(
        n = n,
        p = p,
        coverage = sum(dbinom(successes, size = n, prob = p) *
            (intervals$lower <= p & p <= intervals$upper))
    )
}

estimate_coverage = function(p, n, n_sims, z) {
    successes = rbinom(n_sims, size = n, prob = p)

    wald = wald_interval(successes, n, z) |>
        mutate(method = "Wald")

    agresti_coull = agresti_coull_interval(successes, n, z) |>
        mutate(method = "Agresti-Coull")

    bind_rows(wald, agresti_coull) |>
        mutate(
            contains_p = lower <= p & p <= upper,
            n = n,
            p = p
        ) |>
        group_by(method, n, p) |>
        summarise(coverage = mean(contains_p), .groups = "drop")
}

##########################
### SIMULATE COVERAGE ####
##########################

coverage_results = expand_grid(
    n = sample_sizes,
    p = p_grid
) |>
    mutate(
        coverage_data = map2(p, n, estimate_coverage, n_sims = n_sims, z = z)
    ) |>
    pull(coverage_data) |>
    bind_rows()

wald_focus_results = expand_grid(
    n = focus_sample_sizes,
    p = p_focus_grid
) |>
    mutate(
        coverage_data = map2(p, n, wald_exact_coverage, z = z)
    ) |>
    pull(coverage_data) |>
    bind_rows()

#####################
### BUILD FIGURE ####
#####################

common_theme = theme_minimal(base_size = 12) +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold")
    )

coverage_plot = ggplot(
    coverage_results,
    aes(x = p, y = coverage, color = method)
) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray45") +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ n, ncol = 3, labeller = label_both) +
    scale_color_manual(
        values = c("Wald" = "#B22222", "Agresti-Coull" = "#1F78B4")
    ) +
    scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        limits = c(0, 1)
    ) +
    scale_y_continuous(
        breaks = seq(0.75, 1.00, by = 0.05)
    ) +
    coord_cartesian(ylim = c(0.75, 1.00)) +
    labs(
        title = "Coverage of 95% Wald and Agresti-Coull Intervals",
        subtitle = "Dashed line marks nominal 95% coverage; Wald still undercovers near 0 and 1",
        x = "True free-throw probability p",
        y = "Estimated coverage probability",
        color = "Interval"
    ) +
    common_theme

wald_focus_plot = ggplot(
    wald_focus_results,
    aes(x = p, y = coverage)
) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray45") +
    geom_vline(xintercept = 0.90, linetype = "dotted", color = "gray35") +
    geom_line(linewidth = 0.9, color = "#B22222") +
    facet_wrap(~ n, ncol = 2, labeller = label_both) +
    scale_x_continuous(
        breaks = c(0.80, 0.85, 0.90, 0.95),
        limits = c(0.80, 0.99)
    ) +
    scale_y_continuous(
        breaks = seq(0.88, 0.97, by = 0.02)
    ) +
    coord_cartesian(ylim = c(0.87, 0.97)) +
    labs(
        title = "Exact Wald Coverage Near Stephen Curry's Free-Throw Probability",
        subtitle = "Dashed line marks nominal 95% coverage; dotted line marks p = 0.90",
        x = "True free-throw probability p",
        y = "Exact coverage probability"
    ) +
    common_theme +
    theme(legend.position = "none")

ggsave(
    filename = figure_path,
    plot = coverage_plot,
    width = 10,
    height = 7,
    dpi = 300
)

ggsave(
    filename = wald_focus_figure_path,
    plot = wald_focus_plot,
    width = 8.5,
    height = 7,
    dpi = 300
)
