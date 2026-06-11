########################
### INSTALL PACKAGES ###
########################

# install.packages(c("ggplot2"))

library(ggplot2)
library(grid)

################
### SETTINGS ###
################

population_figure_path = "2026/lectures/figures/09_population-vs-bootstrap.png"
observation_figure_path = "2026/lectures/figures/09_observation-bootstrap.png"
cluster_figure_path = "2026/lectures/figures/09_cluster-bootstrap.png"
parametric_figure_path = "2026/lectures/figures/09_parametric-bootstrap.png"
residual_figure_path = "2026/lectures/figures/09_residual-bootstrap.png"

paper = "#FBF8F1"
ink = "#1F2933"
muted = "#5B6770"
navy = "#274C77"
blue = "#4C78A8"
sky = "#A8DADC"
coral = "#D97B66"
gold = "#E0A458"
sage = "#8AA899"
rose = "#C06C84"
panel_border = "#D8D0C2"

arrow_style = arrow(length = unit(0.1, "inches"), type = "closed")

theme_boot = theme_void(base_size = 13) +
    theme(
        plot.background = element_rect(fill = paper, color = NA),
        panel.background = element_rect(fill = paper, color = NA),
        panel.border = element_rect(color = panel_border, fill = NA, linewidth = 0.8),
        strip.background = element_rect(fill = paper, color = NA),
        strip.text = element_text(face = "bold", size = 15, margin = margin(b = 6)),
        plot.title = element_text(face = "bold", color = ink, size = 15),
        plot.subtitle = element_text(color = muted, size = 11.5),
        plot.caption = element_text(color = muted, size = 10.5, hjust = 0.5),
        plot.margin = margin(12, 12, 12, 12)
    )

curve_df = function(panel, base_y, height, skew = 0) {
    x = seq(1, 9, length.out = 300)
    z = seq(-3, 3, length.out = 300)
    density = dnorm(z) * (1 + skew * pmax(z, 0))
    density = density / max(density)

    data.frame(
        panel = panel,
        x = x,
        y = base_y + height * density
    )
}

save_plot = function(plot, filename, width, height) {
    ggsave(
        filename = filename,
        plot = plot,
        width = width,
        height = height,
        dpi = 300
    )
}

###############################
### POPULATION VS BOOTSTRAP ###
###############################

panel_subtitles = data.frame(
    panel = c("What We Wish We Could Do", "What We Do Instead"),
    x = 5,
    y = 9.3,
    label = c(
        "Repeatedly sample new seasons from the true population",
        "Treat the observed dataset as a stand-in population"
    ),
    color = c(muted, muted)
)

panel_levels = c("What We Wish We Could Do", "What We Do Instead")

panel_subtitles$panel = factor(panel_subtitles$panel, levels = panel_levels)

top_boxes = data.frame(
    panel = c("What We Wish We Could Do", "What We Do Instead"),
    xmin = c(1.2, 1.2),
    xmax = c(8.8, 8.8),
    ymin = c(7.6, 7.6),
    ymax = c(8.7, 8.7),
    fill = c(sky, "#F7D8D1"),
    color = c(navy, coral),
    label = c(
        "Unknown football population\n(all possible seasons we could have observed)",
        "Observed dataset D\n(one season's play-by-play sample)"
    )
)
top_boxes$panel = factor(top_boxes$panel, levels = panel_levels)

mid_boxes = rbind(
    data.frame(
        panel = "What We Wish We Could Do",
        xmin = c(1.0, 4.0, 7.0),
        xmax = c(3.0, 6.0, 9.0),
        ymin = 5.1,
        ymax = 6.4,
        fill = "#E8F0FA",
        color = blue,
        label = c("Season 1", "Season 2", "Season B")
    ),
    data.frame(
        panel = "What We Do Instead",
        xmin = c(1.0, 4.0, 7.0),
        xmax = c(3.0, 6.0, 9.0),
        ymin = 5.1,
        ymax = 6.4,
        fill = "#FCEBE6",
        color = coral,
        label = c("D* (1)", "D* (2)", "D* (B)")
    )
)
mid_boxes$panel = factor(mid_boxes$panel, levels = panel_levels)

arrow_df = rbind(
    data.frame(
        panel = "What We Wish We Could Do",
        x = c(5, 5, 5, 2, 5, 8),
        y = c(7.6, 7.6, 7.6, 5.1, 5.1, 5.1),
        xend = c(2, 5, 8, 2, 5, 8),
        yend = c(6.45, 6.45, 6.45, 4.1, 4.1, 4.1),
        color = navy
    ),
    data.frame(
        panel = "What We Do Instead",
        x = c(5, 5, 5, 2, 5, 8),
        y = c(7.6, 7.6, 7.6, 5.1, 5.1, 5.1),
        xend = c(2, 5, 8, 2, 5, 8),
        yend = c(6.45, 6.45, 6.45, 4.1, 4.1, 4.1),
        color = coral
    )
)
arrow_df$panel = factor(arrow_df$panel, levels = panel_levels)

stat_points = rbind(
    data.frame(
        panel = "What We Wish We Could Do",
        x = c(2, 5, 8),
        y = 3.75,
        label = c("T1", "T2", "TB")
    ),
    data.frame(
        panel = "What We Do Instead",
        x = c(2, 5, 8),
        y = 3.75,
        label = c("T*1", "T*2", "T*B")
    )
)
stat_points$panel = factor(stat_points$panel, levels = panel_levels)

left_curve = curve_df("What We Wish We Could Do", base_y = 0.7, height = 1.25, skew = 0.05)
right_curve = curve_df("What We Do Instead", base_y = 0.7, height = 1.35, skew = 0.35)
left_curve$panel = factor(left_curve$panel, levels = panel_levels)
right_curve$panel = factor(right_curve$panel, levels = panel_levels)

curve_markers = data.frame(
    panel = "What We Do Instead",
    x = c(4.45, 5.1, 3.85, 6.2),
    xend = c(4.45, 5.1, 3.85, 6.2),
    y = c(0.7, 0.7, 0.7, 0.7),
    yend = c(2.2, 2.0, 1.45, 1.55),
    linetype = c("solid", "solid", "dashed", "dashed"),
    color = c(gold, ink, muted, muted),
    linewidth = c(1.1, 1.0, 0.8, 0.8),
    label = c("", "", "2.5%", "97.5%"),
    text_y = c(2.35, 2.15, 1.6, 1.7),
    hjust = c(0.5, 0.55, 1, 0)
)
curve_markers$panel = factor(curve_markers$panel, levels = panel_levels)

general_text = data.frame(
    panel = c("What We Wish We Could Do", "What We Do Instead", "What We Do Instead"),
    x = c(5, 5, 5),
    y = c(4.7, 4.7, 0.35),
    label = c(
        "Recompute the same statistic for each sampled dataset",
        "T can be a mean, median, quantile,\nprediction, coefficient, or curve contrast",
        "Use the bootstrap distribution to estimate center,\nspread, and confidence limits."
    ),
    color = muted,
    size = c(3.8, 3.6, 3.5)
)
general_text$panel = factor(general_text$panel, levels = panel_levels)

population_seed = data.frame(
    panel = factor(panel_levels, levels = panel_levels),
    x = 0,
    y = 0
)

population_plot = ggplot(population_seed) +
    geom_blank(aes(x = x, y = y)) +
    facet_wrap(~ panel, nrow = 1) +
    geom_text(
        data = panel_subtitles,
        aes(x = x, y = y, label = label),
        color = muted,
        size = 4.3
    ) +
    geom_rect(
        data = top_boxes,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, color = color),
        linewidth = 0.8,
        show.legend = FALSE
    ) +
    geom_text(
        data = top_boxes,
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
        color = ink,
        size = 4.2,
        lineheight = 1.0
    ) +
    geom_rect(
        data = mid_boxes,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, color = color),
        linewidth = 0.8,
        show.legend = FALSE
    ) +
    geom_text(
        data = mid_boxes,
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
        color = ink,
        size = 4.1
    ) +
    geom_segment(
        data = arrow_df,
        aes(x = x, y = y, xend = xend, yend = yend, color = color),
        linewidth = 0.8,
        arrow = arrow_style,
        show.legend = FALSE
    ) +
    geom_point(
        data = stat_points,
        aes(x = x, y = y),
        shape = 21,
        size = 8,
        stroke = 0.8,
        fill = gold,
        color = ink
    ) +
    geom_text(
        data = stat_points,
        aes(x = x, y = y, label = label),
        color = ink,
        size = 3.2,
        fontface = "bold"
    ) +
    geom_area(
        data = left_curve,
        aes(x = x, y = y),
        fill = sky,
        alpha = 0.8,
        inherit.aes = FALSE
    ) +
    geom_line(
        data = left_curve,
        aes(x = x, y = y),
        color = navy,
        linewidth = 1,
        inherit.aes = FALSE
    ) +
    geom_area(
        data = right_curve,
        aes(x = x, y = y),
        fill = "#F7D8D1",
        alpha = 0.8,
        inherit.aes = FALSE
    ) +
    geom_line(
        data = right_curve,
        aes(x = x, y = y),
        color = coral,
        linewidth = 1,
        inherit.aes = FALSE
    ) +
    geom_text(
        data = data.frame(
            panel = factor("What We Wish We Could Do", levels = panel_levels),
            x = 5,
            y = 2.35,
            label = "Sampling distribution of T(D)"
        ),
        aes(x = x, y = y, label = label),
        color = navy,
        fontface = "bold",
        size = 4.2
    ) +
    geom_text(
        data = data.frame(
            panel = factor("What We Do Instead", levels = panel_levels),
            x = 5,
            y = 2.05,
            label = "Bootstrap distribution of T*(D*)"
        ),
        aes(x = x, y = y, label = label),
        color = coral,
        fontface = "bold",
        size = 4.2
    ) +
    geom_segment(
        data = curve_markers,
        aes(x = x, y = y, xend = xend, yend = yend, color = color, linetype = linetype, linewidth = linewidth),
        show.legend = FALSE
    ) +
    scale_linetype_identity() +
    scale_linewidth_identity() +
    geom_text(
        data = curve_markers,
        aes(x = x, y = text_y, label = label, color = color, hjust = hjust),
        size = 3.6,
        show.legend = FALSE
    ) +
    geom_text(
        data = general_text,
        aes(x = x, y = y, label = label, color = color, size = size),
        show.legend = FALSE,
        lineheight = 1.0
    ) +
    scale_size_identity() +
    scale_fill_identity() +
    scale_color_identity() +
    coord_cartesian(xlim = c(0.5, 9.5), ylim = c(0.2, 9.8), clip = "off") +
    theme_boot

save_plot(population_plot, population_figure_path, width = 12, height = 6.8)

##########################
### OBSERVATION BOOTSTRAP
##########################

obs_rows = data.frame(
    xmin = c(0.9, 3.0, 5.1, 7.2),
    xmax = c(2.3, 4.4, 6.5, 8.6),
    ymin = 6.0,
    ymax = 7.2,
    label = c("row 1", "row 2", "row 3", "row 4")
)

obs_resample = data.frame(
    xmin = c(5.9, 7.4, 5.9, 7.4),
    xmax = c(7.0, 8.5, 7.0, 8.5),
    ymin = c(2.8, 2.8, 1.4, 1.4),
    ymax = c(4.0, 4.0, 2.6, 2.6),
    label = c("2", "4", "2", "1")
)

observation_plot = ggplot() +
    geom_rect(
        data = obs_rows,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "#E8F0FA",
        color = blue,
        linewidth = 0.8
    ) +
    geom_text(
        data = obs_rows,
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
        color = ink,
        size = 4.1
    ) +
    geom_segment(
        aes(x = 4.8, y = 5.4, xend = 5.75, yend = 4.2),
        color = blue,
        linewidth = 0.9,
        arrow = arrow_style
    ) +
    geom_rect(
        data = obs_resample,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "#CFE1F4",
        color = blue,
        linewidth = 0.8
    ) +
    geom_text(
        data = obs_resample,
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
        color = ink,
        size = 4.2
    ) +
    annotate("text", x = 2.7, y = 5.0, label = "Observed rows", color = muted, size = 4.4) +
    annotate("text", x = 7.15, y = 0.8, label = "Bootstrap sample with repeats", color = muted, size = 4.2) +
    coord_cartesian(xlim = c(0.3, 9.2), ylim = c(0.3, 8.4), clip = "off") +
    theme_boot

save_plot(observation_plot, observation_figure_path, width = 7.3, height = 4.8)

#######################
### CLUSTER BOOTSTRAP
#######################

cluster_top = data.frame(
    xmin = c(1.0, 3.4, 5.8),
    xmax = c(2.8, 5.2, 7.6),
    ymin = 5.7,
    ymax = 7.5,
    label = c("Drive A", "Drive B", "Drive C")
)

cluster_bottom = data.frame(
    xmin = c(4.9, 6.9, 8.4),
    xmax = c(6.5, 8.5, 9.0),
    ymin = 1.8,
    ymax = 3.8,
    label = c("B", "C", "B")
)

cluster_plot = ggplot() +
    geom_rect(
        data = cluster_top,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "#DDEADB",
        color = sage,
        linewidth = 0.8
    ) +
    geom_text(
        data = cluster_top,
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
        color = ink,
        size = 4.2
    ) +
    geom_segment(
        aes(x = 3.1, y = 5.2, xend = 4.7, yend = 3.9),
        color = sage,
        linewidth = 0.9,
        arrow = arrow_style
    ) +
    geom_rect(
        data = cluster_bottom,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "#CFE3D0",
        color = sage,
        linewidth = 0.8
    ) +
    geom_text(
        data = cluster_bottom,
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
        color = ink,
        size = 4.6,
        fontface = "bold"
    ) +
    annotate("text", x = 6.1, y = 4.45, label = "Resample drives, games,\nor contiguous blocks", color = muted, size = 4.3) +
    coord_cartesian(xlim = c(0.3, 9.2), ylim = c(0.3, 8.4), clip = "off") +
    theme_boot

save_plot(cluster_plot, cluster_figure_path, width = 7.3, height = 4.8)

########################
### PARAMETRIC BOOTSTRAP
########################

parametric_outcomes = data.frame(
    xmin = c(5.7, 7.2, 8.7),
    xmax = c(6.7, 8.2, 9.7),
    ymin = 3.0,
    ymax = 4.4,
    label = c("Y*1", "Y*2", "Y*3")
)

parametric_plot = ggplot() +
    geom_rect(
        aes(xmin = 0.9, xmax = 4.1, ymin = 2.6, ymax = 5.0),
        fill = "#F7D8D1",
        color = coral,
        linewidth = 0.8
    ) +
    annotate(
        "text",
        x = 2.5,
        y = 3.8,
        label = "Fitted model\np(y | x, theta-hat)",
        color = ink,
        size = 4.4
    ) +
    geom_segment(
        aes(x = 4.35, y = 3.8, xend = 5.45, yend = 3.8),
        color = coral,
        linewidth = 0.9,
        arrow = arrow_style
    ) +
    geom_rect(
        data = parametric_outcomes,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "#FCEBE6",
        color = coral,
        linewidth = 0.8
    ) +
    geom_text(
        data = parametric_outcomes,
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
        color = ink,
        size = 4.2
    ) +
    annotate("text", x = 7.7, y = 1.55, label = "Simulate new outcomes\nfrom the fitted model", color = muted, size = 4.2) +
    coord_cartesian(xlim = c(0.3, 9.9), ylim = c(0.3, 8.0), clip = "off") +
    theme_boot

save_plot(parametric_plot, parametric_figure_path, width = 7.3, height = 4.8)

#######################
### RESIDUAL BOOTSTRAP
#######################

fit_line = data.frame(
    x = c(0.9, 8.1),
    y = c(2.0, 4.8)
)

observed_pts = data.frame(
    x = c(1.2, 2.3, 4.0, 5.1, 6.4, 7.2),
    y = c(2.5, 2.0, 3.6, 3.0, 4.3, 5.1),
    yhat = c(2.12, 2.55, 3.21, 3.63, 4.14, 4.45)
)

residual_plot = ggplot() +
    geom_segment(
        aes(x = fit_line$x[1], y = fit_line$y[1], xend = fit_line$x[2], yend = fit_line$y[2]),
        color = rose,
        linewidth = 1
    ) +
    geom_segment(
        data = observed_pts,
        aes(x = x, y = yhat, xend = x, yend = y),
        color = rose,
        linewidth = 0.7,
        linetype = "dashed"
    ) +
    geom_point(
        data = observed_pts,
        aes(x = x, y = yhat),
        shape = 21,
        fill = "#F4D3E0",
        color = rose,
        size = 3.4,
        stroke = 0.8
    ) +
    geom_point(
        data = observed_pts,
        aes(x = x, y = y),
        shape = 21,
        fill = "#FCEAF0",
        color = rose,
        size = 3.4,
        stroke = 0.8
    ) +
    geom_segment(
        aes(x = 8.5, y = 3.25, xend = 9.6, yend = 3.25),
        color = rose,
        linewidth = 0.9,
        arrow = arrow_style
    ) +
    annotate("text", x = 7.3, y = 1.2, label = "Keep X fixed, resample residuals e*,\nthen form Y* = Y-hat + e*", color = muted, size = 4.1) +
    coord_cartesian(xlim = c(0.3, 9.9), ylim = c(0.3, 8.0), clip = "off") +
    theme_boot

save_plot(residual_plot, residual_figure_path, width = 7.3, height = 4.8)
