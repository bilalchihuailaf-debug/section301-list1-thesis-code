# ============================================================
# Figure-generation code for the thesis
# ============================================================
#
# This script documents the code used to generate the main and
# appendix figures. It defines a shared visual style, a function
# for monthly treated-control average plots, and a function for
# event-study coefficient plots.
# ============================================================

# ============================================================
# Code excerpt G3. Figure-generation code
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(broom)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Shared figure settings
# ------------------------------------------------------------

font_family <- "sans"

econ_red   <- "#E3120B"
dark_grey  <- "#5A5A5A"
light_grey <- "#D9D9D9"
econ_blue  <- "#00A3E0"
econ_teal  <- "#005F73"

ref_month     <- as.Date("2018-03-01")
interim_start <- as.Date("2018-04-01")
post_start    <- as.Date("2018-08-01")
end_date      <- as.Date("2019-12-01")

theme_thesis_clean <- function() {
  theme_minimal(base_size = 11, base_family = font_family) +
    theme(
      text = element_text(family = font_family, colour = "black"),
      plot.title.position = "plot",
      plot.title = element_text(
        size = 15,
        face = "bold",
        hjust = 0,
        margin = margin(b = 2)
      ),
      plot.subtitle = element_text(
        size = 10,
        hjust = 0,
        margin = margin(b = 18)
      ),
      plot.caption = element_blank(),
      axis.title.y = element_text(size = 12, margin = margin(r = 12)),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = light_grey, linewidth = 0.7),
      axis.line.x = element_line(colour = "black", linewidth = 0.45),
      axis.ticks.x = element_line(colour = "black", linewidth = 0.45),
      axis.ticks.length.x = grid::unit(0.18, "cm"),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(12, 18, 12, 10)
    )
}

# ------------------------------------------------------------
# 2. Monthly treated-control average plots
# ------------------------------------------------------------

make_avg_treat_plot <- function(data,
                                y_var,
                                y_label,
                                title_text,
                                subtitle_text,
                                output_stub = NULL,
                                y_as_percent = FALSE,
                                label_vjust = 1.6) {
  
  plot_df <- data %>%
    group_by(month, treat) %>%
    summarise(
      y = mean(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      treat_label = ifelse(
        treat == 1,
        "Treated (Final List 1)",
        "Control (Removed from proposal)"
      )
    )
  
  p <- ggplot(plot_df, aes(x = month, y = y, colour = treat_label)) +
    annotate(
      "rect",
      xmin = post_start,
      xmax = end_date,
      ymin = -Inf,
      ymax = Inf,
      fill = light_grey,
      alpha = 0.35
    ) +
    geom_vline(
      xintercept = interim_start,
      linewidth = 0.8,
      colour = dark_grey,
      linetype = "22"
    ) +
    geom_vline(
      xintercept = post_start,
      linewidth = 0.8,
      colour = econ_red,
      linetype = "22"
    ) +
    geom_line(linewidth = 1.15) +
    annotate(
      "text",
      x = as.Date("2018-05-15"),
      y = Inf,
      label = "Interim",
      colour = dark_grey,
      fontface = "bold",
      size = 3.8,
      family = font_family,
      vjust = label_vjust,
      hjust = 0.5
    ) +
    annotate(
      "text",
      x = as.Date("2018-12-01"),
      y = Inf,
      label = "Post-treatment",
      colour = econ_red,
      fontface = "bold",
      size = 3.8,
      family = font_family,
      vjust = label_vjust,
      hjust = 0.5
    ) +
    scale_colour_manual(
      values = c(
        "Treated (Final List 1)" = econ_blue,
        "Control (Removed from proposal)" = econ_teal
      )
    ) +
    scale_x_date(
      limits = c(min(plot_df$month, na.rm = TRUE), end_date),
      date_breaks = "6 months",
      date_labels = "%b-%Y",
      expand = expansion(mult = c(0.01, 0))
    ) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL,
      y = y_label,
      colour = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_thesis_clean()
  
  if (y_as_percent) {
    p <- p + scale_y_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0.04, 0.04))
    )
  } else {
    p <- p + scale_y_continuous(
      labels = label_number(accuracy = 0.1),
      expand = expansion(mult = c(0.04, 0.04))
    )
  }
  
  if (!is.null(output_stub)) {
    ggsave(
      filename = file.path("outputs", "figures", paste0(output_stub, ".png")),
      plot = p,
      width = 13,
      height = 7,
      dpi = 320,
      bg = "white"
    )
    
    ggsave(
      filename = file.path("outputs", "figures", paste0(output_stub, ".pdf")),
      plot = p,
      width = 13,
      height = 7,
      bg = "white"
    )
  }
  
  return(p)
}

# ------------------------------------------------------------
# 3. Event-study plots
# ------------------------------------------------------------

make_es_plot <- function(model,
                         title_text,
                         y_text,
                         output_stub = NULL,
                         y_as_percent = FALSE,
                         show_period_labels = TRUE) {
  
  td <- broom::tidy(model, conf.int = TRUE)
  
  es <- td %>%
    filter(str_detect(term, "month_id::")) %>%
    mutate(
      month_str = str_extract(term, "\\d{4}-\\d{2}"),
      month = as.Date(paste0(month_str, "-01"))
    ) %>%
    filter(!is.na(month)) %>%
    arrange(month)
  
  ref_row <- data.frame(
    term = "Reference: 2018-03",
    estimate = 0,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    conf.low = 0,
    conf.high = 0,
    month_str = "2018-03",
    month = ref_month
  )
  
  es <- bind_rows(es, ref_row) %>%
    arrange(month) %>%
    filter(month <= end_date)
  
  y_top <- max(es$conf.high, na.rm = TRUE)
  
  p <- ggplot(es, aes(x = month, y = estimate)) +
    annotate(
      "rect",
      xmin = post_start,
      xmax = end_date,
      ymin = -Inf,
      ymax = Inf,
      fill = light_grey,
      alpha = 0.35
    ) +
    geom_vline(
      xintercept = interim_start,
      linewidth = 0.8,
      colour = dark_grey,
      linetype = "22"
    ) +
    geom_vline(
      xintercept = post_start,
      linewidth = 0.8,
      colour = econ_red,
      linetype = "22"
    ) +
    geom_hline(yintercept = 0, linewidth = 0.7, colour = "black") +
    geom_linerange(
      aes(ymin = conf.low, ymax = conf.high),
      linewidth = 0.55,
      colour = "black"
    ) +
    geom_line(linewidth = 0.7, colour = "black") +
    geom_point(size = 2.8, colour = "black") +
    scale_x_date(
      limits = c(min(es$month, na.rm = TRUE), end_date),
      date_breaks = "6 months",
      date_labels = "%b-%Y",
      expand = expansion(mult = c(0.01, 0))
    ) +
    labs(
      title = title_text,
      subtitle = "Point estimates with 95% confidence intervals; reference month = Mar-2018",
      x = NULL,
      y = y_text
    ) +
    coord_cartesian(clip = "off") +
    theme_thesis_clean() +
    theme(legend.position = "none")
  
  if (show_period_labels) {
    p <- p +
      annotate(
        "text",
        x = as.Date("2018-05-15"),
        y = y_top + 0.04 * max(abs(es$estimate), na.rm = TRUE),
        label = "Interim",
        colour = dark_grey,
        fontface = "bold",
        size = 3.8,
        family = font_family
      ) +
      annotate(
        "text",
        x = as.Date("2018-12-01"),
        y = y_top + 0.04 * max(abs(es$estimate), na.rm = TRUE),
        label = "Post-treatment",
        colour = econ_red,
        fontface = "bold",
        size = 3.8,
        family = font_family
      )
  }
  
  if (y_as_percent) {
    p <- p + scale_y_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0.04, 0.08))
    )
  } else {
    p <- p + scale_y_continuous(
      labels = label_number(accuracy = 0.1),
      expand = expansion(mult = c(0.04, 0.08))
    )
  }
  
  if (!is.null(output_stub)) {
    ggsave(
      filename = file.path("outputs", "figures", paste0(output_stub, ".png")),
      plot = p,
      width = 13,
      height = 7,
      dpi = 320,
      bg = "white"
    )
    
    ggsave(
      filename = file.path("outputs", "figures", paste0(output_stub, ".pdf")),
      plot = p,
      width = 13,
      height = 7,
      bg = "white"
    )
  }
  
  return(p)
}

# ------------------------------------------------------------
# 4. Figure calls
# ------------------------------------------------------------

p_avg_share <- make_avg_treat_plot(
  data = df,
  y_var = "china_share",
  y_label = "Average China import share",
  title_text = "Average China import share by treatment status",
  subtitle_text = "Monthly means for treated and control HTS8 products",
  output_stub = "figure1_avg_china_share_by_treat",
  y_as_percent = TRUE
)

p_es_share <- make_es_plot(
  model = es_share,
  title_text = "Event study: China import share",
  y_text = "Difference in China import share",
  output_stub = "figure2_event_study_china_share",
  y_as_percent = TRUE
)

p_es_ln_china <- make_es_plot(
  model = es_ln_china,
  title_text = "Event study: Log China import value",
  y_text = "Coefficient relative to Mar-2018",
  output_stub = "figureA1_event_study_ln_china"
)

p_es_ln_nonchina <- make_es_plot(
  model = es_ln_nonchina,
  title_text = "Event study: Log non-China import value",
  y_text = "Coefficient relative to Mar-2018",
  output_stub = "figureA2_event_study_ln_nonchina"
)

p_avg_ln_china <- make_avg_treat_plot(
  data = df,
  y_var = "ln_china_value",
  y_label = "Average log(1 + China imports)",
  title_text = "Average log China import value by treatment status",
  subtitle_text = "Monthly means for treated and control HTS8 products",
  output_stub = "figureA3_avg_ln_china_by_treat"
)

p_avg_ln_nonchina <- make_avg_treat_plot(
  data = df,
  y_var = "ln_nonchina_value",
  y_label = "Average log(1 + non-China imports)",
  title_text = "Average log non-China import value by treatment status",
  subtitle_text = "Monthly means for treated and control HTS8 products",
  output_stub = "figureA4_avg_ln_nonchina_by_treat"
)
