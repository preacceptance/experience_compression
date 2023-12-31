########## ---- CONTAINS FUNCTIONS ONLY FOR PLOTTING (Common ones are in '/tools/common_functions.R') ---- #########
## For Study: Customer Journeys

MakeGroupedBarPlot <- function(data_plot_long, wtp = FALSE, plot_only_satisfaction=FALSE) {
    "
    Plot the grouped bar graph in order of ascending satisfaction scores
    Input: data_plot_long
    Output: grouped_bar_plot (the grouped bar graph)
    "

    print("Generating bar plot...")
    ylab <- "Mean Rating"
    if(plot_only_satisfaction) {
        ylab <- "Mean Satisfaction"
    }

    if (wtp) {
        data_plot_long <- data_plot_long[data_plot_long$question_type == "wtp_score_avg",]
        grouped_bar_plot <- ggplot(data_plot_long, aes(x = plot_names, y = score, fill = question_type)) +
            geom_bar(position = "dodge", stat = "identity") +
            geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                          position = position_dodge(.9)) +
            xlab("Customer Journey Plots") +
            ylab(ylab) +
            theme(
                plot.title = element_blank(),
                legend.title = element_blank(),
                legend.text = element_text(color = "black", size = 5),
                legend.position = "top",
                legend.title.align = 0.5,
                text = element_text(color = "black", size = 5),
                axis.title.y = element_text(color = "black", size = 7, face = "bold"),
                axis.title.x = element_text(color = "black", size = 7, face = "bold"),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            ) +
            scale_fill_manual(
                name = "Judgment Type",
                breaks = c("wtp_score_avg"),
                labels = c("Willingness to Pay"),
                values = c("#458ff7"),
                guide = guide_legend(title.position = "top")
            )

        return(grouped_bar_plot)
    }

    if(plot_only_satisfaction) {
        data_plot_long <- data_plot_long[data_plot_long$question_type == 'satisfaction_score_avg',]
    }

    data_plot_long <- data_plot_long[data_plot_long['question_type'] != 'wtp_score_avg',]
    grouped_bar_plot <- ggplot(data_plot_long, aes(x = plot_names, y = score, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = 0.2, size = 0.2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the Satisfaction and Desirability of Different Customer Journeys") +
        xlab("Customer Journey Plots") +
        ylab(ylab) +
        scale_y_continuous(breaks = seq(0, 100, 20)) +
        theme_bw() +
        theme(axis.line.y = element_line(colour = "black", size=0.1),
              axis.ticks = element_line(colour = "black", size=0.1),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_blank(), 
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              legend.position = "top",
              legend.title.align = 0.5,
              text = element_text(color = "black", size = 7),
              axis.title.y = element_text(color = "black", size = 7, face = "bold"),
              axis.title.x = element_text(color = "black", size = 7, face = "bold"),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.key.size = unit(0.5, "line")) +
        scale_fill_manual(
            name = "Judgment Type",
            breaks = c("satisfaction_score_avg", "pd_score_avg"),
            labels = c("Satisfaction", "Personal Desirability"),
            values = c("#bf8482", "#82b2f5"),
            guide = guide_legend(title.position = "top")
        ) +
      geom_hline(yintercept=0, size=0.1)
      
    return(grouped_bar_plot)
}


MakeFigure2 <- function(LifelinesPlot, plot_names) {
  # Make "clean" version of individual images for x-axis
  Plotter_2 <- function(equation, index=10) {
    plot(equation, lwd = 30, xlim = c(0, end_age), ylim = c(0, end_y_axis), main = "",
         xlab = "", ylab = "", col = "#99a692", bty="L", yaxt="n", xaxt="n")
  }
  
  # Print the images that will comprise the x-axis
  for (i in 1:length(my_equations)) { 
    png(file = paste0(plot_names[i], "_plot.png"), res=300, width = 2000, height = 2000)
    sapply(my_equations[i], Plotter_2)
    dev.off()
  }
  
  # Assemble images in the order they appear in data_plot_long$plot_names[1:27]
  plot_images <- list()
  
  plots_reordered <- c("linear_rise", "linear_fall", "linear_low", "linear_middle",
                       "linear_high", "exp_rise_concave", "exp_fall_concave",
                       "exp_rise_convex", "exp_fall_convex", "sin_fr_full", "sin_fr_partial",
                       "sin_rf_full", "sin_rf_partial", "sin_rfr_full", "sin_rfr_partial", 
                       "sin_frf_full", "sin_frf_partial", "sin_frfr", "sin_rfrf", 
                       "logistic_rise", "logistic_fall", "positive_change_full", "positive_change_partial", 
                       "negative_change_full", "negative_change_partial", "linear_rise_sharp_fall",
                       "linear_rise_sharp_fall_exp_rise")
  
  for (i in 1:27) {
    plot_images[[i]] = readPNG(paste0(plots_reordered[i], "_plot.png"))
  }
  
  # Loading all the individual plots in a 3x9 format (3 rows and 9 columns).
  mystack = vector("list", 27)
  
  for (i in 1:27) {
    mystack[[i]] <- grid::rasterGrob(plot_images[[i]])
  }
  margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
  
  # Arrange plots in a grid using grid.arrange from the gridExtra package (adjusted layout)
  plot_matrix <- do.call(grid.arrange, c(mystack, ncol = 9, nrow = 3))
  return(plot_matrix)
}

MakeGroupedBarPlotImages <- function(LifelinesPlot, plot_names) {
    "
    Make a plotter function that produces 'clean' (no labels) version of individual images
    for the x-axis. Then, plot the images in order of ascending satisfaction scores,
    which can be determined by the order in data_plot_long$plot_names[1:27].
    Input: grouped_bar_plot, plot_names
    Output: the plot labels for the grouped bar graph and the sentiment bar graph
    "

    # Make "clean" (no labels) version of individual images for x-axis
    Plotter_2 <- function(equation, index=10) {
        plot(equation, lwd = 30, xlim = c(start_age, end_age), ylim = c(0, end_y_axis), main = "",
             xlab = "", ylab = "", axes = FALSE, col = "#99a692")

        return(Plotter_2)
    }

    # Print the images that will comprise the x-axis
    for (i in 1:length(my_equations)) { #print individual plots
        png(file = paste0(plot_names[i], "_plot.png", ""))
        sapply(my_equations[i], Plotter_2)
        dev.off()
    }

    png(file = paste0("NA_plot.png", ""))
    sapply("", Plotter_2)
    dev.off()

    # Assemble images in the order of data_plot_long$plot_names[1:27]
    plot_images <- axis_canvas(LifelinesPlot, axis = 'x')

    for (i in 1:27) {
        plot_images <- plot_images + draw_image(paste0(data_plot_long$plot_names[i], "_plot.png"), x = i - 0.5)
    }

    return(plot_images)
}

CV_plotter <- function(results_df, x_order, results_order, ques_type, x_labels, random_data) {
    "
    What this function does: creates a grouped box plot of the cross-validated prediction results
    Inputs: results_df, x_order, results_order, ques_type, x_labels
    Output: a boxplot of participant rating predictions with either principal components or predictors
    "

    results_df[results_df['question_type'] == 'satisfaction_results', 'question_type'] = " Satisfaction"
    results_df[results_df['question_type'] == 'personal_desirability_results', 'question_type'] = "Personal Desirability"

    grouped_box_plot <- ggplot(data = results_df, aes(x = x_order, y = results_order)) +
        scale_colour_manual(values = c("#bf8482", "#82b2f5")) +
        scale_x_discrete() +
        stat_summary(fun = get_mean, geom = "point", shape = 20, size = 5, 
            aes(group = question_type, color=question_type), position = position_dodge(.75)) +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", size=0.3, 
            aes(group = question_type, width=0.3), color="black", position = position_dodge(.75)) +
        ggtitle(paste0("Satisfaction and Desirability Predictions with ", x_labels)) +
        xlab(x_labels) +
        ylab("Prediction Performance\n(Cross-Validated Pearson's r)") +
        scale_y_continuous(breaks = round(seq(-1, 1, by = 0.2), 1)) +
        scale_fill_manual(
            guide = guide_legend(title.position = "top")) +
        geom_hline(yintercept = get_mean(random_data$random), size=0.3) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = get_se(random_data$random)$ymin,
                          ymax = get_se(random_data$random)$ymax, fill = "black", alpha = .2, color = NA) +
        theme_bw() +
        if (x_labels == "Predictors") {
            theme(axis.line.y = element_line(colour = "black", size=0.1),
              axis.ticks = element_line(colour = "black", size=0.1),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_blank(), 
              legend.title = element_blank(),
              legend.text = element_text(color = "black", size = 7),
              legend.position = "top",
              legend.title.align = 0.5,
              text = element_text(color = "black", size = 7),
              axis.title.y = element_text(color = "black", size = 7, face = "bold"),
              axis.title.x = element_text(color = "black", size = 7, face = "bold"),
              axis.text.x = element_text(color = "black", angle = 60, vjust = 1, hjust = 1),
              axis.ticks.x = element_blank(),
              legend.key.size = unit(0.5, "line"))
        } else {
            theme(element_blank(),
                  plot.title = element_blank(), #element_text(color = "black", size=32, face = "bold", hjust = 0.5),
                  text = element_text(color = "black", size = 7),
                  axis.title.y = element_text(color = "black", size = 7, face = "bold"),
                  axis.title.x = element_text(color = "black", size = 7, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
                  axis.text.x = element_text(color = "black", size = 5),
                  legend.title = element_blank(), #element_text(color = "black", size=30),
                  legend.text = element_text(color = "black", size = 7),
                  legend.position = "top",
                  legend.title.align = 0.5) }


    return(grouped_box_plot)
}