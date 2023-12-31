MakeGroupedBarPlot <- function(data_plot_long) {
    "
    Plot the grouped bar graph in order of ascending hiring likelihood scores
    Input: data_plot_long
    Output: grouped_bar_plot (the grouped bar graph)
    "

    grouped_bar_plot <- ggplot(data_plot_long, aes(x = plot_names, y = score)) +
        geom_bar(position = "dodge", stat = "identity", fill = "#82b2f5") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the Hiring Likelihood of Different Interview Trajectories") +
        xlab("Interview Performance Plots") +
        ylab("Mean Rating") +
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
        geom_hline(yintercept=0, size=0.1)

    return(grouped_bar_plot)
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
    What this function does: creates a box plot of the cross-validated prediction results
    Inputs: results_df, x_order, results_order, ques_type, x_labels, sum_hiring_likelihood
    Output: a boxplot of participant rating predictions with either principal components or predictors
    "

    grouped_box_plot <- ggplot(data = results_df, aes(x = x_order, y = results_order)) +
        scale_x_discrete() +
        stat_summary(fun = get_mean, geom = "point", color="#82b2f5", shape = 20, size = 5, position = position_dodge(.75)) +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", size=0.3, color="black", aes(group = question_type, width = 0.3), position = position_dodge(.75)) +
        ggtitle(paste0("Hiring Likelihood Predictions with ", x_labels)) +
        xlab(x_labels) +
        ylab("Prediction Performance\n(Cross-Validated Pearson's r)") +
        scale_y_continuous(breaks = round(seq(-1.20, 1.19, by = 0.2), 1)) +
        scale_fill_manual(
            name = "Judgment Type",
            breaks = c("hiring_likelihood"),
            labels = c("Hiring Likelihood"),
            values = c("#3c7ea3"),
            guide = guide_legend(title.position = "top")) +
        geom_hline(yintercept = get_mean(random_data$random), size=0.3) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = get_se(random_data$random)$ymin, ymax = get_se(random_data$random)$ymax, fill = "black", alpha = .2, color = NA) +
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
              text = element_text(color = "black", size = 25),
              axis.title.y = element_text(color = "black", size = 30, face = "bold"),
              axis.title.x = element_text(color = "black", size = 30, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.text.x = element_text(color = "black", size = 17.5),
              legend.title = element_blank(), #element_text(color = "black", size=25),
              legend.position = "top",
              legend.title.align = 0.5)
    }


    return(grouped_box_plot)
}