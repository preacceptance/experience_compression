
CV_plotter <- function(results_df, x_order, results_order, ques_type, x_labels, random_data = NULL, y_axis = "Pearson's r", no_kfold = FALSE) {
    "
    What this function does: creates a grouped box plot of the cross-validated prediction results
    Inputs: results_df, x_order, results_order, ques_type, x_labels, sum_willing
    Output: a boxplot of participant rating predictions with either principal components or predictors
    "

    y_label <- paste0("Prediction Accuracy\n(Cross-Validated ", y_axis, ")")

    box_label <- "Willingness to Buy"
    if (y_axis != "Pearson's r") {
        box_label <- "Raffle Choice"
    }

    if (no_kfold) { y_label <- paste0("Prediction Accuracy\n(", y_axis, ")") }
    
    grouped_box_plot <- ggplot(data = results_df, aes(x = x_order, y = results_order)) +
        scale_x_discrete() +
        stat_summary(fun = get_mean, geom = "point", color="#82b2f5", shape = 20, size = 5, position = position_dodge(.75)) +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", size=0.3, color="black", aes(group = question_type, width = 0.3), position = position_dodge(.75)) +
        ggtitle(paste0("Willingness Predictions with ", x_labels)) +
        xlab(x_labels) +
        ylab(y_label) +
        ylim(0, 0.4) +
        scale_y_continuous(breaks = round(seq(-1, 1, by = 0.2), 1)) +
        scale_fill_manual(
            name = "Judgment Type",
            breaks = c("willing_results"),
            labels = c(box_label),
            values = c("#56B4E9"),
            guide = guide_legend(title.position = "top")) +
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
        }

    if(!is.null(random_data)) {
        grouped_box_plot <- grouped_box_plot +
            geom_hline(yintercept = get_mean(random_data$random)) +
        ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = get_se(random_data$random)$ymin, ymax = get_se(random_data$random)$ymax, fill = "black", alpha = .2, color = NA)
    }

    if(y_axis == "F1 Score") {
        grouped_box_plot <- grouped_box_plot + geom_hline(yintercept = 0.222)
    }

    return(grouped_box_plot)
}


MakeGroupedBarPlot <- function(data_plot_long, raffle_percentage=FALSE) {
    "
    Plot the grouped bar graph in order of ascending willing scores
    Input: data_plot_long
    Output: grouped_bar_plot (the grouped bar graph)
    "

    if(raffle_percentage) {
        grouped_bar_plot <- ggplot(data_plot_long, aes(x = cluster_names, y = raffle_percentage, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        ggtitle("Summarizing the willing of Different Customer Journeys") +
        xlab("Trailer Experience Clusters") +
        ylab("Percentage") +
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
            breaks = c("willing_score_avg"),
            values = c("#82b2f5"),
            guide = guide_legend(title.position = "top")
        )
    } else {
        grouped_bar_plot <- ggplot(data_plot_long, aes(x = cluster_names, y = score, fill = question_type)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_errorbar(aes(ymin = score - sd, ymax = score + sd), width = .2,
                      position = position_dodge(.9)) +
        ggtitle("Summarizing the willing of Different Customer Journeys") +
        xlab("Trailer Experience Clusters") +
        ylab("Mean Willingness to Pay") +
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
            breaks = c("willing_score_avg"),
            labels = c("Willingness to Buy"),
            values = c("#82b2f5"),
            guide = guide_legend(title.position = "top")
        )
    }

    return(grouped_bar_plot)
}


MakeGroupedBarPlotImages <- function(LifelinesPlot, data_plot_long) {
    plot_images <- axis_canvas(LifelinesPlot, axis = 'x')

    sorted_cluster_names <- c(data_plot_long[, 'cluster_names'])

    for (i in 1:n_clusters) {
        plot_images <- plot_images + draw_image(paste0("./plots/cluster/", sorted_cluster_names[i], "_", n_clusters, ".png"), x = i - 0.5)
    }

    return(plot_images)
}
