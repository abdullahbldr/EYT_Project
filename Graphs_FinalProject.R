################################################################################

# Abdullah Bıldır - 2536332

# Alpaslan Doğankollu - 2698785 

################################################################################

install.packages("ggplot2")         # We installed the required packages.
install.packages("writexl")
library("ggplot2")
library("writexl")

################################################################################

# The graph of age-efficiency profiles and its margin or error

ageeff_graph <- ggplot(data.frame(e), aes(x=(1:J))) + geom_line(aes(y=e), colour="navyblue", linewidth=1.5) +
                                                      geom_ribbon(aes(ymin=e + margin_error, ymax=e-margin_error), alpha=0.1, fill = "blue",  color = "black", linetype = "dotted")
ageeff_graph <- ageeff_graph + labs(title="Labor Productivity / Age-Efficiency by Age", x="Age", y="Labor Productivity")
print(ageeff_graph)

################################################################################

# The graph of unemployment rates

un_graph <- ggplot(yearsun, aes(x=Years)) + geom_line(aes(y=Overall_Unemployment, colour="Overall"), linewidth=1.5) + 
                                         geom_line(aes(y=Youth_Unemployment, colour = "Youth"), linewidth=1.5) +
                                         geom_vline(xintercept = 2023, colour="grey2",size=1) +
                                         geom_vline(xintercept = 2038, colour="grey2",size=1) +
                                         annotate('rect', xmin=2022, xmax=2024, ymin=-Inf, ymax=Inf, alpha=.7, fill='grey') +
                                         annotate('rect', xmin=2037, xmax=2039, ymin=-Inf, ymax=Inf, alpha=.7, fill='grey') +
                                         annotate('rect', xmin=2024, xmax=2037, ymin=-Inf, ymax=Inf, alpha=.7, fill='lightgrey') +
                                         scale_color_manual(name = "Groups", 
                                                            breaks = c("Overall", "Youth"),
                                                            values = c("navyblue", "red2"))
un_graph <- un_graph + labs(title="Evolution of Unemployment Rate by Years", x="Years", y="Unemployment Level")
print(un_graph)

################################################################################

# The graph of labor force productivity rates
# Here, we preferred to remove the values of the target group outside the influence period since the group is changing by construction outside the interval.

yearsun_cut <- yearsun
yearsun_cut$Target_LFP[1:(2023-starty)] <- NA
yearsun_cut$Target_LFP[(2042-starty):(endy-starty+2)] <- NA

lfp_graph <- ggplot(yearsun_cut, aes(x=Years)) + geom_line(aes(y=Overall_LFP, colour="Overall"), linewidth=1.5) + 
                                          geom_line(aes(y=Youth_LFP, colour="Youth"), linewidth=1.5) +
                                          geom_line(aes(y=Productive_LFP, colour="Productive"), linewidth=1.5) +
                                          geom_line(aes(y=Target_LFP, colour="Target"), linewidth=1.5) +
                                          geom_line(aes(y=Retired_LFP, colour="Retired"), linewidth=1.5) +
                                          geom_vline(xintercept = 2023, colour="grey2",size=1) +
                                          geom_vline(xintercept = 2038, colour="grey2",size=1) +
                                          annotate('rect', xmin=2022, xmax=2024, ymin=-Inf, ymax=Inf, alpha=.7, fill='grey') +
                                          annotate('rect', xmin=2037, xmax=2039, ymin=-Inf, ymax=Inf, alpha=.7, fill='grey') +
                                          annotate('rect', xmin=2024, xmax=2037, ymin=-Inf, ymax=Inf, alpha=.7, fill='lightgrey') +
                                          scale_color_manual(name = "Groups", 
                                                             breaks = c("Overall", "Youth", "Productive", "Target", "Retired"),
                                                             values = c("navyblue", "red2", "purple3", "orange", "limegreen"))
lfp_graph <- lfp_graph + labs(title="Evolution of Labor Force Participation Rate by Years", x="Years", y="Labor Force Participation Level")
print(lfp_graph)

################################################################################

# We exported the table of results with cropped target group.

write_xlsx(yearsun_cut, "Table_1.xlsx")

################################################################################