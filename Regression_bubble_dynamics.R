library(ggplot2)
library(data.table)
library(dplyr)
library(reshape2)
library(scales)
library(readxl)
library(gridExtra)


rm(list=ls())
project_dir <- "CMES bub vander"

################################################################################
# Load Data
################################################################################

dat <- readxl::read_xlsx(file.path
                         (project_dir, "DATA_CMES_vander.xlsx"),
                         col_names = TRUE,
                         .name_repair = "minimal")

dat <- melt(data = dat,
            id.vars = c("dp_non","method","method_color"))
setDT(dat)
dat[,method:=factor(method,levels=c("Vander","Vander-volume","EOS"))]
levels(dat$method) <- c("Van der Waals","Van der Waals-volume","Ideal gas")

dat$log_dp_non <- log10(dat$dp_non)
dat$log_value <- log10(dat$value)

variable_labels <- c(
  "Vnon_col" = "V[col]/V[0]",
  "max_Rdot_c_l" = "max~E[LPE]/E[TE]",
  "E_LPEnon_col" = "E[LPE,col]/E[TE]",
  "E_BIEnon_col" = "E[BIE,col]/E[TE]",
  "psh_del_p" = "p[sh]/Delta*p",
  "dBIE_dLPE" = "E[eff]",
  "psh_p_init" = "psh_p_init"
)

func_der <- function(x,y){
  subdat_der <- diff(y) / diff(x)
  midpoints <- (head(x, -1) + tail(x, -1)) / 2
  dat_der <- data.table(x=midpoints,y=subdat_der)
  return(dat_der)
}

# Find cutoff point
subdat <- dat[variable == "max_Rdot_c_l",]
dat_der <- func_der(x=subdat[method=="Van der Waals-volume",log_dp_non],
                    y=subdat[method=="Van der Waals-volume",log_value])
dat_der2 <- func_der(x=dat_der[,x], y=dat_der[,y])

x_cutoff_tmp <- dat_der2[which.max(abs(y)),x]
values <- unique(subdat$log_dp_non)
x_cutoff <- values[which.min(abs(values - x_cutoff_tmp))]
x_cutoff

# Fit regression
plot_list <- table_out <- pred_out <- list()
for (v in names(variable_labels)) {
  subdat <- dat[variable == v]

  ##############
  # Regression: below cutoff
  ##############
  compdat1 <- subdat[log_dp_non < x_cutoff,]
  compdat1[,method:=factor(method,levels=c("Ideal gas","Van der Waals","Van der Waals-volume"))]
  compdat1[,log_dp_non_center:=log_dp_non - min(log_dp_non)]
  ## Fit linear regression
  lm_compdat1 <- lm(log_value ~ log_dp_non_center * method, data = compdat1)
  ## For Plot
  pred_subdat1 <- expand.grid(log_dp_non=unique(compdat1$log_dp_non),
                              method = levels(compdat1$method))
  pred_subdat1$log_dp_non_center <- pred_subdat1$log_dp_non - min(compdat1$log_dp_non)
  pred_subdat1$y <- predict(lm_compdat1,newdat=pred_subdat1)
  ## Coefficient Result
  coef_mat1 <- summary(lm_compdat1)$coefficients
  coef_mat1 <- data.frame(coef_mat1)
  coef_mat1$region <- "below_x_cutoff"
  coef_mat1$variable <- v

  table_mat1 <- data.frame(x=x_cutoff/2,
                           method=c("Van der Waals","Van der Waals-volume"),
                           pvalue=coef_mat1[c("log_dp_non_center:methodVan der Waals",
                                              "log_dp_non_center:methodVan der Waals-volume"),"Pr...t.."])

  ##############
  # Regression: above cutoff
  ##############

  compdat2 <- subdat[log_dp_non > x_cutoff,]
  compdat2[,method:=factor(method,levels=c("Ideal gas","Van der Waals","Van der Waals-volume"))]
  compdat2[,log_dp_non_center:=log_dp_non - min(log_dp_non)]
  lm_compdat2 <- lm(log_value ~ log_dp_non_center * method, data = compdat2)

  ## For plot
  pred_subdat2 <- expand.grid(log_dp_non=unique(compdat2$log_dp_non),
                              method = levels(compdat2$method))
  pred_subdat2$log_dp_non_center <- pred_subdat2$log_dp_non - min(compdat2$log_dp_non)
  pred_subdat2$y <- predict(lm_compdat2,newdat=pred_subdat2)

  ## Coefficient Result
  coef_mat2 <- summary(lm_compdat2)$coefficients
  coef_mat2 <- data.frame(coef_mat2)
  coef_mat2$region <- "above_x_cutoff"
  coef_mat2$variable <- v

  table_mat2 <- data.frame(x=x_cutoff*2,
                           method=c("Van der Waals","Van der Waals-volume"),
                           pvalue=coef_mat2[c("log_dp_non_center:methodVan der Waals",
                                              "log_dp_non_center:methodVan der Waals-volume"),"Pr...t.."])

  table_mat <- rbind(table_mat1,table_mat2)
  table_mat$label <- paste0("p = ", signif(table_mat$pvalue, 3))
  if(v %in% c("psh_del_p","psh_p_init")){
    table_mat$y <- quantile(subdat$log_value,0.1) * c(1,2)
  } else{
    table_mat$y <- min(subdat$log_value,na.rm=T)*c(0.5,0.8)
  }

  # Summary Table
  table_out[[v]] <- rbind(coef_mat1,NA,coef_mat2)

  # Summary Predicted Values
  pred_subdat <- rbind(data.table(pred_subdat1,region="below_x_cutoff"),
                       data.table(pred_subdat2,region="above_x_cutoff"))
  pred_subdat$variable <- v
  pred_out[[v]] <- pred_subdat

  p <- ggplot(subdat, aes(x = log_dp_non, y = log_value, color = method, group = method)) +
    geom_point(alpha=0.2) +
    geom_line(alpha = 0.2) +
    geom_line(data = pred_subdat1, aes(x = log_dp_non, y = y, color = method)) +
    geom_line(data = pred_subdat2, aes(x = log_dp_non, y = y, color = method)) +
    geom_text(data=table_mat, aes(x=x, y=y, label=label, color=method)) +
    theme_bw() +
    labs(
      x = "log(dp_non)",
      y = parse(text = variable_labels[[v]])
    ) +
    scale_color_manual(values = c("Blue", "Red", "Green")) +
    theme(legend.position = "bottom")

  plot_list[[v]] <- p
}

## (1) Plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plot_list[[1]])

for (i in seq_along(plot_list)) {
  plot_list[[i]] <- plot_list[[i]] + theme(legend.position = "none")
}

plot_all <- grid.arrange(arrangeGrob(grobs=plot_list,nrow=3),
                         mylegend, nrow=2,heights=c(10, 1))
ggsave(
  plot = plot_all,
  filename = file.path(project_dir, paste0("1_Plot_Regression.png")),
  width = 10, height = 10, dpi = 300
)


## (2) Summary Table
table_out <- do.call("rbind",table_out)
xlsx::write.xlsx(
  table_out,
  file = file.path(project_dir, "dat_regression_output.xlsx"),
  row.names = TRUE,
  sheetName = "Coef_Pvalue",
  append = TRUE)

## (3) Predicted Values
pred_out <- do.call("rbind",pred_out)
xlsx::write.xlsx(
  pred_out,
  file = file.path(project_dir, "dat_regression_output.xlsx"),
  row.names = FALSE,
  sheetName = "Predicted_Y",
  append = TRUE)

table_out[,,by=c("variable","region")]

