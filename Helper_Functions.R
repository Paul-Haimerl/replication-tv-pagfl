loadPackages <- function(pckg) {
  if (!require(pckg, character.only = TRUE)) {
    install.packages(pckg, dep = TRUE, quiet = T)
  }
  require(pckg, character.only = TRUE, quietly = T)
}

create_output_dir <- function(name, MC = TRUE) {
  if (MC == TRUE){
      # Create a parent folder for all MC simulations
      path <- paste0(getwd(), "/Simulation_output/")
      if (!dir.exists(path)) {
          dir.create(path)
      }
      # Create a subfolder
      path <- paste0(path, name, "/")
  } else {
      path <- paste0(getwd(), "/", name, "/")
  }
  if (!dir.exists(path)) {
    dir.create(path)
  }
  return(path)
}

# Evaluation metrics for the slope parameters: RMSE and Bias
getParamMetrics <- function(alpha_hat, beta_hat, beta_oracle, beta_0, groups_0) {
  # RMSE overall
  RMSE_mat <- t(sapply(list(alpha_hat, beta_hat, beta_oracle), function(estim, beta_0) {
    rowMeans(sqrt(colMeans((estim - beta_0)^2, na.rm = TRUE)))
  }, beta_0 = beta_0))
  if (nrow(RMSE_mat) == 1) RMSE_mat <- t(RMSE_mat)
  rownames(RMSE_mat) <- c("post", "pse", "oracle")
  # RMSE over time
  RMSE_t_list <- lapply(list(alpha_hat, beta_hat, beta_oracle), function(estim, beta_0) {
    sqrt(apply((estim - beta_0)^2, c(1, 2), mean))
  }, beta_0 = beta_0)
  names(RMSE_t_list) <- c("post", "pse", "oracle")
  # RMSE over time and groups
  RMSE_k_t_list <- lapply(list(alpha_hat, beta_hat, beta_oracle), function(estim, beta_0, groups_0) {
    RMSE_list <- lapply(1:max(groups_0), function(k, x, beta_0, groups_0) {
      # Collect all individuals that belong to the same true group
      indx <- groups_0 == k
      group_0_delta_sq <- (x[, , indx] - beta_0[, , indx])^2
      if (is.matrix(group_0_delta_sq) | is.vector(group_0_delta_sq)) {
        group_0_delta_sq <- array(group_0_delta_sq,
          dim = c(NROW(group_0_delta_sq), dim(x)[2], sum(indx))
        )
      }
      rmse_k <- sqrt(apply(group_0_delta_sq, c(1, 2), mean))
      return(rmse_k)
    }, x = estim, beta_0 = beta_0, groups_0 = groups_0)
    RMSE_array <- array(unlist(RMSE_list), dim = c(dim(RMSE_list[[1]])[1], dim(RMSE_list[[1]])[2], max(groups_0)))

    return(RMSE_array)
  }, beta_0 = beta_0, groups_0 = groups_0)
  names(RMSE_k_t_list) <- c("post", "pse", "oracle")
  return(list(RMSE = RMSE_mat, RMSE_t = RMSE_t_list, RMSE_k_t = RMSE_k_t_list))
}

# Evaluation metrics for the estimated groupings: NMI and indicator whether correct
getGroupMetrics <- function(groups_hat, groups_0) {
  # Emp freq of correct K and grouping
  correct <- ifelse(all(groups_hat == groups_0), 1, 0)
  correct_K <- ifelse(max(groups_hat) == max(groups_0), 1, 0)
  # Adjusted Rand index
  ARI <- aricode::ARI(groups_0, groups_hat)
  return(c(ARI = ARI, Perc_correct = correct, Perc_K = correct_K))
}

prep_data_inst <- function(data_path = "Data") {
  # Read in the data and pivot
  GDP <- read_excel(paste0(data_path, "/GDP.xlsx")) %>%
    select(-`Country Name`) %>%
    pivot_longer(-`Country Code`, names_to = "year", values_to = "gdp") %>%
    rename(country_code = `Country Code`)
  CO2 <- read_excel(path = paste0(data_path, "/CO2.xlsx"), sheet = "v2024") %>%
    pivot_longer(-year, values_to = "co2", names_to = "country") %>%
    mutate(co2 = co2 * 3.664)

  # Merge
  map <- read_excel(paste0(data_path, "/country_name_map.xlsx"))
  CO2 <- merge(CO2, map, by = "country", all.x = T)
  df <- merge(CO2, GDP, by = c("country_code", "year")) %>%
    drop_na() %>%
    mutate(
      gdp = gdp / 1e9,
      intens = co2 / gdp
    ) %>%
    as_tibble()

  # Pick largest economies and filter
  countries <- filter(GDP, year == 2022) %>%
    arrange(-gdp) %>%
    head(100) %>%
    select(country_code) %>%
    unlist()

  df_out <- df %>%
    filter(country_code %in% countries) %>%
    as_tibble()

  return(df_out)
}

get_legend <- function(fig) {
  tmp <- ggplot_gtable(ggplot_build(fig))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

MC_fig <- function(DGP, replic) {
    coef_plot_list <- lapply(replic[[1]]$result, function(x) {
        coef_array <- x$coefs$post
        if (DGP == 2) {
            coef_mat <- do.call(rbind, lapply(1:dim(coef_array)[3], function(i) coef_array[, , i]))
        } else {
            coef_mat <- matrix(c(coef_array), ncol = dim(coef_array)[2])
        }
        if (DGP == 3) DGP <- 4
        colnames(coef_mat) <- paste0("alpha_", 0:(dim(coef_array)[2] - 1) + DGP - 1)
        groups_hat <- rep(x$estim_groups$groups, each = dim(coef_array)[1])
        groups_0 <- rep(x$sim$groups, each = dim(coef_array)[1])
        res_tib <- as_tibble(cbind(coef_mat,
                                   group_hat = groups_hat, group_0 = groups_0,
                                   ind = rep(1:dim(coef_array)[3], each = dim(coef_array)[1]), index = rep(1:dim(coef_array)[1] / dim(coef_array)[1], dim(coef_array)[3])
        )) %>%
            pivot_longer(cols = -c("group_hat", "group_0", "ind", "index"), names_to = "Coefficient", values_to = "Estimate")
        return(res_tib)
    })
    estimate_tib <- bind_rows(coef_plot_list)
    estimate_tib$mc_run <- rep(1:length(coef_plot_list), each = NROW(coef_plot_list[[1]]))
    x <- replic[[1]]$result[[1]]
    coef_array_0 <- x$sim$beta
    if (DGP == 2) {
        coef_0_mat <- do.call(rbind, lapply(1:dim(coef_array_0)[3], function(i) coef_array_0[, , i]))
    } else {
        coef_0_mat <- matrix(c(coef_array_0), ncol = dim(coef_array_0)[2])
    }
    if (DGP == 3) DGP <- 4
    colnames(coef_0_mat) <- paste0("alpha_", 0:(dim(coef_array_0)[2] - 1) + DGP - 1)
    group_0 <- rep(x$sim$groups, each = dim(coef_array_0)[1])
    true_tib <- cbind(coef_0_mat, group_0) %>%
        as_tibble() %>%
        distinct() %>%
        group_by(group_0) %>%
        mutate(index = 1:n() / n()) %>%
        ungroup()
    if (DGP == 1) {
        true_tib <- true_tib %>%
            group_by(group_0) %>%
            mutate(alpha_0 = alpha_0 - mean(alpha_0)) %>%
            ungroup()
    } else if (DGP == 2) {
        true_tib <- true_tib %>%
            group_by(group_0) %>%
            mutate(alpha_1 = alpha_1 - mean(alpha_1)) %>%
            ungroup()
    }
    true_tib <- true_tib %>%
        pivot_longer(-c(index, group_0), names_to = "Coefficient", values_to = "True")

    plot_tib <- merge(true_tib, estimate_tib, by = c("group_0", "index", "Coefficient"), all.x = T) %>%
        arrange(mc_run, ind, group_0, Coefficient, index) %>%
        as_tibble() %>%
        select(-ind) %>%
        distinct() %>%
        mutate(
            id = factor(paste0(group_hat, "_", mc_run)),
            id = factor(id, levels = sample(levels(id)))
        )
    return(plot_tib)
}
