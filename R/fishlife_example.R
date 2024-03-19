library("FishLife")
fish_db <- FishLife::FishBase_and_RAM
species_details <- Search_species(Genus = "Trisopterus", Species = "luscus", add_ancestors = FALSE)$match_taxonomy
linf = c(42.41)
K = c(0.21)
lm = c(19.5)
#'
#'  # Extract estimates
predict_GP = Plot_taxa(Search_species(Genus = "Trisopterus", Species = "luscus", add_ancestors = FALSE)$match_taxonomy, mfrow = c(3, 2))
#'
#'  # Format new
Ynew_ij = matrix(NA, nrow = length(linf), ncol = length(predict_GP[[1]]$Mean_pred))
colnames(Ynew_ij) = names(predict_GP[[1]]$Mean_pred)
Ynew_ij[, "Loo"] = log(linf)
Ynew_ij[, "K"] = log(K)
Ynew_ij[, "Lm"] = log(lm)
#'
#'  # Update
which_cols = which(names(predict_GP[[1]]$Mean_pred) %in% colnames(FishLife::FishBase_and_RAM$obsCov_jj))
Update = update_prediction(predmean_j = predict_GP[[1]]$Mean_pred[which_cols],
                           predcov_jj = predict_GP[[1]]$Cov_pred[which_cols, which_cols],
                           obscov_jj = FishLife::FishBase_and_RAM$obsCov_jj,
                           Ynew_ij = Ynew_ij[, which_cols])

#'  # Check
cbind("Orig" = Update$updatemean_j, "New" = colMeans(Ynew_ij[, which_cols], na.rm = TRUE), "Updated" = predict_GP[[1]]$Mean_pred[which_cols])

