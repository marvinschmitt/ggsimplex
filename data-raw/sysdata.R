## Code to prepare sysdata.rda

# pre-computed simplex grid
sysdata.pi_grid = ggsimplex:::construct_simplex_grid(n_x = 2.01 * 101,
                                                          n_y = 2.01*101)









# Save all data in R/sysdata.rda
usethis::use_data(sysdata.pi_grid, 
                  internal=TRUE, overwrite = TRUE, compress="bzip2")
