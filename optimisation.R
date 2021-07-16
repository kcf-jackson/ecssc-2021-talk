find_x_pos_in_y <- Rcpp::cppFunction("
int find_cind(NumericVector x, NumericMatrix y) {
  for (int i = 0; i < y.nrow(); i++) {
    if (x[0] == y[i] && x[1] == y[i + y.nrow()]) {
      return i;
    }
  }
  return -1;
}")
