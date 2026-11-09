for (p in c("amen", "latentnet", "sna", "clubSandwich", "igraph", "network")) {
  cat(sprintf("%-14s %s\n", p,
      if (requireNamespace(p, quietly = TRUE)) "installed" else "not installed"))
}
