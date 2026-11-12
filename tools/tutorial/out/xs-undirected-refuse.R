fit_selection(mutual ~ dyad(affect_gap, lag = 1) + prior_tie(lag = 1),
              data = pairs_data)
