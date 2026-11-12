fit_influence(affect ~ ar() + exposure(affect, via = confides, normalize = "row"),
              data = demo_affect_teams)
