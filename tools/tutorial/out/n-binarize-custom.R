strong <- custom_binarize_rule(function(x) x >= 6, name = "contact >= 6")
compute_exposure(co, content = "y", via = binarize(contact, strong), wave = 1)
