
# Wow. we never need to actually refer to the variable by name.
var.name <- readline("What variable in the environment would you like to double? ")
cat("The doubled value is: ", as.numeric(get(var.name)) * 2, "\n")
