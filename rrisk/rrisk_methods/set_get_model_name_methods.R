rriskModelClass$set("public", "set_model_name",
  function(model_name) {private$model_name <- model_name})

rriskModelClass$set("public", "get_model_name",
  function() {private$model_name})