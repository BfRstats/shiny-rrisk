# stats function setNames does not work with obejct = NULL
# for obvious reasons. But we need to, so here is a workaround using the 
# properties of list
set_names <- function(object, object_name)
{
  tmp <- list(object)
  names(tmp) <- object_name
  tmp
}