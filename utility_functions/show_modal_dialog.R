# show_info_dialog <- function(title, text) 
# {
#   modalDialog(title = title,
#               HTML(text))
# }

show_modal_dialog <- function(title, text) 
{
  showModal(
    modalDialog(
      title = title,
      HTML(text)
    )
  )
}