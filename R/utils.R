get_package_version <- function(pkg = "invicoctrlr"){

  base::gsub('\n\\s+', ' ',
       utils::packageDescription(pkg = pkg,
                                 fields = 'Version'))

}
