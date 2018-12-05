# This function takes combined data on the location of bites, and creates a new
# column in the dataframe detailing TRUE/NA.

add_bite_loc = function(dataframe, bite.site.col="Bite.site"){

  # Create new empty column in dataframe
  dataframe[["Bite.site.Head...neck"]] <- "false"
  dataframe[["Bite.site.Arms"]] <- "false"
  dataframe[["Bite.site.Hands"]] <- "false"
  dataframe[["Bite.site.Trunk"]] <- "false"
  dataframe[["Bite.site.Legs"]] <- "false"
  dataframe[["Bite.site.Feet"]] <- "false"

  # Add in TRUE if detail is correct
  dataframe[["Bite.site.Head...neck"]][grep("head|neck|cheek|ear|jaw|face|eye|lip|mouth|nose|chin|throat", dataframe[[bite.site.col]], ignore.case = TRUE)] <- "true"
  dataframe[["Bite.site.Arms"]][grep("arm|elbow|aram|wrist", dataframe[[bite.site.col]], ignore.case = TRUE)] <- "true"
  dataframe[["Bite.site.Hands"]][grep("hand|finger|thumb|palm|fin|fingr", dataframe[[bite.site.col]], ignore.case = TRUE)] <- "true"
  dataframe[["Bite.site.Trunk"]][grep("trunk|teat|body|butock|bucket|bank|soulder|waist|bottom|breast|buttock|Guteal|chest|torso|hip|side|flank|back|shoulder|stomach|penis|scrotum|private|ribs|abdomen|testicles|testes|belly|groin", dataframe[[bite.site.col]], ignore.case = TRUE)] <- "true"
  dataframe[["Bite.site.Legs"]][grep("leg|calf|knee|thigh|ankle|khee", dataframe[[bite.site.col]], ignore.case = TRUE)] <- "true"
  dataframe[["Bite.site.Feet"]][grep("feet|toe|foot", dataframe[[bite.site.col]], ignore.case = TRUE)] <- "true"

  return(dataframe)
}