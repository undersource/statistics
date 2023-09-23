team_of_miu <- data.frame(
    name = c("Miu", "Yuko", "Aiko", "Mai", "Marie"),
    score = c(48, 32, 88, 61, 71)
)

team_of_risa <- data.frame(
    name = c("Risa", "Asuka", "Nana", "Yuki", "Reika"),
    score = c(67, 55, 61, 63, 54)
)


sum_of_sq_devs <- function(team) {
    result <- 0
    mean_of_team <- mean(team$score)

    for (score in team$score) {
        result <- result + (score - mean_of_team)^2
    }

    return(result)
}


dispersion <- function(team) {
    return(sum_of_sq_devs(team) / length(team$score))
}


squared_dev <- function(team) {
    return(sqrt(dispersion(team)))
}


print(paste("Sum of sq devs of Miu's team: ", sum_of_sq_devs(team_of_miu)))
print(paste("Dispersion of Miu's team: ", dispersion(team_of_miu)))
print(paste("Squared dev of Miu's team: ", squared_dev(team_of_miu)))
print(paste("Sum of sq devs of Risa's team: ", sum_of_sq_devs(team_of_risa)))
print(paste("Dispersion of Risa's team: ", dispersion(team_of_risa)))
print(paste("Squared dev of Risa's team: ", squared_dev(team_of_risa)))
