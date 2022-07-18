
calculate_loss <- function(action, observed, scale_up_cost, scale_down_cost, max_loss) {
	loss <- 0
	if (observed < action){
		loss <- observed
	}
	else {
		if (observed < action + scale_down_cost){
			loss <- observed + scale_up_cost
		}
		else{
			if (observed < action + scale_down_cost + scale_up_cost){
				loss <- observed + scale_up_cost + scale_down_cost
			}
			else{
				loss <- observed + scale_up_cost + scale_down_cost - (observed - action - scale_down_cost)
			}
		}
	}

	if (loss > max_loss){
		return(max_loss)
	}
	if (loss < 0){
		return(0)
	}
	return(loss)
}


run <- function(data, actions, default_window, eta, scale_down_cost, scale_up_cost){
	print(scale_up_cost)
	scale_cost <- scale_down_cost + scale_up_cost
	weights <- rep(1, times = length(actions))
	max_loss <- max(actions + scale_cost)

	total_reward <- 0
	theoretical_reward <- 0 

	for (s in data){
		prob <- (1-eta)*weights/sum(weights) + eta
		if (any(is.na(prob))){
			print(weights)
			print("prob na")
		}			
		chose <- sample(x = 1:length(actions), 1, replace = T, prob = prob)
		
		reward <- 0
		loss <- unlist(lapply(actions, (function(x) calculate_loss(x, s, scale_up_cost, scale_down_cost, max_loss)))) 
		reward <- 1 - loss/max_loss
		total_reward <- total_reward + reward[chose]

		theo_rew <- 1 - calculate_loss(default_window, s, scale_up_cost, scale_down_cost, max_loss)/max_loss

		theoretical_reward <- theoretical_reward + theo_rew	
		
		# print(sprintf("next timeout: %d, request has arrived after: %d, reward: %f, reward with default window: %f", actions[chose], s, reward, theo_rew))

		weights <- weights * exp(eta * reward/prob)
		if (any(weights > 10000)){
			weights <- weights / 10000
		}
	}

	return(c(total_reward, theoretical_reward))
}


data <- read.csv('filtered_data.csv')
timeline <- as.POSIXct(data[,1])
delta <- diff(timeline)

eta <- 0.05
scale_down_cost <- 20
scale_up_cost <- 20
actions <- seq(0, 200, by = 5)
default_window <- 30

# run(delta, actions, default_window, eta, scale_down_cost, scale_up_cost)

result <- lapply(seq(0, 120, by=5), (function(x) run(delta, actions, default_window, eta, x, x)))

exp3 <- c()
theo <- c()
for (row in result){
	print(row)
	print(row[1])
	print(row[[1]])
	exp3 <- c(exp3, row[1])
	theo <- c(theo, row[2])
}

print(exp3)
print(theo)
plot(1:length(exp3) * 5, as.vector(theo), ylim = c(min(min(exp3), min(theo)), max(max(exp3), max(theo))), xlab = "scale up/down cost in seconds", ylab = "total reward")
points(1:length(exp3) * 5, as.vector(exp3), pch = 19)



