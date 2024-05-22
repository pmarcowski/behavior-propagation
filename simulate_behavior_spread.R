# Title: Simulate behavior spread in a social network
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-02-03
# Copyright (c) 2024 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This script simulates the spread of behaviors within a social network,
# represented as a grid. Each cell can be empty, contain an unaffected agent, 
# or an agent influenced by one of several behaviors. The simulation models 
# how behaviors spread through social influence, with agents adopting behaviors
# based on specific probabilities. Unaffected agents can adopt a behavior if 
# adjacent to an agent with that behavior, while affected agents can switch to
# a different behavior if exposed to it. The process visualizes the dynamics of
# behavior spread, including adoption and switching, across a defined number of
# steps. Parameters such as the probabilities for adopting or switching 
# behaviors, the initial density of agents, and the number of origin points for
# behaviors are configurable, enabling exploration of different scenarios of 
# social influence and behavior dynamics. The script also calculates and 
# displays various statistics related to the agent population and behavior 
# distribution, such as the total number of agents, the count and percentage of
# agents adopting each behavior, and the current simulation step.

# Initialize parameters
grid_size <- 100 # size of grid representing social network
tlength <- 100 # total number of simulation steps
num_behaviors <- 2 # total number of distinct behaviors that can spread
prob_unaffected <- 0.75 # probability of unaffected agent adopting behavior
prob_affected <- 0.25 # probability of affected agent switching to different behavior
initial_agents_density <- 0.7 # initial agent density within grid
origins_per_behavior <- c(2, 2) # number of origin points for each behavior

# Create color map for visualizing agent states
color_map <- c("white", "grey", hcl.colors(num_behaviors, palette = "Dark 2"))

# Validate parameter configuration
if (length(origins_per_behavior) != num_behaviors) {
  stop("Error: The length of 'origins_per_behavior' must match 'num_behaviors'.")
}

simulate_behavior_spread <- function(tlength) {
  # Simulates the spread of behaviors within a social network, represented as a grid.
  # It updates the grid over a series of time steps, reflecting the adoption
  # and switching of behaviors among agents. The grid is visualized at each time
  # step, displaying the dynamic spread and evolution of behaviors through color-coded cells.
  #
  # Args:
  #   tlength: Total number of time steps the simulation will run.
  #
  # Returns:
  #   The function does not return a value but generates a series of visualizations
  #   showing the state of the social network at each step. It concludes when
  #   either the specified number of steps is completed or if a step occurs where no new
  #   behavior spreads, indicating a stable configuration has been reached.

  # Initialize grid with empty cells and agents
  # 0 = empty cell
  # 1 = unaffected agent
  grid <- matrix(
    sample(0:1, grid_size^2, replace = TRUE, prob = c(1 - initial_agents_density, initial_agents_density)),
    nrow = grid_size, ncol = grid_size
  )

  # Assign origin points for each behavior within grid
  for (behavior in 1:num_behaviors) {
    for (origin in 1:origins_per_behavior[behavior]) {
      repeat {
        selected_cell <- sample(which(grid == 1), 1)
        if (grid[selected_cell] == 1) { # ensure the cell is currently unaffected
          # Assign behavior (behaviors start from index 2)
          grid[selected_cell] <- behavior + 1
          break
        }
      }
    }
  }

  # Vector to track if behaviors are spreading during each simulation step
  behavior_spreading <- rep(FALSE, num_behaviors)

  # Main simulation loop
  for (t in 1:tlength) {
    new_grid <- matrix(nrow = grid_size, ncol = grid_size, data = grid) # prepare new grid state
    behavior_spreading <- rep(FALSE, num_behaviors) # reset tracking for current step

    # Update grid state based on current behaviors and spread probabilities
    for (i in 1:nrow(grid)) {
      for (j in 1:ncol(grid)) {
        # Identify neighboring cells to consider for behavior spread
        neighbors <- c(
          grid[i, max(j - 1, 1)], # west
          grid[max(i - 1, 1), j], # north
          grid[i, min(j + 1, ncol(grid))], # east
          grid[min(i + 1, nrow(grid)), j] # south
        )

        current_state <- grid[i, j]

        # Check and apply behavior spread rules
        for (behavior in 2:(num_behaviors + 1)) {
          if ((current_state == 1 && any(neighbors == behavior) && runif(1) < prob_unaffected) ||
            (current_state >= 2 && current_state != behavior && any(neighbors == behavior) && runif(1) < prob_affected)) {
            new_grid[i, j] <- behavior
            behavior_spreading[behavior - 1] <- TRUE
            break
          }
        }
      }
    }

    grid <- new_grid # apply updated state for next step
    
    # Calculate statistics
    total_agents <- sum(grid > 0)
    behavior_counts <- table(grid[grid > 1])
    behavior_percentages <- round(behavior_counts / total_agents * 100, 2)
    
    # Set horizontal and vertical offsets for labels positioning relative to plot
    x_offset <- 2
    y_offset <- 0.06
    
    # Create vector of labels for statistics
    labels <- c(
      substitute(paste(bold("Behavior Spread Simulation"))),
      paste("Time:", t, "/", tlength),
      paste("Total agents:", total_agents)
    )
    for (b in 1:num_behaviors) {
      labels <- c(labels, paste0("Behavior ", b, ": ", behavior_counts[b], " (", behavior_percentages[b], "%)"))
    }
    
    # Generate sequence of y-coordinates for labels
    y_coords <- seq(
      from = grid_size - y_offset * grid_size,
      by = -y_offset * grid_size,
      length.out = length(labels)
    )

    # Visualize current state of grid
    par(mfrow = c(1, 1), pty = "s")
    image(1:grid_size, 1:grid_size, t(grid[nrow(grid):1, ]), col = color_map, axes = FALSE, xlab = "", ylab = "")
    box()
    
    # Display labels by applying text function to each label and y-coordinate  
    mapply(
      text,
      x = rep(-x_offset, length(labels)),
      y = y_coords,
      labels = labels,
      pos = 2,
      cex = 0.8,
      xpd = TRUE
    )
    
    Sys.sleep(.1) # pause for animation effect

    if (!any(behavior_spreading)) { # if no behaviors spread, stop simulation
      message("Simulation stopped early. No behavior spreading at time: ", t)
      break
    }

    # Message simulation complete
    if (t == tlength) {
      message("Simulation complete after specified time: ", tlength)
    }
  }
}

# Run simulation for specified time steps
simulate_behavior_spread(tlength)
