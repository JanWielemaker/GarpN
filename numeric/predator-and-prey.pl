		 /*******************************
		 *             MODEL		*
		 *******************************/

number_of(predator) := number_of(predator) + growth(predator) * δt.
number_of(prey) := number_of(prey) + growth(prey) * δt.

t := t + δt.

growth(predator) := (number_of(predator) * number_of(prey) * c_birthrate_predator) - (number_of(predator) * c_deathrate_predator).
growth(prey) := (number_of(prey) * c_birthrate_prey) - (number_of(predator)*number_of(prey)*c_deathrate_prey).

		 /*******************************
		 *         INITIAL VALUES	*
		 *******************************/

t := 0.
δt := 0.1.

number_of(predator) := 900.
number_of(prey) := 900.
c_birthrate_prey := 0.067.
c_deathrate_prey := 0.000135.
c_birthrate_predator := 0.0000375.
c_deathrate_predator := 0.05.

