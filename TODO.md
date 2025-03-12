# TODO GarpN

 - Deal with the initial states
   - Find clever way to order relations
     - Check Garp state transitions.  Values that change
	   must be ordered before those that stay constant?
	 - Try random cut and map
     - Hoe te laten zien?
 - Show equation ordering.  Allow for user reordering?	[ok]
   - Group equations in
     - Model equations
	 - Time equations
	 - Initial values
	 - Constants
   - Preserve ordering when saving						[ok]
 - Deal with modifications to the qualitative model
   - Can we preserve as much as possible from the current
     numerical model?
 - Allow for shorter labels if the name of a quantity is
   "Name (Short)", e.g., "Net force (F)".
 - Generic code for peeling arithmetic expressions for
   mapping and analysis.
 - Use multiple columns to reduce size of equations?
 - Extend support for more functions.  This requires
   extending the LaTeX to Prolog parser.  List?
 - Provide support for simulation configuration
   - Simulation time/iterations (can be explored to
     cover all Garp states, get into a stable state,
	 cover N cycles when cyclic).
   - Introduce units?  That may help finding sensible
     values for the constants.
   - (Re)name constants?
 - Equation management
   - Allow renaming constants
   - Allow adding constants.  Make new constants appear
     in the Constants folder.
   - Show and allow editing constants in the formula
     in which it appears.
   - Allow adding units.
   - Provide menu with (domain dependent) additional
     functions.
 - Dynalearn integration
   - How does this server integrate with Dynalearn?
   - How are the UI components integrated/migrated?
   - How are the models stored?
 - Code cleanup en documentation
 - Add tests for
   - Mathlife equation round tripping (LaTeX generation and
     parsing)
   - Model proposal
   - Model Simulation
   - Mapping to Garp states
     - State detection
	 - Asymptotes
 - Validate correspondences
 - Initial steps: no value

## Issues

 - Should P(+) in quantity space mean `X = c*Y` or
   `X is X+c*Y`?
 - Invalid equation warning below equation			[fixed]

## Notes

### Dealing with model updates

 - Reuse constants and initial values.				[working]
 - See whether we can reuse formulas
