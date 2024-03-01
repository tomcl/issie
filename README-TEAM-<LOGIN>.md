# Team-Phase Work JW3621 

## D2 sheetOrderFlip Build
Effert in writing helper datatypes and functions to perform two algorithms mentioned in the project requirement

## 1. **Exhaustive Search Algorithm**

This approach utilizes two new data types, `symbolScript` and `modelScript`, to describe the configuration of a `SheetT.Model`:

- A `symbolScript` contains all parameters that can be changed for a single symbol.
- A `modelScript` is a list of `symbolScript`, with each entry corresponding to one symbol in the model.

### Functions

- **`generateModelScript`**: Generates all possible `symbolScript` instances for one symbol. This function is pivotal in exploring the configuration space of individual symbols within the model.

- **`generateAllModelScripts`**: Produces all possible `modelScript` configurations for the entire model. By iterating through every symbol and applying `generateModelScript`, it constructs a comprehensive list of model configurations.

- **`optimizeFlipForComponents`**: Conducts an evaluation across all generated model configurations, selecting and returning the one with the lowest number of overlaps. This function is crucial for identifying the optimal model configuration with minimized component overlap.


## 2. **Heuristic Approach**
