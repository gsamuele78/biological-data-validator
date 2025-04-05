# Understanding Classes in R with R6

## What is a Class?
A **class** is a blueprint for creating objects. Objects are instances of a class and can have properties (data) and methods (functions). Classes allow you to organize and reuse code effectively.

In R, the `R6` package provides a way to define classes using the `R6Class()` function. This approach is object-oriented programming (OOP), where data and behavior are encapsulated together.

---

## Starting with Functions
Before understanding classes, let's start with functions. Functions in R are reusable blocks of code that perform specific tasks.

### Example
```r
# A function to calculate the area of a rectangle
calculate_area <- function(length, width) {
  return(length * width)
}

# Using the function
area <- calculate_area(5, 3)
print(area)  # Output: 15
```

---

## Moving to Classes
A class combines data (properties) and functions (methods) into a single structure. For example, instead of just calculating the area, a class can store the dimensions of a rectangle and provide methods to calculate the area or perimeter.

### Example: Rectangle Class
```r
library(R6)

Rectangle <- R6Class("Rectangle",
  public = list(
    length = NULL,
    width = NULL,

    # Constructor to initialize the object
    initialize = function(length, width) {
      self$length <- length
      self$width <- width
    },

    # Method to calculate the area
    calculate_area = function() {
      return(self$length * self$width)
    },

    # Method to calculate the perimeter
    calculate_perimeter = function() {
      return(2 * (self$length + self$width))
    }
  )
)

# Creating an object of the Rectangle class
rect <- Rectangle$new(5, 3)
print(rect$calculate_area())       # Output: 15
print(rect$calculate_perimeter())  # Output: 16
```

---

## Classes in the Project

### Example 1: `ValidationError` Class
The `ValidationError` class represents a validation error encountered during data validation. It encapsulates details like the source of the error, the row and column where it occurred, and a descriptive message.

```r
ValidationError <- R6Class("ValidationError",
  public = list(
    source = NULL,
    row = NULL,
    column = NULL,
    message = NULL,

    initialize = function(source, row, column, message) {
      self$source <- source
      self$row <- row
      self$column <- column
      self$message <- message
    },

    to_dataframe_row = function() {
      data.frame(
        Source = self$source,
        Row = self$row,
        Column = self$column,
        Message = self$message,
        stringsAsFactors = FALSE
      )
    }
  )
)

# Example usage
error <- ValidationError$new("Sheet1", 1, "Plot.code", "Invalid data")
print(error$to_dataframe_row())
```

---

### Example 2: `PathGenerator` Class
The `PathGenerator` class generates directory paths and file paths based on plot data.

```r
PathGenerator <- R6Class("PathGenerator",
  public = list(
    base_path = NULL,

    initialize = function(base_path) {
      self$base_path <- base_path
    },

    generate = function(plot_code, sample_date) {
      return(file.path(self$base_path, plot_code, sample_date))
    }
  )
)

# Example usage
path_gen <- PathGenerator$new("/data")
path <- path_gen$generate("Plot1", "2023-05-10")
print(path)  # Output: "/data/Plot1/2023-05-10"
```

---

## Key Concepts in R6 Classes

1. **Public Fields and Methods**:
   - Public fields store data (e.g., `length`, `width`).
   - Public methods define behavior (e.g., `calculate_area()`).

2. **`initialize` Method**:
   - This is the constructor method, called when creating a new object.

3. **Accessing Fields and Methods**:
   - Use `$` to access fields and methods (e.g., `rect$length`, `rect$calculate_area()`).

4. **Encapsulation**:
   - Classes encapsulate data and behavior, making the code modular and reusable.

---

## Why Use Classes?
- **Modularity**: Classes group related data and functions together.
- **Reusability**: Once a class is defined, you can create multiple objects from it.
- **Maintainability**: Classes make the code easier to understand and maintain.

---

## References
- [R6 Documentation](https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html)
- [Object-Oriented Programming in R](https://adv-r.hadley.nz/oo.html)