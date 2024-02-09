# hdtype: Data Analysis Tool in R

## Overview

The **hdtype** package was created in order to provide a different classification of data than the usual machine classification, which classifies data into types such as strings, integers, floats, arrays, among others. Instead, **hdtype** focuses on how humans perceive and understand data. The acronym "hd" originally refers to "Homodatum" which is interpreted as "Data for human", meaning that **hdtype** are data types designed for human comprehension. 

Within the package, users can create objects that are classified into various data classes such as Binary (Bin), Categorical (Cat), Text (txt), Percentage (Pct), among others. In addition, there are several methods for each class, addressing the operation between classes, conversion, and formatting of each object.

## Features

-   Visualization of the list of available classes in the package through the `available_hdtypes()` function, which will display the list with the following classes:
    * Bin (Binary),
    * Cat (Categorical),
    * Cats (Categoricals),
    * Chk (Check),
    * Clr (Color),
    * Dat (Date),
    * Gcd (Geographic Code),
    * Gln (Geographic Longitude),
    * Glt (Geographic Latitude),
    * Gnm (Geographic Name),
    * Img (Image),
    * Num (Numeric),
    * Nums (Numeric Lists),
    * NUT (Null),
    * Pct (Percentage),
    * Seq (Sequential),
    * Txt (Text),
    * Txts (Texts),
    * Uid (Uid),
    * UKT (Unknown),
    * Yea (Year).

-   Creation of specific objects with the data type "hd" for more efficient handling. Functions follow the format of new_Class such as `new_Dat`, `new_Num`, `new_Cat`, etc.

- For each object, methods have been developed to obtain information about the format, coercion and conversion of the different data types. It is important to note that the coercion method is designed to create and structure operations between objects, while conversion provides a more direct way to transform an object from one class to another.

- Obtaining information by means of format (`Class_format`), statistical (`Class_stats`), order (`Class_get_order`) or visualization (`Class_show`) functions.

## Installation

Users can take advantage of **hdtype** functionality by installing the package from the GitHub repository using the following command in R:

```{R eval = FALSE}
devtools::install_github("datasketch/hdtype")
```

## Usage

```

#Load the hdtype package from Github.

library(hdtype)

#Other important packages
library(rlang)
library(tidyverse)
library(vctrs)

```

## Examples

#### hd_Num Object
```

#Creation hd_Num object

num_data <- c(1, 2, 3, NA, 5)
num <- Num(num_data)

# Convert a character type vector to hd_Num
char_vector <- c("1", "2", "3", "NA", "5")
num <- as_Num(char_vector)

# Get statistics from an hd_Num object
summary_stats <- Num_stats(num)
print(summary_stats)

```

#### hd_Bin Object

```
#Creation hd_Bin object

bin_data <- c("Yes", "No", "Yes", "Yes", "No")
bin <- Bin(bin_data)

# Convert a character type vector to hd_Bin
bin_vector <- c("Yes", "No", "Yes", "No", "Yes")
bin <- as_Bin(bin_vector)

#Show the categories of the object
categories <- Bin_get_categories(bin)
print(categories)

#Show number of categories of the object
n_categories <- Bin_get_n_categories(bin)
print(n_categories)


```

#### hd_Dat Object 

```
# Creation hd_Dat object
date_data <- c("2022-01-01", "2022-01-15", "2022-02-01", NA, "2022-02-15")
date <- Dat(date_data)


# Check if the date object is hd_Dat type
is_Dat(date)  

# Get the format of the date object
format <- Dat_format(date)
print(format)


```

#### hd_Seq Object

```

datos_secuenciales_char <- c("A", "B", "A", "C", "B", "C")

# Conversion of the vector to an object of type "Seq".
datos_seq <- Seq(datos_secuenciales_char)
print(class(datos_seq))

# Obtain categories
categorias <- hdtype:::Seq_get_categories(datos_seq)
print(categorias)

# Obtain the number of categories
n_categorias <- hdtype:::Seq_get_n_categories(datos_seq)
print(n_categorias)

#Get statistics from an hd_Num object
estadisticas <- Seq_stats(datos_seq)
print(estadisticas)

# Conversion of the object from Seq to character
datos_seq_a_char <- as.character(datos_seq)

# Class verification
print(class(datos_seq_a_char))

# Format
datos_seq_formateado <- format(datos_seq)
print(datos_seq_formateado)

```
