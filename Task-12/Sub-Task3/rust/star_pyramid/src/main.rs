use std::fs;
use std::io;
use std::num::ParseIntError;

// This function reads the content of a file, trims whitespace, and parses it into a u32.
// It returns a Result, handling both file I/O and parsing errors.
fn read_rows_from_file(file_path: &str) -> Result<u32, Box<dyn std::error::Error>> {
    let contents = fs::read_to_string(file_path)
        .map_err(|e| format!("Error reading file: {}", e))?; // Convert to String error

    let rows: u32 = contents
        .trim()
        .parse()
        .map_err(|e: ParseIntError| format!("Error parsing number: {}", e))?; // Convert to String error

    Ok(rows)
}

// This function prints the hollow star pyramid based on the number of rows.
fn print_hollow_pyramid(rows: u32) {
    // Outer loop for each row
    for i in 1..=rows {
        // Print leading spaces for centering
        for _ in 0..(rows - i) {
            print!(" ");
        }

        // Inner loop to print stars and inner spaces
        for j in 1..=(2 * i - 1) {
            // Print a star if it's the first column, last column, or the very last row
            if j == 1 || j == 2 * i - 1 || i == rows {
                print!("*");
            } else {
                // Otherwise, print a space
                print!(" ");
            }
        }
        // Move to the next line after each row is complete
        println!();
    }
}

// The main entry point of the program
fn main() {
    let file_path = "input.txt";

    // Call the function to read the number of rows from the file
    match read_rows_from_file(file_path) {
        Ok(rows) => {
            println!("Creating a hollow pyramid with {} rows:", rows);
            print_hollow_pyramid(rows);
        }
        Err(e) => {
            // If an error occurred, print the error message
            eprintln!("Failed to process input: {}", e);
            eprintln!("Please ensure 'input.txt' exists and contains a single valid integer.");
        }
    }
}
