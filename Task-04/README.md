
#CineScope – Task Write-Up 

In this task, I created a movie dashboard using Python (PySide6) and a MySQL database. The dashboard lets users search movies by genre, year, rating, director, or actor. It also has options to choose which columns (like Title, Year, Genre, Rating, Director, Stars) to display in the results.

The app has two sides:

On the left side, there are search buttons, column selectors, and actions like Search and Export CSV.

On the right side, the results show in a table, and below that, a small console gives feedback (like “Results Found” or “No Results Found”).

From this task, I learned how to:

Build a GUI with PySide6.

Connect Python with MySQL.

Run SQL queries and show results in a table.

Add useful features like search filters and exporting data.


In short, this project is like a mini IMDB tool where users can search and explore movies easily.

##A brief overview

    ## Code Explanation of dashboard.py

1. Imports & Setup

The code uses PySide6 for GUI, mysql.connector for database connection, and csv for exporting results.

The Dashboard class is the main window of the app, with a title “CineScope – Dashboard” and dark theme styling.





---

2. UI Layout (init_ui)

The window is split into two panels:

Left Panel → search buttons, column selectors, input box, and action buttons.

Right Panel → results table and an output console.


Example: The header is created using QLabel, buttons with QPushButton, and results table with QTableWidget.





---

3. Search Modes (set_search_mode)

Buttons like Genre, Year, Rating, Director, Actor set the type of search.

When you click one, the search mode changes, and the input box placeholder updates (e.g., “Enter Genre”).





---

4. Column Selection (toggle_column)

Users can pick which columns to display (Title, Year, Genre, Rating, etc.).

If “Stars” is selected, it actually adds Star1, Star2, Star3 to the selection.





---

5. Execute Search (execute_search)

When Search is clicked:

It connects to the MySQL database.

Builds a SQL query based on search mode and selected columns.

Runs the query and fetches results.

Displays them in the table.


Example: If search mode is “director” and you type Nolan, it runs:

SELECT Title, Year, Genre FROM movies WHERE Director LIKE '%Nolan%';





---

6. Export CSV (export_csv)

Reads the data currently in the table.

Writes it into a CSV file (at /home/.../csvexport.csv).

Console shows a success message after export.





---

7. Load Movies on Startup (load_movies_from_db)

When the app opens, it loads all movies from the database by default.

Shows columns: Id, Title, Year, Genre, Rating, Director, Star1, Star2, Star3.





---

8. Main Function

At the bottom:

if __name__ == "__main__":
    app = QApplication(sys.argv)
    dashboard = Dashboard()
    dashboard.show()
    sys.exit(app.exec())

This launches the CineScope Dashboard window.
