import mysql.connector
import csv

mycon = mysql.connector.connect(host="localhost", user="root", password="geethumithu", database="cinescope_db")
cursor = mycon.cursor()

with open("movies.csv", encoding="utf-8", newline="") as fh:
    reader = csv.DictReader(fh)
    for row in reader:
        released_year = int(row["Released_Year"]) if row["Released_Year"] else 0
        imdb_rating = float(row["IMDB_Rating"]) if row["IMDB_Rating"] else 0.0
        query = "INSERT INTO movies (Series_Title, Released_Year, Genre, IMDB_Rating, Director, Star1, Star2, Star3) VALUES (%s, %s, %s, %s, %s, %s, %s, %s)"
        values = (row["Series_Title"], released_year, row["Genre"], imdb_rating, row["Director"], row["Star1"], row["Star2"], row["Star3"])
        cursor.execute(query, values)

mycon.commit()
cursor.close()
mycon.close()
