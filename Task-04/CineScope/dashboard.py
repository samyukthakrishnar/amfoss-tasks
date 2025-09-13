import sys
from PySide6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QLabel,
    QPushButton, QTableWidget, QTableWidgetItem, QGridLayout, 
    QTextEdit, QSizePolicy, QLineEdit
)
from PySide6.QtGui import QFont
from PySide6.QtCore import Qt

class Dashboard(QWidget):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("CineScope – Dashboard")
        self.setMinimumSize(1200, 800)
        self.setStyleSheet("background-color: #121212; color: white; padding: 20px;")
        self.init_ui()
        self.search_mode = None
        self.selected_columns = []


    def init_ui(self):
        main_layout = QVBoxLayout()
        main_layout.setContentsMargins(20, 20, 20, 20)
        main_layout.setSpacing(10)

        # Header
        header = QLabel("🎬 CineScope Dashboard")
        header.setFont(QFont("Arial", 24, QFont.Bold))
        header.setAlignment(Qt.AlignCenter)
        header.setFixedHeight(80)
        main_layout.addWidget(header)

        split_layout = QHBoxLayout()

        # Left Panel
        left_container = QVBoxLayout()
        left_container.setSpacing(10)
        left_container.setAlignment(Qt.AlignTop)

        # Search buttons
        search_heading = QLabel("Search By")
        search_heading.setFont(QFont("Arial", 18, QFont.Bold))
        left_container.addWidget(search_heading)

        search_buttons = [
            ("Genre", "genre"),
            ("Year", "year"),
            ("Rating", "rating"),
            ("Director", "director"),
            ("Actor", "actor"),
        ]

        search_grid = QGridLayout()
        for index, (label, mode) in enumerate(search_buttons):
            btn = QPushButton(label)
            btn.setStyleSheet(self.get_button_style(False))
            btn.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
            btn.clicked.connect(lambda _, m=mode: self.set_search_mode(m))
            row, col = divmod(index, 2)
            search_grid.addWidget(btn, row, col)
        left_container.addLayout(search_grid)

        # Column selection
        column_heading = QLabel("Select Columns")
        column_heading.setFont(QFont("Arial", 18, QFont.Bold))
        left_container.addWidget(column_heading)

        column_buttons = [
            ("Title", "Series_Title"),
            ("Year", "Released_Year"),
            ("Genre", "Genre"),
            ("Rating", "IMDB_Rating"),
            ("Director", "Director"),
            ("Stars", "Stars"),
        ]

        column_grid = QGridLayout()
        for index, (label, col) in enumerate(column_buttons):
            btn = QPushButton(label)
            btn.setStyleSheet(self.get_button_style(False))
            btn.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
            btn.clicked.connect(lambda _, c=col: self.toggle_column(c))
            row, col = divmod(index, 2)
            column_grid.addWidget(btn, row, col)
        left_container.addLayout(column_grid)

        # Search input
        self.query_input = QLineEdit()
        self.query_input.setPlaceholderText("Enter search term")
        self.query_input.setStyleSheet("background-color: #1e1e1e; color: white; padding: 5px; border: 1px solid #444;")
        left_container.addWidget(self.query_input)

        # Action buttons
        action_layout = QHBoxLayout()
        search_btn = QPushButton("Search")
        search_btn.setStyleSheet("background-color: #e50914; color: white; padding: 6px; border-radius: 5px;")
        search_btn.clicked.connect(self.execute_search)
        action_layout.addWidget(search_btn)

        export_btn = QPushButton("Export CSV")
        export_btn.setStyleSheet("background-color: #1f1f1f; color: white; padding: 6px; border-radius: 5px;")
        export_btn.clicked.connect(self.export_csv)
        action_layout.addWidget(export_btn)
        left_container.addLayout(action_layout)

        # Right Panel
        right_side_layout = QVBoxLayout()
        right_side_layout.setSpacing(10)

        # Table
        self.table = QTableWidget()
        self.table.setStyleSheet("""
            QTableWidget {
                color: white;
                font-family: Arial, sans-serif;
                font-size: 14px;
            }
            QHeaderView::section {
                background-color: white;
                color: black;
                padding: 4px;
            }
        """)
        self.table.horizontalHeader().setStretchLastSection(True)
        self.table.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        # Output console
        self.output_console = QTextEdit()
        self.output_console.setPlaceholderText("Results will appear here...")
        self.output_console.setStyleSheet("""
            QTextEdit {
                background-color: #1e1e1e;
                color: white;
                border: 1px solid #444;
                padding: 5px;
            }
        """)
        self.output_console.setFixedHeight(100)

        right_side_layout.addWidget(self.table)
        right_side_layout.addWidget(self.output_console)

        split_layout.addLayout(left_container, 2)
        split_layout.addLayout(right_side_layout, 8)
        main_layout.addLayout(split_layout)
        self.setLayout(main_layout)


        self.load_movies_from_db()


    def get_button_style(self, is_selected):
        if is_selected:
            return """
                QPushButton {
                    background-color: #ffcc00;
                    border: 1px solid #ff9900;
                    border-radius: 3px;
                    padding: 6px;
                }
            """
        else:
            return """
                QPushButton {
                    background-color: #1f1f1f;
                    border: 1px solid #333;
                    border-radius: 3px;
                    padding: 6px;
                }
                QPushButton:hover {
                    background-color: #333;
                }
            """

    def set_search_mode(self, mode): 
        self.search_mode = mode
        placeholder_map = {
        "genre": "Enter Genre",
        "year": "Enter Year",
        "rating": "Enter Rating",
        "director": "Enter Director",
        "actor": "Enter Actor"
        }
        placeholder = placeholder_map.get(mode, "Enter search term")
        self.query_input.setPlaceholderText(placeholder)
        self.output_console.append(f"Search mode set to {mode}")

    def toggle_column(self, column):
        if column == "Stars":
            for col in ["Star1", "Star2", "Star3"]:
                if col not in self.selected_columns:
                    self.selected_columns.append(col)
                    self.output_console.append(f"{col} selected")
                else:
                    self.selected_columns.remove(col)
                    self.output_console.append(f"{col} deselected")
        else:
            if column in self.selected_columns:
                self.selected_columns.remove(column)
                self.output_console.append(f"{column} deselected")
            else:
                self.selected_columns.append(column)
                self.output_console.append(f"{column} selected")


    def execute_search(self):
        import mysql.connector
        search_text = self.query_input.text().strip()

        if not search_text:
            self.output_console.append("Please enter something to search !")
            return

        mycon = mysql.connector.connect(host="localhost", user="root", password="geethumithu", database="cinescope_db")
        cursor = mycon.cursor()
        columns = ",".join(self.selected_columns) if self.selected_columns else "*"
        if self.search_mode == "genre":
            query = f"select {columns} from movies where Genre like %s"
            values = ("%" + search_text + "%",)
        elif self.search_mode == "year":
            query = f"select {columns} from movies where Released_Year like %s"
            values = ("%" + search_text + "%",)
        elif self.search_mode == "rating":
            query = f"select {columns} from movies where IMDB_Rating like %s"
            values = ("%" + search_text + "%",)
        elif self.search_mode == "director":
            query = f"select {columns} from movies where Director like %s"
            values = ("%" + search_text + "%",)
        elif self.search_mode == "actor":
            query = f"select {columns} from movies where Star1 like %s or Star2 like %s or Star3 like %s"
            values = (f"%{search_text}%", f"%{search_text}%", f"%{search_text}%",)
        else:
            query = "select * from movies"
            values = ()
        
        cursor.execute(query, values)
        results = cursor.fetchall()
        
        if len(results) == 0:
            self.output_console.append("No Results Found !")
        else:
            self.output_console.append("Results Found !")   
            self.table.setRowCount(len(results))
            self.table.setColumnCount(len(self.selected_columns))
            self.table.setHorizontalHeaderLabels(self.selected_columns)

            for i, movie in enumerate(results):
                for j, value in enumerate(movie):
                    self.table.setItem(i, j, QTableWidgetItem(str(value)))



            cursor.close()
            mycon.close()


    def export_csv(self):
        import csv 
        row_count = self.table.rowCount()
        col_count = self.table.columnCount()

        headers = []
        for i in range(col_count):
            headers.append(self.table.horizontalHeaderItem(i).text())

        with open("/home/samyuktha-krishna-r/Downloads/csvexport.csv", "w", newline="", encoding="utf-8") as fh:
            writer = csv.writer(fh)
            writer.writerow(headers)
            for row in range(row_count):
                row_data = []
                for col in range(col_count):
                    item = self.table.item(row, col)
                    row_data.append(item.text() if item else "")
                writer.writerow(row_data)

        self.output_console.append("Data Exported Successfully: /home/samyuktha-krishna-r/Downloads/csvexport.csv")




    def load_movies_from_db(self):
        import mysql.connector
        mycon = mysql.connector.connect(host="localhost", user="root", password="geethumithu", database="cinescope_db")
        cursor = mycon.cursor()

        cursor.execute("SELECT Id, Series_Title, Released_Year, Genre, IMDB_Rating, Director, Star1, Star2, Star3 FROM movies")
        movies = cursor.fetchall()

        self.table.setRowCount(len(movies))
        self.table.setColumnCount(9)
        self.table.setHorizontalHeaderLabels(["Id", "Title", "Year", "Genre", "Rating", "Director", "Star1", "Star2", "Star3"])


        for i, movie in enumerate(movies):
            self.table.setItem(i, 0, QTableWidgetItem(str(movie[0])))
            self.table.setItem(i, 1, QTableWidgetItem(str(movie[1])))
            self.table.setItem(i, 2, QTableWidgetItem(str(movie[2])))
            self.table.setItem(i, 3, QTableWidgetItem(str(movie[3])))
            self.table.setItem(i, 4, QTableWidgetItem(str(movie[4])))
            self.table.setItem(i, 5, QTableWidgetItem(str(movie[5])))
            self.table.setItem(i, 6, QTableWidgetItem(str(movie[6])))
            self.table.setItem(i, 7, QTableWidgetItem(str(movie[7])))
            self.table.setItem(i, 8, QTableWidgetItem(str(movie[8])))

        cursor.close()
        mycon.close()











if __name__ == "__main__":
    app = QApplication(sys.argv)
    dashboard = Dashboard()
    dashboard.show()
    sys.exit(app.exec())
