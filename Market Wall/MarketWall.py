from bs4 import BeautifulSoup
import urllib.request
import sys
import math
from PyQt5.QtGui import *
from PyQt5.QtWidgets import *

class MainWindow(QWidget):
    data = []
    companies = []

    cols = 14
    rows = 14
    cellsize = 30
    pages = math.ceil(cols * rows / 15)  # 15 is the number of companies per page

    if pages > 128:
        pages = 128

    def __init__(self):
        super().__init__()

        self.setGeometry(0, 0, self.cols * self.cellsize, self.rows * self.cellsize)
        self.setWindowTitle("Market Wall - NYSE")
        self.show()
        self.getdata()

    def getdata(self):
        # Get data from https://money.cnn.com
        for i in range(0, self.pages):
            print("Getting page {0}/{1}".format(i + 1, self.pages))
            # Get HTML
            html = urllib.request.urlopen("https://money.cnn.com/data/markets/nyse/?page=" + str(i + 1))

            # BeautifulSoup
            soup = BeautifulSoup(html.read(), "html.parser")

            table = soup.find_all("div", {'class': 'wsod_dataTableBorder'})[1].find("table").find("tbody").find_all(
                "tr")

            for stock in table:
                company = stock.find_all('td')[0].text
                change = stock.find_all('td')[2].text
                print("{0} {1}".format(company, change))
                self.data.append(change)
                self.companies.append(company)

    def paintEvent(self, QPaintEvent):
        qp = QPainter()
        qp.begin(self)

        x = 0
        y = 0

        for i in range(0, len(self.data)):
            change = self.data[i]

            # Stupid Americans, use commas instead of spacing.
            if ',' in change:
                change = change.replace(',', '')

                # Use green/red/black if positive/negative/neutral
            if float(change) > 0.0:
                qp.setBrush(QColor(0, 255, 0))
            elif float(change) < 0.0:
                qp.setBrush(QColor(255, 0, 0))
            else:
                qp.setBrush(QColor(0, 0, 0))

            if math.fabs(float(change)) > 5:
                print("Large change in {1}, change of {0}".format(str(change), self.companies[i]))

            qp.drawRect(x * self.cellsize, y * self.cellsize, self.cellsize, self.cellsize)

            x += 1
            if x == self.cols:
                x = 0
                y += 1
        qp.end()


if __name__ == '__main__':
    # Display posCount amount of green squares and negCount amount of red squares.

    app = QApplication(sys.argv)
    app.setApplicationName("Market Wall")

    window = MainWindow()
    sys.exit(app.exec_())
