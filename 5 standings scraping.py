from pyquery import PyQuery as pq
import pandas as pd

league0 = []
club0 = []
rank0 = []

def parse_one_page(doc):
    items = doc('.standing-table__table').items()
    for item in items:
        clubs = item('.standing-table__cell--name').items()
        ranks = item('.standing-table__row').items()
        for rank in ranks:
            r = rank('.standing-table__cell').text()
            rank0.append(r)
        length = 0
        for club in clubs:
            c = club.text()
            length = length + 1
            club0.append(c)
        for i in range(length):
            league = item('.standing-table__caption').text()
            league0.append(league)



url = 'https://www.skysports.com/football/tables'
doc = pq(url)
parse_one_page(doc)


df = pd.DataFrame({'league': league0, 'club': club0, 'rank': rank0})

df.to_csv("league2.csv", index = False, sep = ',')