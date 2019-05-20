from pyquery import PyQuery as pq
import pandas as pd

league0 = []
club0 = []


def parse_one_page(doc):
    items = doc('.category-list__item.accordian__item').items()
    for item in items:
        length = 0
        clubs = item('div ul li a').items()
        for club in clubs:
            c = club.text()
            club0.append(c)
            length = length+1
        for i in range(length):
            league = item('.category-list__item.accordian__item h4').text()
            league0.append(league)



url = 'https://www.skysports.com/football/teams'
doc = pq(url)
parse_one_page(doc)


df = pd.DataFrame({'league': league0, 'club': club0})

df.to_csv("league.csv", index = False, sep = ',')