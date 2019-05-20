from pyquery import PyQuery as pq
import pandas as pd

rank0 = []
prev0 = []
coach0 = []
club0 = []
point0 = []
won0 = []
lost0 = []

def parse_one_page(doc):
    items = doc('.col-md-12 tr').items()
    for item in items:
        rank = item('tr td .rank').text()
        rank0.append(rank)
        prev = item('tr td .prev').text()
        prev0.append(prev)
        coach = item('tr td .col-name').text()
        coach0.append(coach)
        club = item('tr td .col-secundair.hidden-xs').text()
        club0.append(club)
        point = item('tr td .points').text()
        point0.append(point)
        won = item('tr td .won').text()
        won0.append(won)
        lost = item('tr td .lost').text()
        lost0.append(lost)


for i in range(39):
    url = 'https://www.clubworldranking.com/ranking-coaches?wd=16&yr=2019&index=' + str(25*i)
    doc = pq(url)
    parse_one_page(doc)
    print(i)

df=pd.DataFrame({'rank':rank0,'prev':prev0,'coach':coach0,'club':club0,'point':point0,'won':won0,'lost':lost0})

df.to_csv("coach.csv",index=False,sep=',')