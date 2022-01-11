'''
The analysis report on gerrymandering doesn’t support the idea of what gerrymandering stands for. Gerrymandering claims to be the process of redrawing legislative
boundaries in order to benefit the party in power. These boundaries are redrawn periodically to make sure that each district has approximately the same population. After
browsing through the data of Florida, Wisconsin, and Minnesota. We zeroed in on the voting results from the 2018 election in Minnesota which has 37% of the republican seats whereas the democrats make up 62% of the seats.  
Concluding the data from Minnesota, it is clear to see that gerrymandering can do more harm than good in swing states. Regardless of which political party
benefits from the gerrymandering procedures, by changing the boundaries of the counties they can sway the political climate of the area. Politics should accurately represent every voter as an individual, and after looking through the data of Minnesota it clearly
states the discriminatory boundaries that are created from gerrymandering. 
'''

def main():
    state = input(str("What state are you interested in? "))
    party = input(str("What party are you interested in? "))
    
    info = filter_by_state(table,state)
    
    print("in " + state + ", the " + party + " party recived:")
    print(round(get_percent_votes(info, party)*100, 2))
    print(round(get_percent_seats(info, party)*100, 2))

def parse_csv(filename):
    f = open(filename)
    table = []
    f.readline()
    for line in f:
        row = line.strip().split(",")
        table.append(row)
    f.close()
    return table

def get_state(row):
    return row[1]
    
def get_party(row):
    return row[11]

def get_votes(row):
    return row[14]

def get_district(row):
    return row[7]

def filter_by_state(table, state):
    table2 = []
    for line in table:
        if get_state(line) == state:
            table2.append(line)
    return table2

def filter_by_party(table1, party):
    table3 = []
    for line in table1:
        if get_party(line) == party:
            table3.append(line)
    return table3

def group_by_district(table):
    dict1 = {}
    for row in table:
        get_dictionary1 = get_district(row)
        if get_dictionary1 not in dict1:
            dict1[get_dictionary1] = [row]
        else:
            val = dict1[get_dictionary1]
            val.append(row)
                
    return dict1

def get_total_votes(table):
    total_votes = 0
    for row in table:
        total_votes = total_votes + int(get_votes(row))
    return total_votes
        
def get_percent_votes(table, party):
    total_votes1 = get_total_votes(table)
    party1 = filter_by_party(table, party)
    final_votes = get_total_votes(party1)
    percent = float(final_votes/total_votes1)
    return percent
    
def find_highest_vote(table):
    best_so_far = ""
    row1 = ""
    for row in table:
        votes1 = get_votes(row)
        if votes1 > best_so_far:
            best_so_far = votes1
            row1 = row
        else:
            pass
    return row1

def get_percent_seats(table, party):
    districts = group_by_district(table)
    party_wins = 0
    for district in districts:
        winning_seat = find_highest_vote(districts[district])
        winning_party = get_party(winning_seat)
        if get_party(winning_seat) == party:
            party_wins = party_wins + 1
    return party_wins / len(districts)

    
main()
    