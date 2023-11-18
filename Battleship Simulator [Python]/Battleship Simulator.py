# -*- coding: utf-8 -*-
"""
Created on Sun Mar 19 12:36:22 2023

@author: Tom.Midgley
"""


# Hi Tim! Welcome to my Battleship code!
# I'm new to coding and I really enjoyed the challenge; I defintley learned a lot doing this.
# I'm sure there's many better ways to do things and keen to hear any feedback. I feel my code is quite long winded and could be abbreviated with more skill/experience.

# The design of this game is based around two lists: one to track the placement of the ships (ship_location) and the other to track the players progress (board).
# The game loads the ships into the ship list, coordinates input are checked against that list and the player list ('board') is updated with the hit/miss result.
# This is perhaps overkill for a game of Battleship where there is no player, as this design is better suited to someone 'playing' it, however, this concept
# really helped me get to grips with assignment in the design phase.

# A few things I noted:
    # I used a lot of global lists/variables in my functions. I know this is not recommended but not sure how I could have avoided it. Perhaps with fewer, larger functions maybe?
    # Use of While loops: again, you said these are not recommended but I hope their use here was justified/appropriate
    
# Verbiage:
    # shots = the number of shots taken in a game (i.e. 'rounds', 'goes' etc)
    # games = a game is played until all ships are sunk
    
# Game functions
# I've created 3 functions to play the game:
    #play_game1 = very basic function that selects random board coordinates and checks if they've been used before
    #play_game2 = based on play_game1 but with extra steps to make use of last known hit coordinates. Doesn't work too well, similar results to play_game1. 
        #I can see a lot of scope for additional logical steps to make more use of this concept, however I'm running out of time... and skill :)
    #play_game_seq = selects all cells in sequence e.g. A1, A2, A3 etc. My best performing algorithm but hardly 'smart'
        #script will run using that one but feel free to try the others
        
#code limitations
    #logic for game play isn't very smart. Best I could do was >80 guesses
    #code seems long and complex: 
        # my design is based on functionality not needed (i.e. to 'play' the game as a human and see the board)
        # probably more neat and succinct ways to code some of the functions to reduce the number of them
        #  use of while loops and global variables is risky and not best practice, potential for bugs/breaks

import random
import statistics as stats

#~~~~~~~~Game Options~~~~~~~~
board_size = 10     ## size of the square board e.g. 10x10
des_size = 2        ## size of the destroyer. Default = 2
sub_size = 3        ## size of the submarine. Default = 3
cru_size = 3        ## size of the cruiser. Default = 3
bat_size = 4        ## size of the battleship. Default = 4
car_size = 5        ## size of the carrier. Default = 5
games_sim = 1000    ##number of games to simulate

#~~~~~~~~lists~~~~~~~~
board = []          ##this is the game board that the player sees
ship_location = []   ##this is a ship specific board that tracks each ships location
shots_taken = []         ##number of shots taken in each simulation. Each simulations' shot count is appended to this list

#~~~~~~~~Global Variables~~~~~~~~

games_played = 0    


#~~~~~~~~user defined functions~~~~~~~~

def create_board(board):             ##creates the player and ship board
    for i in range(board_size):
        board.append(["[__]"] * (board_size)) ##creates '10x10' list
      
def print_board():              ##prints the board in a readable format in the console (shows shots taken)
    for i in range (0,10):
        print(board[0+i])           
       
def print_ship_location():              ##prints the ship locations in a readable format in the console (shows prepared board)
    for i in range (0,10):
        print(ship_location[0+i])
        
def place_des():                ##creates and place the destroyer on the shipboard
    orients = ['horizontal', 'vertical']                    ##potential ship orientations
    des_row = random.randint(0, (board_size - des_size))    ##assigns row location. -size included to prevent clipping off board
    des_col = random.randint(0, (board_size - des_size))    ##assigns column location. -size included to prevent clipping off board
    des_orient = random.choice(orients)                     ##randomly chooses a ship orientation
    for i in range(des_size):                               ##adds additional ship locations based on original location, ship size and oritentation
        if des_orient == 'horizontal':
            ship_location[des_row][des_col + i] = "[De]"
        else:
            ship_location[des_row + i][des_col] = "[De]"   
            
def place_sub():                ##creates and place the destroyer on the shipboard
    orients = ['horizontal', 'vertical']                    ##potential ship orientations
    sub_row = random.randint(0, (board_size - sub_size))    ##assigns row location. -size included to prevent clipping off board
    sub_col = random.randint(0, (board_size - sub_size))    ##assigns column location. -size included to prevent clipping off board
    sub_orient = random.choice(orients)                     ##randomly chooses a ship orientation
    for i in range(sub_size):                               ##adds additional ship locations based on original location, ship size and oritentation
        if sub_orient == 'horizontal':
            ship_location[sub_row][sub_col + i] = "[Su]"
        else:
            ship_location[sub_row + i][sub_col] = "[Su]"      
    
def place_cru():                ##creates and place the destroyer on the shipboard
    orients = ['horizontal', 'vertical']                    ##potential ship orientations
    cru_row = random.randint(0, (board_size - cru_size))    ##assigns row location. -size included to prevent clipping off board
    cru_col = random.randint(0, (board_size - cru_size))    ##assigns column location. -size included to prevent clipping off board
    cru_orient = random.choice(orients)                     ##randomly chooses a ship orientation
    for i in range(cru_size):                               ##adds additional ship locations based on original location, ship size and oritentation
        if cru_orient == 'horizontal':
            ship_location[cru_row][cru_col + i] = "[Cr]"
        else:
            ship_location[cru_row + i][cru_col] = "[Cr]"      

def place_bat():                ##creates and place the destroyer on the shipboard
    orients = ['horizontal', 'vertical']                    ##potential ship orientations
    bat_row = random.randint(0, (board_size - bat_size))    ##assigns row location. -size included to prevent clipping off board
    bat_col = random.randint(0, (board_size - bat_size))    ##assigns column location. -size included to prevent clipping off board
    bat_orient = random.choice(orients)                     ##randomly chooses a ship orientation
    for i in range(bat_size):                               ##adds additional ship locations based on original location, ship size and oritentation
        if bat_orient == 'horizontal':
            ship_location[bat_row][bat_col + i] = "[Ba]"
        else:
            ship_location[bat_row + i][bat_col] = "[Ba]"      

def place_car():                ##creates and place the destroyer on the shipboard
    orients = ['horizontal', 'vertical']                    ##potential ship orientations
    car_row = random.randint(0, (board_size - car_size))    ##assigns row location. -size included to prevent clipping off board
    car_col = random.randint(0, (board_size - car_size))    ##assigns column location. -size included to prevent clipping off board
    car_orient = random.choice(orients)                     ##randomly chooses a ship orientation
    for i in range(car_size):                               ##adds additional ship locations based on original location, ship size and oritentation
        if car_orient == 'horizontal':
            ship_location[car_row][car_col + i] = "[Ca]"
        else:
            ship_location[car_row + i][car_col] = "[Ca]"  
                     
def ship_count_test():          ##test to check whether the correct number of ships is on the board i.e. no overlapping occurred
    ship_total = des_size + sub_size + cru_size + bat_size + car_size  ##total ships on the board, based on design
    ship_count = 0                                                      ##empty list 
    global ship_location                                                ##loads the ship location list from global memory
    for ships in ship_location:                                         ##counts the list for ships
        ship_count += ships.count('[De]')
        ship_count += ships.count('[Su]')
        ship_count += ships.count('[Cr]')
        ship_count += ships.count('[Ba]')
        ship_count += ships.count('[Ca]')
    if ship_count == ship_total:                                         ##enough ships on the board? e.g. 17/17?
        return True
    else:
        return False
       

def board_setup():              ##sets up a fresh board for play
    global board                ##make use of global 'player' board
    global ship_location        ##make use of global ship board
    board = []                  ##makes blank list
    ship_location = []          ##makes blank list
    while ship_count_test() == False:   ##while loop that runs until ship count test returns positive i.e. ships added with no overlap
        board = []
        ship_location = []
        create_board(board)
        create_board(ship_location)
        place_des()
        place_sub()
        place_cru()
        place_bat()
        place_car()
    

   
def play_game1():                ##plays a single game to completion and increases the round counter    
    hit_count = 0                ##number of times hit have been achieved this game
    miss_count = 0               ##number of misses. Used to calculate the below
    total_shots = 0              ##Total number of shots taken to complete the game
    global games_played
    global shots_taken
    while hit_count < des_size + sub_size + cru_size + bat_size + car_size:     ##While loop to play the game until all ships hit
        guess_row = random.randint(0,9)                                         ##random guesses for row
        guess_col = random.randint(0,9)                                         ##random guess for column
        if board[guess_row][guess_col] != "[__]":                               ##test to check the player board if row/column combo used previously (Will be X or O if so)
            continue                                                            ##start loop again if shot coordinates already used
        elif ship_location[guess_row][guess_col] != "[__]":                     ##test to check if the coordinates have a ship in them
            board[guess_row][guess_col] = "[X ]"                                ##marks the player board as a hit
            hit_count = sum(sublist.count('[X ]') for sublist in board)         ##counts the hits/X's
            total_shots = hit_count + miss_count                                ##updates total shots
        else:
            board[guess_row][guess_col] = "[O ]"
            miss_count = sum(sublist.count('[O ]') for sublist in board) 
            total_shots = hit_count + miss_count
    else:                                                                       ##once all ships are hit and game is finished:
        games_played = games_played +1                                          ##updates the global list games played (for simulation purposes)
        shots_taken.append(total_shots)                                         ##updates the shots_taken list with the total shots taken that game

      
def play_game2():                ##plays a single game to completion and increases the round counter    
    hit_count = 0                ##number of times hit have been achieved this game       
    miss_count = 0               ##number of misses. Used to calculate the below
    total_shots = 0             ##Total number of shots taken to complete the game
    global games_played
    global shots_taken
    while hit_count < des_size + sub_size + cru_size + bat_size + car_size:     ##while loop to play games until all ships hit
        guess_row = random.randint(0,9)
        guess_col = random.randint(0,9)
        if board[guess_row][guess_col] != "[__]":                               ##test to check the player board first to see if coordinates already targeted. Restarts loop if so
            continue
        elif ship_location[guess_row][guess_col] != "[__]":                     ##if shiplocation coordinates are not blank, score a hit
            board[guess_row][guess_col] = "[X ]"                                ##marks shipboard with a hit
            hit_count = sum(sublist.count('[X ]') for sublist in board)         ##counts a hit
            total_shots = hit_count + miss_count                                ##counts total shots
            last_hit_row = guess_row                                            ##loads last hit coordinates into memory
            last_hit_col = guess_col
            if last_hit_row == 9:                                               ##code that bases the next row coordinate on the last hit row coordinate. Not sure this really works as hit count still high
                guess_row = last_hit_row -1                                     ## -1 included on last row to make sure didnt go out of the board
                guess_col = last_hit_col
                if board[guess_row][guess_col] != "[__]":                       ##previous coordinate check
                    continue
                elif ship_location[guess_row][guess_col] != "[__]":
                    board[guess_row][guess_col] = "[X ]"
                    hit_count = sum(sublist.count('[X ]') for sublist in board)  
                    total_shots = hit_count + miss_count
                else:
                    board[guess_row][guess_col] = "[O ]"
                    miss_count = sum(sublist.count('[O ]') for sublist in board) 
                    total_shots = hit_count + miss_count
            elif last_hit_row == 0:                                             ##more code to base next shot over previous hits
                guess_row = last_hit_row +1
                guess_col = last_hit_col
                if board[guess_row][guess_col] != "[__]":
                    continue
                elif ship_location[guess_row][guess_col] != "[__]":
                    board[guess_row][guess_col] = "[X ]"
                    hit_count = sum(sublist.count('[X ]') for sublist in board)  
                    total_shots = hit_count + miss_count
                else:
                    board[guess_row][guess_col] = "[O ]"
                    miss_count = sum(sublist.count('[O ]') for sublist in board) 
                    total_shots = hit_count + miss_count
            elif last_hit_row == range(2,8):                                    ##more code to base next shot over previous hits
                guess_row = last_hit_row +1
                guess_col = last_hit_col
                if board[guess_row][guess_col] != "[__]":
                    continue
                elif ship_location[guess_row][guess_col] != "[__]":
                    board[guess_row][guess_col] = "[X ]"
                    hit_count = sum(sublist.count('[X ]') for sublist in board)  
                    total_shots = hit_count + miss_count
                else:
                    board[guess_row][guess_col] = "[O ]"
                    miss_count = sum(sublist.count('[O ]') for sublist in board) 
                    total_shots = hit_count + miss_count           
        else:
            board[guess_row][guess_col] = "[O ]"
            miss_count = sum(sublist.count('[O ]') for sublist in board) 
            total_shots = hit_count + miss_count
    else:                                                                       ##once all ships are hit and game is finished:
        games_played = games_played +1                                          ##updates the global list games played (for simulation purposes)
        shots_taken.append(total_shots)                                         ##updates the shots_taken list with the total shots taken that game

            
def play_game_seq():                ##plays a single game to completion and increases the round counter    
    hit_count = 0               ##counts the number of times a ship is hit
    miss_count = 0 
    total_shots = 0
    guess_row = 0
    guess_col = 0
    global games_played
    global shots_taken
    while hit_count < des_size + sub_size + cru_size + bat_size + car_size:     ##while loop to play until all ships have been hit
        if ship_location[guess_row][guess_col] != "[__]":                       ##If location in ship list is anything other than blank i.e. has a ship in it
            board[guess_row][guess_col] = "[X ]"                                ##replaces blank space with an X
            hit_count = sum(sublist.count('[X ]') for sublist in board)  
            total_shots = hit_count + miss_count
            guess_col = guess_col + 1
        else:
            board[guess_row][guess_col] = "[O ]"
            miss_count = sum(sublist.count('[O ]') for sublist in board) 
            total_shots = hit_count + miss_count
            guess_col = guess_col + 1
        if guess_col == board_size:
                guess_row = guess_row +1
                guess_col = 0
    else:
        games_played = games_played +1
        shots_taken.append(total_shots)
            
    
##Welcome

print("Hello! Welcome to Tom's Battleship Simulator")
print("Game options like board size and number of rounds can be adjusted in the code")
 

while games_played < games_sim:
    board = []
    ship_location = []
    create_board(board)
    create_board(ship_location)
    board_setup()
    play_game_seq()
    
avg = round(sum(shots_taken) / len(shots_taken), 1)  
stdev = round(stats.stdev(shots_taken),1)
cv = round(stdev/avg*100, 1)
min_shots = min(shots_taken)
max_shots = max(shots_taken)

print("You have just simulated %d games of Battleships!" % games_sim)
print("Average number of shots per game was:", avg)
print("The standard deviation was:", stdev)
print("And the precision (%CV) was:", cv)
print("The lowest number of shots taken was:", min_shots)
print("The highest number of shots taken was:", max_shots)
