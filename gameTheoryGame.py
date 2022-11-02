# name: Mennatullah Ahmed hamdy awadallah
# email: m.awadallah@innopolis.university
# My birthday is 24.02.2003.
# So, day + months -> 26 (max move at a time).
# And day + months + year -> 2029 (map size).

import random
import logging

day = 24
month = 2
year = 2003
logging.basicConfig(filename='GameLogs.txt', filemode='a',
                    format='%(message)s', level=logging.INFO)

# Interacts with the user, knowing the position they're in, to get a valid move.
def get_user_play(position, computer_strategy):

    # Give the user advice before they play if they've indicated they want it.
    if (computer_strategy == 3):
        advisor_play()

    limit = min( (year + month + day - position), (day + month) )
    print("Please enter the number of steps you'd like to take. [1," + str(limit) + "]\n")
    count = 0
    while True:
        count += 1
        x = input()
        x = int(x)
        if isinstance(x, int) and x > 0 and x < (limit+1):
            return x
        elif count > 5:
            print("You've failed too many times, try again later.")
            exit(0)
        else:
            print("You've entered an invalid input, please try again within the range of [1:" + str(limit) + "]\n")

# Gets a valid random move for the computer, knowing the position it's in.
def random_play(position):
    x = random.randint(1,(day + month))
    return min(x, (year + day + month - position) )

# Applies a smart strategy to get a valid winning move for the computer, knowing the position it's in.
def smart_play(position):
    reminder = day + month + year - position
    x = 0
    # This is a losing situation.
    if ( reminder %(month + day + 1) == 0):
        x = month + day
    # This is a winning situation.
    else:
        x = (reminder %(month + day + 1) )

    return min(x, (year + day + month - position))

# Applies a smart strategy for the computer, and supplies the user with a hint to win.
def advisor_play():
    print("Hint: To win, try to get the computer to be in a position where they have " + str((day + month + 1)) + " positions until the end of the game.")
    print("If it's the computer's turn, and the number of positions until the end of the game is divisible by " + str((day + month + 1)) + ", you can win! ;)\n")
    return

# Interacts with the user to get valid initial conditions for the game.
def start_play():
    print("The game goes as follows:\n1) You and the computer start at the same position\n2) You're allowed to move [1-" + str((day+month)) + "] steps at a time, and so is the computer.\n3) The computer starts playing, but you get to control the starting position and strategy of game for the computer\nWhoever reaches position 2029 first wins!\n")
    count = 0
    while (True):
        count += 1
        position = input("Please enter the position you want to start in, or -1 to start randomly\n")
        int_position = int(position)
        if  int_position > -2 and int_position < (day+month+year):
            if int_position == -1:
                int_position = random.randint(1,(day+month+year-1))
            break
        elif count > 5:
            print("You've failed too many times, try again later.\n")
            exit(0)
        else:
            response = input(
                "You've entered an invalid value, would you like to try again?\nWrite 'X' to exit and 'Y' to try again\n")
            if response == 'X':
                return
    print("You and the computer both start at position " + str(int_position) + ".\n")
    count = 0
    while True:
        count += 1
        strategy = input("Please enter the number of the strategy you'd like to compete against:\n1) Random\n2) Smart\n3) Advisor\n")
        int_strategy = int(strategy)
        if int_strategy > 0 and int_strategy < 4:
            break
        elif count > 5:
            print("You've failed too many times, try again later.\n")
            exit(0)
        else:
            response = input(
                "You've entered an invalid value, would you like to try again?\nWrite 'X' to exit and 'Y' to try again\n")
            if response == 'X':
                return
    return int_position, int_strategy

# Manages the game by calling the proper corrosponding functions and interacting with the user throught the play.
def manage_play():
    logs =[]
    logging.info("\nNew game:")
    position, strategy = start_play()
    current_play = 0
    while True:
        # user plays first:
        current_play = get_user_play(position, strategy)
        position += current_play
        logs.append("You played " + str(current_play) + " and moved to position " + str(position) + ".\n")
        logging.info("You played " + str(current_play) + " and moved to position " + str(position) + ".\n")
        print("You played " + str(current_play) + " and moved to position " + str(position) + ".\n")

        if position >= (day + month + year):
            print("YOU WIN! :D\n")
            logs.append("YOU WIN! :)\n")
            logging.info("YOU WIN! :)\n")
            break
        # computer plays according to strategy selected:
        if strategy == 1:
            current_play = random_play(position)
        elif strategy == 2:
            current_play = smart_play(position)
        else:
            current_play = smart_play(position)
        position += current_play
        logs.append("Computer played " + str(current_play) + " and moved to position " + str(position) + ".\n")
        logging.info("Computer played " + str(current_play) + " and moved to position " + str(position) + ".\n")
        print("Computer played " + str(current_play) + " and moved to position " + str(position) + ".\n")

        if position >= (day + month + year):
            print("COMPUTER WINS! :D\n")
            logs.append("COMPUTER WINS! :)\n")
            logging.info("COMPUTER WINS! :)\n")
            break

    print("Here's a recap of how the game went:\n")
    for i in logs:
        print(i)

# Interacts with the user to run multiple plays of the game if wanted.
if __name__ == '__main__':
    while(True):
        ans = input("Do you want to start a new game?(Y/N)\n")
        if ans == 'Y':
            manage_play()
        elif ans == 'N':
            print("Thank you for playing, have a good day! : )")
            exit(0)
        else:
            print("Invalid input, try again\n")


