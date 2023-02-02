import random

def main():
    infile = open("dictionary.txt", "r")
    word_list = infile.read().splitlines()
    
    all_five = open("Wordle.txt", "r")
    all_fivewords = all_five.read().splitlines()
    #   all_five = open("Wordle.txt", "a")
    
    word_loop = "Yes"
    while word_loop == "Yes":
    
        print("DIRECTIONS:")
        print("? is like gray in wordle, it means that there is no letter from the word there")
        print("# is like yellow in wordle, it means that the letter is in the word, just not in the right spot")
        print("I will print the letter if it is in the word and in the right spot")

        count = 1
        randomint = random.randint(0,8938)
        scramdomword = all_fivewords[randomint]
        # print(scramdomword)
        
        for i in range(6):
            print()
            my_guess = input("Please enter a guess for a five letter word in all caps: ")
            for x in range(5):
                if scramdomword[x] == my_guess[x] and my_guess[x] in scramdomword:
                    print(my_guess[x], end = '')
                elif my_guess[x] in scramdomword:
                    print("#", end = '')
                elif scramdomword[x] != my_guess[x] and my_guess[x] not in scramdomword:
                    print("?", end = '')
                    
    #         for x in range(5):
    #             if my_guess[x] in scramdomword:
    #                 print(my_guess[x],"#")

            print()
            if my_guess == scramdomword:
                print("Congrats you got it in", count, "guesses!")
                print("The word was", scramdomword)
                break
            count = count + 1
            
            if count == 6 and scramdomword != my_guess:
                print("Sorry, you did not get the word in six guesses. The word was", scramdomword)
                break
       

    #     for word in word_list:
    #         if len(word) == 5:
    #             all_five.write(word)
    #             all_five.write("\n")
    #             
    #     for word in all_fivewords:
    #         if len(word) == 5:
    #             count = count + 1
    #     print(count)
               
        word_loop = input("Do you want to play again? ")
        
        infile.close()
        all_five.close()
    
main()