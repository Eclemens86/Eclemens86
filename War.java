package cardGame;

public class War {

	public static void main(String[] args) {
		
		Deck d = new Deck(); // Creates a new deck, assigns the computer and user 3 cards, and initializes the score to be zero so we can count up
		Card aicard1, usercard1, aicard2, usercard2, aicard3, usercard3;
		int aiscore = 0, userscore = 0;
		
		System.out.println("The deck is now shuffling!");// Prompts the user that the deck is now shuffling 
		d.shuffle();
		
		while(!(d.isEmpty())) // This while loop runs when the deck is not empty 
		{
			aicard1 = d.draw(); // Both users draw a card
			usercard1 = d.draw();
			
			if(usercard1.getRank()>aicard1.getRank()) // this loop Tallies the points based on who's card was higher 
				userscore++;
			else if(usercard1.getRank()<aicard1.getRank())
				aiscore++; 
			else if(usercard1.getRank()==aicard1.getRank()) // If there is a tie a war ensues meaning both players draw two more cards 
			{
				aicard2 = d.draw();
				usercard2 = d.draw();
				aicard3 = d.draw();
				usercard3 = d.draw();
				if(usercard2.getRank()>aicard2.getRank()) // This loop Tallies the points based on who's second card was higher 
					userscore = userscore + 3;
				else if(usercard2.getRank()<aicard2.getRank())
					aiscore = aiscore + 3;
				else if(usercard2.getRank()==aicard2.getRank()) // This loop Tallies the points based on who's third card was higher if there is another tie
				{
					if(usercard3.getRank()>aicard3.getRank())
						userscore = userscore + 3;
					else if(usercard3.getRank()<aicard3.getRank())
						aiscore = aiscore + 3;
				}
			}
		}
		System.out.println("The users score was " + userscore); // Prompts the user with both their score and the computers 
		System.out.println("The ai's score was " + aiscore);
	}

}
