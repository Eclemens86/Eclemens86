// Evan Clemens, 3/17/21, Creates the game of war in java!
package cardGame;

import java.util.Random;

public class Deck {
	private Card[] deck;
	private int top;
	
	public Deck()
	{
		deck = new Card[52]; // Creates an array 52 spaces long to store the cards
		top = 0; // Makes the "top" card the first card in the array by setting its position to 0 
		
		for(int rank = 2; rank <= 14; rank ++)
		{
			Card Club = new Card(rank, 'C'); // Creates a club with rank "C". Next three lines do the same thing with the other suits 
			Card Diamond = new Card(rank, 'D');
			Card Spade = new Card(rank, 'S');
			Card Heart = new Card(rank, 'H');
			deck[rank-2] = Club; // Creates the club card with the first 14 spaces. Does the same with each other suit in next 3 lines 
			deck[rank+11] = Diamond;
			deck[rank+24] = Spade;
			deck[rank+37] = Heart;

		}
	}
	
	public void shuffle() 
	{
		Random randnum = new Random(); // Initializes a random number generator 
		int swap_cards; // This int holds the random number 
		
		for(int indexcard = 0; indexcard <= 51; indexcard++) // Runs through all the cards in the deck and shuffles them using the random number generator 
		{
			swap_cards = randnum.nextInt(52);
			swap(indexcard,swap_cards);
		}
	}
	
	public Card draw()
	{
		Card topcard; // Initializes a variable to be the top card in the deck 
		topcard = deck[top]; // Sets the top card equal to the actual top card at that time
		top = top + 1; // Returns the top card
		return topcard;
	}

	public boolean isEmpty() // Creates a class to show when the deck is empty, if the top card is the final spot in the array, the deck is now empty
	{
		if(top == 52)
			return true;
		else 
			return false;

	}
	
	private void swap(int i, int j)
	{
		Card icard = deck[i]; // Creates a card for the i and j variable 
		Card jcard = deck[j];
		deck[i] = jcard; // Swaps the cards positions 
		deck[j] = icard;

	}
	
}