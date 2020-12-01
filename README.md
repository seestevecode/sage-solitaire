# Sage Solitaire

Based on Sage Solitaire, by Zach Gage

[Play the game](https://seestevecode.github.io/sage-solitaire/elm-sage-solitaire.html)

## Rules

In Sage Solitaire, your goal is to **clear all the cards**.

Clear cards by **making hands**. Tap "Show Hand List" to see the full list of hands.

One other thing about hands: you'll need cards from **at least 2 rows**.

Bonus points are scored by **clearing a cell**. 150pts for the top row; 100pts for the middle row; 50pts for the bottom row.

Hands including the **bonus suit** score 2x more. The bonus card has been dealt out of the game.

Can't or don't want to make a hand? **Trash a card** instead! To trash a card, select it tap Trash.

You have 2 Trashes initially. Making hands restores Trashes, up to this initial value.

## Recent Changes

- You are now charged for some hints, based on the number of hints already taken and your current score.

## Road Map

- Make Hint suggest Trash when no valid hands are available
- Add in-game tutorial
- Make layout responsive for mobile, etc.
- Add keyboard controls
- Allow generation of game seed and manual input
- Add an undo function - at a cost(?)
- Add server-side high score table
- Improve best hand selection
