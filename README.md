# Elm Sage Solitaire

Based on Sage Solitaire, by Zach Gage

[Play the game](https://seestevecode.github.io/elm-sage-solitaire/elm-sage-solitaire.html)

## Rules

In Sage Solitaire, your goal is to clear all the cards.

Clear cards by making hands. Tap "Show Hand List" to see the full list of hands.

One other thing about hands: you'll need cards from at least 2 rows.

Bonus points are scored by clearing a cell. 150pts for the top row; 100pts for the middle row; 50pts for the bottom row.

Hands including the bonus suit score 2x more. The bonus card has been dealt out of the game.

Can't or don't want to make a hand? Trash a card instead! To trash a card, select it tap Trash.

You have 2 Trashes initially. Making hands restores Trashes, up to this initial value.

## TODO:

- [x] Render single card
- [x] Render list of cards
- [x] Render standard deck
- [x] Render list of lists, showing head and length
- [x] Render HTML
- [x] Extend model
- [x] Render full board
- [x] Generate shuffled deck
- [x] Highlight bonus cards
- [x] Redo CSS
- [x] Add/remove cards to/from selected
- [x] Highlight selected cards
- [x] Determine unique ranks and suits of selection
- [x] Refactor model and remove matrices
- [x] Check for valid hand selection
- [x] Check for selection over multiple rows
- [x] Trash single card
- [x] Render empty cell
- [x] Fix row selection count on rows with empty cell
- [x] Submit valid hand
- [x] Update score on hand submission
- [x] Add Clear button
- [x] Check for valid hands and add hint function
- [x] Add discards to model
- [x] Comply with elm-analyse
- [x] Add game state to model
- [x] Add info bar and refactor view to smaller functions
- [x] Refactor hand validation functions
- [x] Implement bonus for clearing a stack
- [x] Build complete layout with Stylish Elephants
- [x] Fix offset of card with no others in stack
- [ ] Improve best hand selection
- [ ] Add keyboard controls
- [ ] Separate into smaller modules
- [ ] Add in-game tutorial
- [ ] Add server-side high score table
- [ ] Allow generation of game seed and manual input
- [ ] Make layout responsive for mobile, etc.
- [ ] Charge for hints, on a sliding (logarithmic?) scale
- [ ] Add an undo function - at a cost(?)
