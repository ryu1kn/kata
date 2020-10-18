
# Extensible Card Game

You're playing a game with a trump. A card has a following notation.

1H, 2S, ..., 13C

1. Calculate the point of your hand by adding their numbers

   "1H,2S,3C,4D" => 10

1. No point is given if invalid card is in the hand

   "1H,2S,100C,4D" => 0

1. Now the input has of the form "<rule-name>;<hand>" to enhance the rule.
   If the given rule is not known, you get 0 point.

   "unknown;1H,2S,3C,4D"                => 0

1. If `prefer-odd` rule is given, extra two points for each odd number card.

   "prefer-odd;1H,2S,3C,4D"             => 14

1. If `same-suite` rule is given, add extra 50 points.

   "same-suite;1H,2H,3H,4H"             => 60

1. Multiple rules can be given separated by comma `,`.

   "prefer-odd,same-suite;1H,2H,3H,4H"  => 64

1. If `lucky-card` rule is given specifying which card is special, if you
   have the same card in your hand, add 50 point.

   "lucky-card=4D;1H,2S,3C,4D" => 60

1. If `Joker` is in your hand, treat it as 100 point. Make Joker as a valid card.

   "1H,2S,3C,Joker" => 106
