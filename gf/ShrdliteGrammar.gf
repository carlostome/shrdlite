
abstract ShrdliteGrammar = {

flags startcat = Command;

cat 

Command; Location; Entity; Block;
Quantifier; Relation; Size; Color; Form;

fun

-- Non-lexical grammar rules

take : Entity -> Command;
put  : Location -> Command;
move : Entity -> Location -> Command;

relative : Relation -> Entity -> Location;

floor : Entity;
basic_entity : Quantifier -> Block -> Entity;
relative_entity : Quantifier -> Block -> Location -> Entity;

block : Form -> Size -> Color -> Block;

-- Lexical rules

the, any, all : Quantifier;
beside, leftof, rightof, above, ontop, under, inside : Relation;
small, medium, large, wide, tall : Size;
black, white, blue, green, yellow, red : Color;
anyblock, box, pyramid, rectangle, square, ball : Form;

}
