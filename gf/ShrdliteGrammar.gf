
abstract ShrdliteGrammar = {

flags startcat = Command;

cat 

Command; Location; Entity; Object;
Quantifier; Relation; Size; Color; Form;

fun

-- Non-lexical grammar rules

take : Entity -> Command;
put  : Location -> Command;
move : Entity -> Location -> Command;

relative : Relation -> Entity -> Location;

floor : Entity;
basic_entity : Quantifier -> Object -> Entity;
relative_entity : Quantifier -> Object -> Location -> Entity;

object : Form -> Size -> Color -> Object;

-- Lexical rules

the, any, all : Quantifier;
beside, leftof, rightof, above, ontop, under, inside : Relation;
small, large : Size;
black, white, blue, green, yellow, red : Color;
anyform, brick, plank, ball, pyramid, box, table_ : Form;

}
