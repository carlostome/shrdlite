
:- op(1200, xfx, '--->').

%% Non-lexical grammar rules

command : Cmd --->
    opt_will_you, opt_please,
    basic_command : Cmd,
    opt_please.

basic_command : take(Entity) ---> take, entity:Entity.
basic_command : put(Location) ---> move, it, location:Location.
basic_command : move(Entity, Location) ---> move, entity:Entity, location:Location.

location : relative(Relation, Entity) ---> relation:Relation, entity:Entity.

entity : floor ---> the_floor.

entity : basic_entity(Quant, Block) --->
    quantifier(Num):Quant, block(Num):Block.

entity : relative_entity(Quant, Block, Location) ---> 
    quantifier(Num):Quant, block(Num):Block,
    opt_that_is(Num),
    location:Location.

block(Num) : block(Form,Size,Color) ---> size:Size, color:Color, form(Num):Form.
block(Num) : block(Form,Size,Color) ---> color:Color, size:Size, form(Num):Form.
block(Num) : block(Form,'-', Color) ---> color:Color, form(Num):Form.
block(Num) : block(Form,Size,'-')  ---> size:Size, form(Num):Form.
block(Num) : block(Form,'-', '-')  ---> form(Num):Form.

%% Lexical rules

quantifier(sg) : the ---> [the].
quantifier(sg) : any ---> [a] ; [an] ; [any].
quantifier(sg) : all ---> [every].
quantifier(pl) : all ---> [all].

relation : beside ---> [beside].
relation : leftof ---> [left,of] ; [to,the,left,of].
relation : rightof ---> [right,of] ; [to,the,right,of].
relation : above ---> [above].
relation : ontop ---> [on,top,of] ; [on].
relation : under ---> [under].
relation : inside ---> [inside] ; [in] ; [into].

size : small ---> [small].
size : medium ---> [medium] ; ['medium-sized'].
size : large ---> [large] ; [big].
size : wide ---> [wide].
size : tall ---> [tall].

color : black ---> [black].
color : white ---> [white].
color : blue ---> [blue].
color : green ---> [green].
color : yellow ---> [yellow].
color : red ---> [red].

form(sg) : anyblock ---> [block].
form(pl) : anyblock ---> [blocks].
form(sg) : ball ---> [ball].
form(pl) : ball ---> [balls].
form(sg) : box ---> [box].
form(pl) : box ---> [boxes].
form(sg) : pyramid ---> [pyramid].
form(pl) : pyramid ---> [pyramids].
form(sg) : rectangle ---> [rectangle].
form(pl) : rectangle ---> [rectangles].
form(sg) : square ---> [square].
form(pl) : square ---> [squares].

%% Lexicon (without semantic content)

the_floor ---> [the,floor].

opt_that_is(_) ---> [].
opt_that_is(sg) ---> [that,is].
opt_that_is(pl) ---> [that,are].

move ---> [move] ; [put] ; [drop].
take ---> [take] ; [grasp] ; [pick,up].
it ---> [it].

opt_will_you ---> [] ; [will,you] ; [can,you] ; [could,you].
opt_please ---> [] ; [please].
