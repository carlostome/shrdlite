
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

entity : basic_entity(Quant, Object) --->
    quantifier(Num):Quant, object(Num):Object.

entity : relative_entity(Quant, Object, Location) ---> 
    quantifier(Num):Quant, object(Num):Object,
    opt_that_is(Num),
    location:Location.

object(Num) : object(Form,Size,Color) ---> size:Size, color:Color, form(Num):Form.
object(Num) : object(Form,Size,Color) ---> color:Color, size:Size, form(Num):Form.
object(Num) : object(Form,'-', Color) ---> color:Color, form(Num):Form.
object(Num) : object(Form,Size,'-')  ---> size:Size, form(Num):Form.
object(Num) : object(Form,'-', '-')  ---> form(Num):Form.

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
relation : under ---> [under] ; [below].
relation : inside ---> [inside] ; [in] ; [into].

size : small ---> [small] ; [tiny].
size : large ---> [large] ; [big].

color : black ---> [black].
color : white ---> [white].
color : blue ---> [blue].
color : green ---> [green].
color : yellow ---> [yellow].
color : red ---> [red].

form(sg) : anyform ---> [object] ; [thing] ; [form].
form(pl) : anyform ---> [objects] ; [things] ; [forms].
form(sg) : brick ---> [brick].
form(pl) : brick ---> [bricks].
form(sg) : plank ---> [plank].
form(pl) : plank ---> [planks].
form(sg) : ball ---> [ball].
form(pl) : ball ---> [balls].
form(sg) : pyramid ---> [pyramid].
form(pl) : pyramid ---> [pyramids].
form(sg) : box ---> [box].
form(pl) : box ---> [boxes].
form(sg) : table ---> [table].
form(pl) : table ---> [tables].

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
