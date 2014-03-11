--# -path=.:alltenses

concrete ShrdliteGrammarEng of ShrdliteGrammar = 
  open SyntaxEng, (E=ExtraEng), ParadigmsEng, (I=IrregEng), (M=MorphoEng) in {

lincat 

Command = Utt;
Entity = NP;
Object = CN;
Location = Adv;
Quantifier = Det;
Relation = Prep;
Size = A;
Color = A;
Form = N;

lin

take e = mkCommand (mkVP take_V2 e);
put  l = mkCommand (mkVP (mkVP move_V2 (mkNP it_Pron)) l);
move e l = mkCommand (mkVP (mkVP move_V2 e) l);

relative r e = SyntaxEng.mkAdv r e;

floor = mkNP the_Det floor_N;

basic_entity q b = mkNP q b;

relative_entity q b l = 
    mkNP q (mkCN b l)
  | mkNP q (mkCN b (mkRS (mkRCl E.that_RP l)));

object f s c = 
    mkCN c (mkCN s f)
  | mkCN s (mkCN c f)
  | mkCN c f
  | mkCN s f
  | mkCN f;

-- Lexical rules

the = the_Det;
any = a_Det | mkDet E.any_Quant;
all = every_Det | lin Det (M.mkDeterminer plural "all");

beside  = mkPrep "beside";
leftof  = mkPrep "to the left of" | mkPrep "left of";
rightof = mkPrep "to the right of" | mkPrep "right of";
above   = mkPrep "above";
ontop   = mkPrep "on top of" | mkPrep "on";
under   = mkPrep "under";
inside  = mkPrep "inside" | mkPrep "in" | mkPrep "into";

small  = mkA "small" | mkA "tiny";
large  = mkA "large" | mkA "big";

black  = mkA "black";
white  = mkA "white";
blue   = mkA "blue";
green  = mkA "green";
yellow = mkA "yellow";
red    = mkA "red";

anyform = mkN "object" | mkN "thing" | mkN "form";
brick   = mkN "brick";
plank   = mkN "plank";
ball    = mkN "ball";
pyramid = mkN "pyramid";
box     = mkN "box";
table_  = mkN "table";

oper

-- Lexicon

mkCommand : VP -> Utt;
mkCommand vp = lin Utt 
  {s = variants{"" | "will you" | "can you" | "could you"} ++
     variants{"" | "please"} ++ 
     (mkUtt (mkImp vp)).s ++ 
     variants{"" | "please"}};

floor_N : N = mkN "floor";

move_V2 : V2 = mkV2 "move" | mkV2 "put" | mkV2 "drop";
take_V2 : V2 = mkV2 I.take_V | mkV2 "grasp" | mkV2 (partV (mkV "pick") "up");

}
