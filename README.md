The Shrdlite programming project
================================

Shrdlite is a programming project in Artificial Intelligence, a course given 
at the University of Gothenburg and Chalmers University of Technology.
For more information, see the course webpages:

- <http://www.cse.chalmers.se/edu/course/TIN172/>

The goal of the project is to create an interpreter and a planner so that
a person can control a robot in a blocks world to move around objects,
by giving commands in natural language.

To make the project more interesting, there is a web-based graphical 
interface from which the user can interact with the blocks world.
The interface is written in Javascript and SVG, and it communicates with
the natural language parser, interpreter and planner via Ajax CGI.

The natural language grammar and parser are already given, in four different
formats, and can be used from at least four different programming languages:

- Prolog DCG: 
  This grammar can be used as is from any standard compliant Prolog, 
  but also from Java via the GNUPrologJava library.
  
- Python NLTK grammar: 
  This grammar is written using the NLTK library, and can be used 
  from within Python.
  
- Haskell combinator parser: 
  The grammar is written as parser combinators, and can be used 
  in a Haskell program.

- Grammatical Framework: 
  This grammar formalism has bindings to C/C++ and Python.

Local web server
----------------

To be able to run the graphical interface you need a web server. On Linux
or Mac, there are no direct problems -- in fact, Mac comes preinstalled with
the Apache server. But on Windows you have to install something.

The local web server that these files are using is Python's built-in web
server. To start it, just run

    python -m CGIHTTPServer 8000

from the same directory as the file `shrdlite.html`. Now let the web
server keep running and browse to any of these addresses:

- <http://localhost:8000/shrdlite.html>
- <http://127.0.0.1:8000/shrdlite.html>
- <http://0.0.0.0:8000/shrdlite.html>

How it works
------------

The graphical interface is written in SVG and Javascript in the file 
`shrdlite.js`. The first 20 lines of the file contain some constants 
that you can experiment with. Otherwise it shouldn't be necessary to 
read or understand the Javscript code, if you don't want to.

When the user enters an utterance, the Javascript calls the Ajax CGI script 
`ajaxwrapper.py` with information about the current world and the user's
utterance. The wrapper script calls another command line program, which
is the one that you will be writing.

Currently there are four possible alternatives for the command line program:

- `javaprolog/shrdlite.pl`, written in SWI Prolog
- `javaprolog/Shrdlite.java`, written in Java
- `haskell/Shrdlite.hs`, written in Haskell
- `python/shrdlite.py`, written in Python

Just uncomment the appropriate definitions of `SCRIPTDIR` and `SCRIPT` 
in `ajaxwrapper.py`, and you should be ready to go.

Speech synthesis
-----------------

The latest versions of Safari (v6.1 or later) and Chrome (v33 or later) 
have implemented the W3C Web Speech API. Shrdlite uses this if possible!

- <https://dvcs.w3.org/hg/speech-api/raw-file/tip/speechapi.html>
- <http://www.broken-links.com/2013/09/20/web-speech-api-part-one-speech-synthesis>


Alternative webserver
---------------------

If you want to use another webserver (such as the built-in Apache on a Mac), 
then you might have to modify some files:

- Some webservers only allow CGI scripts with file suffix ".cgi":

    + Try to rename `cgi-bin/ajaxwrapper.py` to `cgi-bin/ajaxwrapper.py.cgi`
      (and edit `shrdlite.js` accordingly)

- Some webservers require that CGI scripts are in a certain directory:

    + Try to rename the `cgi-bin` directory, or move `ajaxwrapper.py`
      to the directory above (and edit `shrdlite.js` accordingly)

- The program that `ajaxwrapper.py` calls might need another path:

    + Try to replace the first argument of the `SCRIPT` variable in
      `ajaxwrapper.py`, e.g., `/usr/bin/python` with `/usr/local/bin/python`

The command line program
------------------------

The command line program takes JSON input and should produce JSON output.
The input JSON contains the following fields:

- "utterance": a list of words (strings)
- "holding": an object identifier (string) or `null`
- "world": a list of list of object identifiers (strings)
- "objects": a mapping from object identifiers to information about them

The output JSON should contain at least the following:

- "output": a system utterance (string)
- "plan": a list of actions (strings)

It can also contain the fields "utterance", "trees" and "goals", but they 
are only used for printing debug information.

Grammars
--------

All four given command line programs have include a grammar and a parser,
which means that you do not have to implement parsing. 
(If you don't want to modify the grammar of course).

There are Shrdlite grammars that can be used in several different
programming languages.

### GF (Grammatical Framework)

To use this grammar you need to install GF, 
see instructions at <http://www.grammaticalframework.org>.

There are two grammar files, `ShrdliteGrammar.gf` (the abstract syntax) 
and `ShrdliteGrammarEng.gf` (the English translations). 

Here is an example run from within the GF runtime:

    > i ShrdliteGrammarEng.gf
    ShrdliteGrammar> p "put the white ball in a box on the floor"
    move (basic_entity the (object ball ?3 white)) (relative inside (relative_entity any (object box ?10 ?11) (relative ontop floor)))
    move (relative_entity the (object ball ?3 white) (relative inside (basic_entity any (object box ?9 ?10)))) (relative ontop floor)

This grammar is not used in any of the command line programs, but there
are GF bindings for C/C++, Haskell and Python.

### Python (with NLTK)

To use this grammar you need to install Python and NLTK,
see instructions here:

- <http://www.python.org/> (only if you have Windows)
- <http://www.nltk.org/>

There is one grammar file, `shrdlite_grammar.fcfg`, which can be used like this:

    >>> import nltk
    >>> grammar = nltk.data.load("file:shrdlite_grammar.fcfg", cache=False)
    >>> parser = nltk.FeatureChartParser(grammar)
    >>> sentence = "put the white ball in a box on the floor".split()
    >>> for tree in parser.nbest_parse(sentence): 
    ...     print tree.label()['sem']
    (move, (basic_entity, the, (object, ball, -, white)), (relative, inside, (relative_entity, any, (object, box, -, -), (relative, ontop, floor))))
    (move, (relative_entity, the, (object, ball, -, white), (relative, inside, (basic_entity, any, (object, box, -, -)))), (relative, ontop, floor))

To test the command line program `shrdlite.py`, you can do this:

    python shrdlite.py < ../medium.json

### Prolog

There is one grammar file, `shrdlite_grammar.pl`, and one parser file
`dcg_parser.pl`. They should be usable in most Prologs:

    ?- [shrdlite_grammar, dcg_parser].
    ?- parse(command, [put,the,white,ball,in,a,box,on,the,floor], Tree).
    Tree = move(basic_entity(the, object(ball, -, white)), relative(inside, relative_entity(any, object(box, -, -), relative(ontop, floor)))) ;
    Tree = move(relative_entity(the, object(ball, -, white), relative(inside, basic_entity(any, object(box, -, -)))), relative(ontop, floor)) ;
    No (more) solutions

To test the command line program `shrdlite.pl` in SWI Prolog, you should 
be able to do this:

    swipl -q -g main,halt -s shrdlite.pl < ../medium.json

More information about SWI Prolog can found here: 

- <http://www.swi-prolog.org/>

### Java

There are no good grammar libraries for Java, so this version uses the 
Prolog grammar via the GNUPrologJava library. After compilation you can
test the command line program like this:

    java -cp gnuprologjava-0.2.6.jar:json-simple-1.1.1.jar:. Shrdlite < ../medium.json

Read more about the needed libraries here:

- <http://www.gnu.org/software/gnuprologjava/>
- <http://code.google.com/p/json-simple/>

### Haskell

The Haskell grammar is of course written as a combinator parser, 
implemented as an *Applicative Functor* (McBride & Paterson, 2008).

    Prelude> :l Shrdlite.hs
    Main> let sent = words "put the white ball in a box on the floor"
    Main> mapM_ print $ parse command sent
    Move (BasicEntity The (Object AnySize White Ball)) (Relative Inside (RelativeEntity Any (Object AnySize AnyColor Box) (Relative Ontop Floor)))
    Move (RelativeEntity The (Object AnySize White Ball) (Relative Inside (BasicEntity Any (Object AnySize AnyColor Box)))) (Relative Ontop Floor)

To test the command line program `Shrdlite.hs` just do this:

    runhaskell Shrdlite.hs < ../medium.json

To be able to read and write JSON, you have to install Text.JSON:

- <http://hackage.haskell.org/package/json>
