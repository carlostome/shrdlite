The Shrdlite programming project
================================

Shrdlite is based on a programming project for the course Artificial
Intelligence, a course given at the University of Gothenburg and Chalmers
University of Technology.  For more information, see the course webpages:

- <http://www.cse.chalmers.se/edu/course/TIN172/>

In the [original readme](ORIG_README.md) the original instructions for that project can be
found.


Setup instructions
==========================

In order to build the program, it is necessary to have installed `python 2.7`
and haskell `stack` tool.

The haskell program that serves as a backend is located in the folder
`/haskell`. It is necessary to build it first:

```bash
cd haskell
stack build
```



Usage 
==========================

To use the program just use the script on the root folder of the project:

```bash
./run.sh

```

Then visit [localhost:8000/shrdlite.html](https://localhost:8000/shrdlite.html) and play
with it!
