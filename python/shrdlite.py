#!/usr/bin/env python

# Test from the command line:
# python shrdlite.py < ../examples/medium.json

from __future__ import print_function

import sys
import json

GRAMMAR_FILE = "shrdlite_grammar.fcfg"


def parse(utterance):
    import nltk
    grammar = nltk.data.load("file:" + GRAMMAR_FILE, cache=False)
    parser = nltk.FeatureChartParser(grammar)
    # API change between NLTK v2 and v3:
    # - v2 uses .nbest_parse()
    # - v3 will probably use .parse_all() or .parse() instead
    do_parse = getattr(parser, 'nbest_parse', 
                       getattr(parser, 'parse_all', parser.parse))
    try:
        return [result.label()['sem'] 
                for result in do_parse(utterance)]
    except ValueError:
        return []


def interpret(tree, world, holding, objects):
    return [True]


def solve(goal, world, holding, objects):
    col = list(map(bool, world)).index(True)
    return ["I pick up . . .", 'pick %d' % col, ". . . and I drop down", 'drop %d' % col]


def main(utterance, world, holding, objects, **_):
    result = {}
    result['utterance'] = utterance
    trees = parse(utterance)
    result['trees'] = [str(t) for t in trees]
    if not trees:
        result['output'] = "Parse error!"
        return result
    result['goals'] = goals = [goal for tree in trees
                               for goal in interpret(tree, world, holding, objects)]
    if not goals:
        result['output'] = "Interpretation error!"
        return result
    if len(goals) > 1:
        result['output'] = "Ambiguity error!"
        return result
    goal = goals[0]
    result['plan'] = plan = solve(goal, world, holding, objects)
    if not plan:
        result['output'] = "Planning error!"
        return result
    result['output'] = "Success!"
    return result


if __name__ == '__main__':
    input = json.load(sys.stdin)
    output = main(**input)
    # json.dump(output, sys.stdout)
    # json.dump(output, sys.stdout, sort_keys=True, indent=4)
    print("{", ",\n  ".join('%s: %s' % (json.dumps(k), json.dumps(v))
                            for (k, v) in output.items()), "}")
