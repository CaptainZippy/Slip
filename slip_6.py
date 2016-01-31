#! /usr/bin/env python
# add fexprs


import re
import pprint

def p(*args):
    for a in args:
        pprint.pprint(a)

class Atom:
    loc = ("x",0)
class Num(Atom):
    def __init__(self, num):
        if not isinstance(num,(int,float)):
            print("???", type(num))
        assert(isinstance(num,(int,float)))
        self.num = num
    def __str__(self):
        return str(self.num)
    def __repr__(self):
        return repr(self.num)
    def __sub__(self, n):
        assert(isinstance(n,Num))
        return Num(self.num-n.num)
    def __mul__(self, n):
        assert(isinstance(n,Num))
        return Num(self.num*n.num)
    def __truediv__(self, n):
        assert(isinstance(n,Num))
        return Num(self.num/n.num)
    def __add__(self, n):
        assert(isinstance(n,Num))
        return Num(self.num+n.num)
    def __eq__(self,n):
        assert(isinstance(n,Num))
        return self.num == n.num
class String(Atom):
    def __init__(self, s):
        assert(isinstance(s,str))
        self.str = s
    def __str__(self):
        return self.str
    def __repr__(self):
        return repr(self.str)
    def __len__(self):
        return len(self.str)
    def __getitem__(self, idx):
        return String(self.str[idx])
    def __eq__(self,n):
        assert(isinstance(n,String))
        return self.str == n.str
class Ident(Atom):
    def __init__(self, ident):
        self.ident = ident
    def __str__(self):
        return self.ident
    def __repr__(self):
        return repr(self.ident)
    def __hash__(self):
        return hash(self.ident)
    def __eq__(self, i):
        assert(isinstance(i,Ident))
        return self.ident == i.ident
class List(Atom):
    def __init__(self, lst=None):
        self.lst = []
        if lst:
            self.lst.extend(lst)
    def __len__(self):
        return len(self.lst)
    def __str__(self):
        return str(self.lst)
    def __repr__(self):
        return repr(self.lst)
    def append(self, atom):
        assert( isinstance(atom,Atom) )
        self.lst.append(atom)
    def extend(self, lst):
        assert( all(isinstance(i,Atom) for i in lst) )
        self.lst.extend(lst)
    def __getitem__(self, idx):
        return self.lst[idx]
class Operative(Atom):
    def __init__(self, op):
        self.oper = op
    def __call__(self, *args):
        return self.oper(*args)


class Env:
    def __init__(self, parent, keys=None, vals=None):
        self.keys = {}
        self.parent = parent
        if keys or vals:
            if len(keys)!=len(vals):
                #(keys, vals)
                raise RuntimeError("Wrong number of args %i,%i" % (len(keys), len(vals)))
            for n,v in zip(keys, vals):
                self.set(n, v)

    def __repr__(self):
        return repr(self.keys) + repr(self.parent) if self.parent else ""

    def get(self, key):
        assert(isinstance(key,Ident))
        tab = self
        while tab:
            v = tab.keys.get(key)
            if v is not None:
                return v
            tab = tab.parent
        #print("??",key.ident)
        raise KeyError(key)

    def set(self, key, val):
        if not isinstance(key,Ident):
            assert(isinstance(key,str))
            key = Ident(key)
        #print("%s=%s" % (key,val))
        assert self.keys.get(key) is None
        self.keys[key] = val
        return val


class Wrap:
    def __init__(self, func):
        self.func = func
    def __call__(self, call_env, *args):
        return self.func( call_env, *[s_eval(call_env, a) for a in args] )


def s_wrap(env, expr):
    return Wrap(s_eval(env, expr))


def lineof(txt, pos):
    return txt.count("\n", 0, pos)+1
def context(txt, pos):
    return txt[pos:min(len(txt),pos+30)].replace("\n","\\n")
def location(expr):
    txt, pos = expr.loc
    return "%s:%s" % (lineof(txt, pos), context(txt, pos))

stack = []

def s_eval(env, expr):
    #print(location(expr))#print("EVAL", expr)
    stack.append(expr)
    ret = None
    if isinstance(expr, (String,Num)):
        ret = expr
    elif isinstance(expr, Ident):
        ret = env.get(expr)
    elif isinstance(expr, List):
        func = s_eval(env, expr[0])
        ret = func(env, *expr[1:])
    else:
        print(type(expr))
        assert(0)
    stack.pop()
    return ret


def s_comp1(env, denv, expr):
    return s_eval(denv, expr)


def s_begin(env, *args):
    r = None
    lenv = Env(env)
    for a in args:
        r = s_eval(lenv, a)
    return r


def s_switch(env, expr, *cases):
    exp = s_eval(env, expr)
    print("?", exp, type(exp))
    for val,rest in cases:
        if s_eval(env, val):
            return s_eval(env,rest)
    assert 0, "Not matched"


def s_define(env, sym, val):
    return env.set(sym, s_eval(env,val))


class Lambda:
    def __init__(self, lex_env, vars, body):
        self.lex_env = lex_env
        self.vars = vars
        self.body = body
    def __call__(self, call_env, *args):
        #print(">lam", args, [type(a) for a in args])
        e = Env(self.lex_env, self.vars, [s_eval(call_env,a) for a in args])
        r = s_eval(e, self.body)
        #print("<lam", r, type(r))
        return r
    def __repr__(self):
        return repr(self.body)


def s_lambda(lex_env, vars, body):
    #return Lambda(lex_env, vars, body)
    v = Vau(lex_env, vars, None, body)
    return Wrap(v)


def s_func(env, name, vars, *body):
    return s_define(env, name, List([
        Ident("lambda"),
        vars,
        List( [Ident("begin")] + list(body) )
    ]))


class Vau:
    def __init__(self, lex_env, vars, sym, body):
        self.lex_env = lex_env
        self.vars = vars
        self.sym = sym
        self.body = body
    def __call__(self, call_env, *args):
        e = Env(self.lex_env, self.vars, args)
        if self.sym:
            e.set(self.sym, call_env)
        return s_eval(e, self.body)


def s_vau(lex_env, vars, sym, body):
    return Vau(lex_env, vars, sym, body)


def s_macro(env, name, vars, sym, body):
    return s_define(env, name, List([Ident("vau"), vars, sym, body]))


def f_plus(env, *args):
    acc = Num(0)
    for a in args:
        acc += a
    return acc


def f_sub(env, *args):
    acc = args[0]
    for a in args[1:]:
        acc -= a
    return acc


def f_mul(env, *args):
    acc = Num(1)
    for a in args:
        acc *= a
    return acc

def f_div(env, *args):
    assert(len(args)==2)
    return args[0] / args[1]


def s_cond(env, *cases):
    for c,b in cases:
        if s_eval(env, c):
            return s_eval(env, b)
    raise RuntimeError("invalid case")


def s_if(env, test, yes, no):
    if s_eval(env, test):
        return s_eval(env, yes)
    else:
        return s_eval(env, no)


def s_print(env, *args):
    def next(args):
        for a in args:
            v = s_eval(env,a)
            if isinstance(v, bytes):
                v = v.decode("utf8")
            yield str(v)
    print(" ".join(next(args)))


def p_eq(env, *args):
    #print("EQ",args)
    cur = s_eval(env, args[0])
    for r in args[1:]:
        a = s_eval(env, r)
        assert type(a)==type(cur), "%s vs %s" %( str(a),str(cur))
        #print(cur, a, type(cur), type(a))
        #if type(cur) != type(a):
        #    return False
        if cur != a:
            return False
    return True


def p_list(env, expr):
    return isinstance(expr, List)


def p_empty(env, expr):
    cur = s_eval(env, expr)
    assert isinstance(cur, (List,String))
    return len(cur)==0

def f_map(env, func, expr):
    #print("MAP", env, func, expr)
    return List([func(env,f) for f in expr])


def f_chr(env, c):
    assert(isinstance(c,String))
    return Num(int(c.str))

def f_type(env, c):
    return type(c).__name__

def f_first(env, expr):
    assert isinstance(expr, (List,String))
    return expr[0]


def f_rest(env, expr):
    assert isinstance(expr, (List,String))
    return expr[1:]

def s_let(env, bind, body):
    e = Env(env)
    for b in bind:
        e.set(b[0], s_eval(env,b[1]))
    return s_eval(e, body)


def s_printf(env, fmt, *args):
    print(fmt, args)


def read(txt, inputname="<input>"):
    class ParseError(RuntimeError):
        def __init__(self, msg):
            RuntimeError.__init__(self,"%s:%s:%s near '%s'" % (inputname, msg, lineof(txt,pos), context(txt,pos)))
    ret = List([Ident("begin")])
    stack = [ ret ]
    reg = re.compile(r"""
        (?P<white>\s+) |
        (?P<comment>;[^\n]+) |
        (?P<open>\() |
        (?P<close>\)) |
        (?P<ident>[$a-zA-Z_\+\-][a-zA-Z0-9-=\+\-\?]*) |
        (?P<qstring>"[^"]*") |
        (?P<num>[0-9][a-zA-Z0-9_-]*)
        """, re.VERBOSE)
    def white(m):
        pass
    def comment(m):
        pass
    def open(m):
        i = List()
        i.loc = (txt, pos)
        stack[-1].append(i)
        stack.append(i)
    def close(m):
        stack.pop()
        if len(stack)==0:
            raise ParseError("Extra ')'")
    def ident(m):
        i = Ident(m.group())
        i.loc = (txt, pos)
        stack[-1].append(i)
    def qstring(m):
        i = String(m.group()[1:-1])
        i.loc = (txt, pos)
        stack[-1].append(i)
    def num(m):
        i = Num(int(m.group()))
        i.loc = (txt, pos)
        stack[-1].append(i)

    actions = locals()
    pos = 0
    while 1:
        m = reg.match(txt, pos)
        if m:
            actions[m.lastgroup](m)
            pos = m.end()
        elif pos==len(txt):
            break
        else:
            raise ParseError("unrecognized token")
    if len(stack)!=1:
        raise ParseError("open brace at eof")
    return ret


def readfile(fname):
    return read(open(fname).read(), fname)


import sys
sys.setrecursionlimit(10000)


def main():
    env = Env(None)
    argv = [String(a) for a in sys.argv[1:]]
    env.set("argv", List(argv))
    for k,v in globals().items():
        if len(k)<2 or k[0]=="_" or k[1]!="_":
            continue
        if k.startswith("s_"):#special operative
            env.set(k[2:], Operative(v))
        elif k.startswith("f_"):#func applicative
            env.set(k[2:], Wrap(Operative(v)))
        elif k.startswith("p_"):#pred?
            env.set(k[2:]+"?", Wrap(Operative(v)))
        else:
            raise RuntimeError(k)
    try:
        prog = readfile("slip_6.slip")
    except RuntimeError as err:
        print(err)
        return None
    #pprint.pprint(prog)
    #pprint.pprint(normalize(prog))
    try:
        print(s_eval(env, prog))
    except:
        for s in stack:
            print(location(s))
        raise


if __name__ == '__main__':
    main()
