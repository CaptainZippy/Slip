
# add fexprs


import re
import pprint

def p(*args):
    for a in args:
        pprint.pprint(a)

class Num(int):
    loc = -1
class String(bytes):
    loc = -1
class Ident(str):
    loc = -1
class List(list):
    loc = -1


class Env:
    def __init__(self, parent, keys=None, vals=None):
        self.keys = {}
        self.parent = parent
        if keys or vals:
            if len(keys)!=len(vals):
                print(keys, vals)
                raise RuntimeError("foo")
            for n,v in zip(keys, vals):
                self.set(n, v)

    def __repr__(self):
        return repr(self.keys) + repr(self.parent) if self.parent else ""

    def get(self, key):
        tab = self
        while tab:
            v = tab.keys.get(key)
            if v is not None:
                return v
            tab = tab.parent
        raise KeyError(key)

    def set(self, key, val):
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


def s_eval(env, expr):
    print(expr.loc)#print("EVAL", expr)
    if isinstance(expr, (String,Num)):
        return expr
    elif isinstance(expr, Ident):
        return env.get(expr)
    elif isinstance(expr, List):
        func = s_eval(env, expr[0])
        return func(env, *expr[1:])
    else:
        print(type(expr))
        assert(0)


def s_comp1(env, denv, expr):
    return s_eval(denv, expr)


def s_begin(env, *args):
    r = None
    lenv = Env(env)
    for a in args:
        r = s_eval(lenv, a)
    return r


def s_define(env, sym, val):
    return env.set(sym, s_eval(env,val))


class Lambda:
    def __init__(self, lex_env, vars, body):
        self.lex_env = lex_env
        self.vars = vars
        self.body = body
    def __call__(self, call_env, *args):
        e = Env(self.lex_env, self.vars, [s_eval(call_env,a) for a in args])
        return s_eval(e, self.body)


def s_lambda(lex_env, vars, body):
    return Lambda(lex_env, vars, body)


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
        e.set(self.sym, call_env)
        return s_eval(e, self.body)


def s_vau(lex_env, vars, sym, body):
    return Vau(lex_env, vars, sym, body)


def s_macro(env, name, vars, sym, body):
    return s_define(env, name, List([Ident("vau"), vars, sym, body]))


def s_plus(env, *args):
    acc = 0
    for a in args:
        acc += s_eval(env, a)
    return acc


def s_sub(env, *args):
    acc = s_eval(env, args[0])
    for a in args[1:]:
        acc -= s_eval(env, a)
    return acc


def s_mul(env, *args):
    acc = 1
    for a in args:
        acc *= s_eval(env, a)
    return acc


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
        #print(cur, a, type(cur), type(a))
        #if type(cur) != type(a):
        #    return False
        if cur != a:
            return False
    return True


def p_list(env, expr):
    cur = s_eval(env, expr)
    return isinstance(cur, List)


def p_empty(env, expr):
    cur = s_eval(env, expr)
    return isinstance(cur, (List,String)) and not cur

def f_map(env, func, expr):
    #print("MAP", env, func, expr)
    return List([func(env,f) for f in expr])


def f_chr(env, c):
    return int(c)


def s_first(env, expr):
    cur = s_eval(env, expr)
    assert isinstance(cur, (List,String))
    return cur[0]


def s_rest(env, expr):
    cur = s_eval(env, expr)
    assert isinstance(cur, (List,String))
    return cur[1:]

def s_let(env, bind, body):
    e = Env(env)
    for b in bind:
        e.set(b[0], s_eval(env,b[1]))
    return s_eval(e, body)


def s_printf(env, fmt, *args):
    print(fmt, args)


def read(txt, inputname="<input>"):
    def lineof(txt, pos):
        return txt.count("\n", 0, pos)+1
    def context(txt, pos):
        return txt[pos:min(len(txt),pos+30)].replace("\n","\\n")
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
        i.loc = pos
        stack[-1].append(i)
        stack.append(i)
    def close(m):
        stack.pop()
        if len(stack)==0:
            raise ParseError("Extra ')'")
    def ident(m):
        i = Ident(m.group())
        i.loc = pos
        stack[-1].append(i)
    def qstring(m):
        i = String(m.group()[1:-1].encode("utf8"))
        i.loc = pos
        stack[-1].append(i)
    def num(m):
        i = Num(int(m.group()))
        i.loc = pos
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
    argv = [String(a.encode("utf8")) for a in sys.argv[1:]]
    print(argv)
    env.set("argv", List(argv))
    for k,v in globals().items():
        if len(k)<2 or k[0]=="_" or k[1]!="_":
            continue
        if k.startswith("s_"):#special operative
            env.set(k[2:], v)
        elif k.startswith("f_"):#func applicative
            env.set(k[2:], Wrap(v))
        elif k.startswith("p_"):#pred?
            env.set(k[2:]+"?", v)
        else:
            raise RuntimeError(k)
    try:
        prog = readfile("slip_5.slip")
    except RuntimeError as err:
        print(err)
        return None
    pprint.pprint(prog)
    #try:
    print(s_eval(env, prog))
    #except U:
    #    pass


if __name__ == '__main__':
    main()
