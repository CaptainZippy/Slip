
# add fexprs


import re
import pprint

def p(*args):
    for a in args:
        pprint.pprint(a)

class Num(int):
    pass
class String(bytes):
    pass
class Ident(str):
    pass
class List(list):
    def __init__(self,*args):
        self.extend(args)

class Env:
    def __init__(self, parent, keys=None, vals=None):
        self.keys = {}
        self.parent = parent
        if keys or vals:
            assert(len(keys)==len(vals))
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
        return self.func( call_env, *[b_eval(call_env, a) for a in args] )


def b_wrap(env, expr):
    return Wrap(b_eval(env, expr))


def b_eval(env, expr):
    #print("EVAL", expr)
    if isinstance(expr, (String,Num)):
        return expr
    elif isinstance(expr, Ident):
        return env.get(expr)
    elif isinstance(expr, List):
        func = b_eval(env, expr[0])
        return func(env, *expr[1:])
    else:
        print(type(expr))
        assert(0)


def b_comp1(env, denv, expr):
    return b_eval(denv, expr)


def b_begin(env, *args):
    r = None
    lenv = Env(env)
    for a in args:
        r = b_eval(lenv, a)
    return r


def b_define(env, sym, val):
    return env.set(sym, b_eval(env,val))


class Lambda:
    def __init__(self, lex_env, vars, body):
        self.lex_env = lex_env
        self.vars = vars
        self.body = body
    def __call__(self, call_env, *args):
        e = Env(self.lex_env, self.vars, [b_eval(call_env,a) for a in args])
        return b_eval(e, self.body)


def b_lambda(lex_env, vars, body):
    return Lambda(lex_env, vars, body)


def b_func(env, name, vars, *body):
    return b_define(env, name, List(Ident("lambda"), vars, List(Ident("begin"), *body)))


class Vau:
    def __init__(self, lex_env, vars, sym, body):
        self.lex_env = lex_env
        self.vars = vars
        self.sym = sym
        self.body = body
    def __call__(self, call_env, *args):
        e = Env(self.lex_env, self.vars, args)
        e.set(self.sym, call_env)
        return b_eval(e, self.body)


def b_vau(lex_env, vars, sym, body):
    return Vau(lex_env, vars, sym, body)


def b_macro(env, name, vars, sym, body):
    return b_define(env, name, List(Ident("vau"), vars, sym, body))


def b_plus(env, *args):
    acc = 0
    for a in args:
        acc += b_eval(env, a)
    return acc


def b_sub(env, *args):
    acc = b_eval(env, args[0])
    for a in args[1:]:
        acc -= b_eval(env, a)
    return acc


def b_mul(env, *args):
    acc = 1
    for a in args:
        acc *= b_eval(env, a)
    return acc


def b_cond(env, *cases):
    for c,b in cases:
        if b_eval(env, c):
            return b_eval(env, b)
    raise RuntimeError("invalid case")


def b_if(env, test, yes, no):
    if b_eval(env, test):
        return b_eval(env, yes)
    else:
        return b_eval(env, no)


def b_print(env, *args):
    print(" ".join(str(b_eval(env,a)) for a in args))
    return None


def s_read(txt, inputname="<input>"):
    def lineof(txt, pos):
        return txt.count("\n", 0, pos)+1
    def context(txt, pos):
        return txt[pos:min(len(txt),pos+30)].replace("\n","\\n")
    class ParseError(RuntimeError):
        def __init__(self, msg):
            RuntimeError.__init__(self,"%s:%s:%s near '%s'" % (inputname, msg, lineof(txt,pos), context(txt,pos)))
    ret = List(Ident("begin"))
    stack = [ ret ]
    reg = re.compile(r"""
        (?P<white>\s+) |
        (?P<comment>;[^\n]+) |
        (?P<open>\() |
        (?P<close>\)) |
        (?P<ident>[$a-zA-Z_][a-zA-Z0-9-]*) |
        (?P<qstring>"[^"]*") |
        (?P<num>[0-9][a-zA-Z0-9_-]*)
        """, re.VERBOSE)
    def white(m):
        pass
    def comment(m):
        pass
    def open(m):
        l = List()
        stack[-1].append(l)
        stack.append(l)
    def close(m):
        stack.pop()
        if len(stack)==0:
            raise ParseError("Extra ')'")
    def ident(m):
        stack[-1].append(Ident(m.group()))
    def qstring(m):
        item = m.group()[1:-1]
        stack[-1].append(String(item.encode("utf8")))
    def num(m):
        stack[-1].append(Num(int(m.group())))
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


def s_readfile(fname):
    return s_read(open(fname).read(), fname)


def main():
    env = Env(None)
    for k,v in globals().items():
        if k.startswith("b_"):
            #p(k)
            env.set(k[2:], v)
    try:
        prog = s_readfile("slip_5.slip")
    except RuntimeError as err:
        print(err)
        return None
    #pprint.pprint(prog)
    print(b_eval(env, prog))


if __name__ == '__main__':
    main()
