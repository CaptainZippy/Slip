
# add fexprs


import re
import pprint


class Env:
    def __init__(self, parent, keys=None, vals=None):
        self.keys = {}
        self.parent = parent
        if keys or vals:
            assert(len(keys)==len(vals))
            for n,v in zip(keys, vals):
                self.set(n, b_eval(parent,v))

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


def b_wrap(env, expr):
    func = b_eval(env, expr)
    def closure(e, *args):
        return func( e, *[b_eval(e, a) for a in args] )
    return closure

def b_eval(env, expr):
    #print("EVAL", expr)
    if isinstance(expr, (bytes,float,int)):
        return expr
    elif isinstance(expr, str):
        return env.get(expr)
    elif isinstance(expr, list):
        func = b_eval(env, expr[0])
        return func(env, *expr[1:])
    else:
        assert(0)

def b_comp1(env, denv, expr):
    return b_eval(denv, expr)

def b_begin(env, *args):
    r = None
    lenv = Env(env)
    for e in args:
        r = b_eval(lenv, e)
    return r


def b_define(env, sym, val):
    return env.set(sym, b_eval(env,val))


def b_lambda(lex_env, vars, body):
    def closure(call_env, *args):
        e = Env(lex_env, vars, [b_eval(call_env,a) for a in args])
        return b_eval(e, body)
    return closure


b_func = b_lambda
#def b_func(env, name, args, body):
#;    return env.set(name, b_lambda(env, args, body))


def b_vau(lex_env, vars, sym, body):
    def closure(call_env, *args):
        e = Env(lex_env, vars, args)
        e.set(sym, call_env)
        return b_eval(e, body)
    return closure


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


def s_read2(txt):
    txt = txt.lstrip()
    item = None
    while txt.startswith(";"):
        txt = txt.split("\n",1)[1].lstrip()
    if not txt:
        raise EOFError()
    elif txt[0] == "(":
        txt = txt[1:].lstrip()
        item = []
        while txt[0]!=")":
            it, txt = s_read2(txt)
            item.append(it)
        txt = txt[1:]
    elif txt[0] == '"':
        item, txt = txt[1:].split('"',1)
        item = item.encode("utf8")
    elif txt[0].isalpha():
        item = re.match("[$a-zA-Z_\-\?!0-9]+",txt).group()
        txt = txt[len(item):]
    elif txt[0].isdigit():
        item = re.match("[0-9]+",txt).group()
        txt = txt[len(item):]
        item = int(item)
    else:
        raise RuntimeError(txt)
        assert 0
    assert item is not None
    return item, txt.lstrip()


def s_read(txt):
    ret = ["begin"]
    while txt:
        try:
            item, txt = s_read2(txt)
        except EOFError:
            break
        ret.append(item)
    return ret


def main():
    env = Env(None)
    for k,v in globals().items():
        if k.startswith("b_"):
            env.set(k[2:], v)
    prog = s_read(open("slip_4.slip").read())
    pprint.pprint(prog)
    print(b_eval(env, prog))


if __name__ == '__main__':
    main()
