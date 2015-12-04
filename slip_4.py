
# add fexprs


import re
import pprint


class Symtab:
    def __init__(self, parent=None):
        self.keys = {}
        self.parent = parent

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

    def pop(self, key):
        self.keys.pop(key)


class Operative:
    pass


class Applicative:
    pass


class Lambda:
    def __init__(self, args, body):
        self.args, self.body = args, body

    def __call__(self, env, *args):
        assert(len(self.args)==len(args))
        #print("LAM", list(zip(self.args, args)))
        lenv = Symtab(env)
        for n,v in zip(self.args, args):
            lenv.set(n, b_eval(env,v))
        return b_eval(lenv, self.body)


class Builtin:
    def __init__(self, f):
        self.func = f


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


def b_begin(env, *args):
    r = None
    lenv = Symtab(env)
    for e in args:
        r = b_eval(lenv, e)
    return r


def b_define(env, sym, val):
    env.set(sym, b_eval(env,val))


def b_vau(env, args):
    assert(len(args)==3 and type(args[1])==str)
    env.set(args[1], b_eval(env,args[2]))


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


def b_func(env, name, args, body):
    l = Lambda(args,body)
    env.set(name, l)
    return l


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


def b_lambda(env, args, body):
    return Lambda(args, body)


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
    env = Symtab()
    for k,v in globals().items():
        if k.startswith("b_"):
            env.set(k[2:], v)
    prog = s_read(open("slip_4.slip").read())
    #pprint.pprint(env.keys)
    pprint.pprint(prog)
    print(b_eval(env, prog))


if __name__ == '__main__':
    main()
