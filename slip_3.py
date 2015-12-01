
# replace dynamic scoping with lexical


import re


class Symtab:
    def __init__(self, parent=None):
        self.keys = {}
        self.parent = parent

    def get(self, key):
        tab = self
        while tab:
            v = tab.keys.get(key)
            if v:
                return v
            tab = tab.parent
        raise KeyError(key)

    def set(self, key, val):
        assert self.keys.get(key) is None
        self.keys[key] = val

    def pop(self, key):
        self.keys.pop(key)


def sp_eval(env, expr):
    if isinstance(expr, (bytes,float,int)):
        return expr
    elif isinstance(expr, str):
        return env.get(expr)
    elif isinstance(expr, list):
        func = sp_eval(env,expr[0])
        if isinstance(func, Lambda):
            fargs = [sp_eval(env,e) for e in expr[1:]]
            return sp_apply(func.env, func, fargs)
        elif callable(func):
            return func(env, expr)
        else:
            raise RuntimeError(str(expr) + str(func))
    else:
        assert(0)


def sp_apply(env, func, args):
    assert(len(args)==len(func.args))
    for n,v in zip(func.args, args):
        env.set(n, v)
    r = sp_eval(env, func.body)
    for n in func.args:
        env.pop(n)
    return r


def b_begin(env, args):
    r = None
    env2 = Symtab(env)
    for e in args[1:]:
        r = sp_eval(env2, e)
    return r


def b_define(env, args):
    assert(len(args)==3 and type(args[1])==str)
    env.set(args[1], sp_eval(env,args[2]))


def b_plus(env, args):
    acc = sp_eval(env, args[1])
    for a in args[2:]:
        acc += sp_eval(env, a)
    return acc


def b_mul(env, args):
    acc = sp_eval(env, args[1])
    for a in args[2:]:
        acc *= sp_eval(env, a)
    return acc


class Lambda:
    def __init__(self, args, body, env):
        self.args, self.body, self.env = args, body, env


def b_lambda(env, args):
    assert(len(args)==3)
    return Lambda(args[1],args[2], Symtab(env))


def b_print(env, args):
    print(" ".join(str(sp_eval(env,a)) for a in args[1:]))
    return None


def s_read2(txt):
    txt = txt.lstrip()
    while txt[0] == ";":
        txt = txt.split("\n",1)[1].lstrip()
    if txt[0] == "(":
        txt = txt[1:].lstrip()
        item = []
        while txt[0]!=")":
            it, txt = s_read2(txt)
            item.append(it)
        txt = txt[1:]
    elif txt[0] == '"':
        item, txt = txt[1:].split('"',1)
        item = item.encode("ascii")
    elif txt[0].isalpha():
        item = re.match("[a-zA-Z]+",txt).group()
        txt = txt[len(item):]
    elif txt[0].isdigit():
        item = re.match("[0-9]+",txt).group()
        txt = txt[len(item):]
        item = int(item)
    else:
        raise RuntimeError(txt)
        assert 0
    assert item
    return item, txt.lstrip()


def s_read(txt):
    ret = ["begin"]
    while txt:
        item, txt = s_read2(txt)
        ret.append(item)
    return ret


def main():
    env = Symtab()
    for k,v in globals().items():
        if k.startswith("b_"):
            env.set(k[2:], v)
    prog = s_read(open("slip_3.slip").read())
    print(prog)
    print(sp_eval(env, prog))


if __name__ == '__main__':
    main()
