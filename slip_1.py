
# basic evaluation, lambda, apply etc

import collections


def sp_eval( env, expr ):
    # bytes==string, string==identifier
    if isinstance(expr, (bytes,float,int)):
        return expr
    elif isinstance(expr, str):
        try:
            return env[expr][-1]
        except IndexError:
            raise RuntimeError(expr)
    elif isinstance(expr, list):
        func = sp_eval(env,expr[0])
        if isinstance(func, Lambda):
            fargs = [sp_eval(env,e) for e in expr[1:]]
            return sp_apply(env, func, fargs)
        elif callable(func):
            return func(env, expr)
        else:
            raise RuntimeError(str(expr) + str(func))
    else:
        assert(0)


def sp_apply(env, func, args):
    assert(len(args)==len(func.args))
    for n,v in zip(func.args, args):
        env[n].append(v)
    r = sp_eval(env, func.body)
    for n in func.args:
        env[n].pop()
    return r


def b_begin(env, args):
    r = None
    for e in args[1:]:
        r = sp_eval(env, e)
    return r


def b_define(env, args):
    assert(len(args)==3 and type(args[1])==str)
    env[args[1]].append( sp_eval(env,args[2]) )


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
    def __init__(self, args, body):
        self.args, self.body = args, body


def b_lambda(env, args):
    assert(len(args)==3)
    return Lambda(args[1],args[2])


def main():
    prog = ["begin",
        ["define", "x", 10],
        ["define", "y", 20],
        [["lambda", ["x", "z"], ["mul", "x", ["plus", "x", "z"]]], 13, 17],
        ["define", "test", ["lambda", ["a", "b", "c"],
            ["plus", ["mul","a","a"], ["mul","b","b"], "c"]]],
        ["test", 11,21,31],
    ]
    env = collections.defaultdict(list)
    for k,v in globals().items():
        if k.startswith("b_"):
            env[k[2:]].append(v)
    print(sp_eval(env, prog))


if __name__ == '__main__':
    main()
