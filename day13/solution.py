import json
import itertools
import functools


def pairs(iter):
    next = []

    for i in iter:
        next += [i]

        if len(next) == 2:
            yield next
            next = []


def get_arrays1(c: str) -> list:
    out = []
    for l, r in pairs(filter(lambda x: bool(x), c.splitlines())):
        out += [(json.loads(l), json.loads(r))]

    return out


def get_arrays2(c: str) -> list:
    out = []
    for a in filter(lambda x: bool(x), c.splitlines()):
        out += [json.loads(a)]

    return out


def is_same(left: list, right: list) -> bool | None:
    index = 0
    for left_, right_ in zip(left, right):
        index += 1
        if isinstance(left_, list) and isinstance(right_, list):
            maybe = is_same(left_, right_)
            if maybe is not None:
                return maybe
            continue

        if isinstance(left_, list):
            maybe = is_same(left_, [right_])
            if maybe is not None:
                return maybe
            continue

        if isinstance(right_, list):
            maybe = is_same([left_], right_)
            if maybe is not None:
                return maybe
            continue

        if left_ < right_:
            return True
        if right_ < left_:
            return False

    if len(left) != len(right):
        if index >= len(left):
            return True
        if index >= len(right):
            return False

    return None


def sum(iter):
    out = 0
    for count, val in enumerate(iter):
        if val:
            out += count + 1

    return out


def to_int(f):
    @functools.cmp_to_key
    def inner(*args, **kwargs):
        n = f(*args, **kwargs)
        if n:
            return 1
        else:
            return -1

    return inner


def main():
    with open("input.txt") as f:
        content = f.read()

    print("Part One:", sum(map(lambda a: is_same(*a), get_arrays1(content))))

    arrays2 = get_arrays2(content) + [[[2]]] + [[[6]]]
    list.sort(arrays2, key=to_int(is_same), reverse=True)

    res = 1

    for i, value in enumerate(arrays2):
        if value == [[6]]:
            res *= (i+1)
        if value == [[2]]:
            res *= (i+1)

    print("Part Two:", res)


main()
