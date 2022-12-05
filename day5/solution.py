from collections import defaultdict


def parse_stacks(text: str) -> dict[int, list[str]]:
    stacks: dict[int, list[str]] = defaultdict(list)
    for line in text.splitlines():
        if not line:
            break
        for count, letter in enumerate(line):
            if not letter.isalpha():
                continue
            col = count // 4 + 1
            stacks[col] += [letter]
    return stacks


def parse_line(line: str) -> tuple[int, int, int]:
    line = line.replace("move ", "").replace("from ", "").replace("to ", "")
    return tuple(map(int, line.split(" ")))  # type: ignore


def crate_mover_9000(text: str, stacks: dict[int, list[str]]):
    lines = text.splitlines()
    at_breakpoint = False
    for line in lines:
        if not line:
            at_breakpoint = True
            continue
        if not at_breakpoint:
            continue

        amount, f, to = parse_line(line)

        popping = stacks[f][:amount]
        del stacks[f][:amount]

        stacks[to] = list(reversed(popping)) + stacks[to]


def crate_mover_9001(text: str, stacks: dict[int, list[str]]):
    lines = text.splitlines()
    at_breakpoint = False
    for line in lines:
        if not line:
            at_breakpoint = True
            continue
        if not at_breakpoint:
            continue

        amount, f, to = parse_line(line)

        popping = stacks[f][:amount]
        del stacks[f][:amount]

        stacks[to] = popping + stacks[to]


def print_dict_list(stacks: dict[int, list[str]]):
    out = list()
    for _ in stacks.items():
        out.append("")

    for index, stack in stacks.items():
        out[index - 1] = stack[0]

    print("".join(out))


with open("input.txt", "r") as f:
    text = f.read()

stacks = parse_stacks(text)
crate_mover_9000(text, stacks)
print_dict_list(stacks)

stacks = parse_stacks(text)
crate_mover_9001(text, stacks)
print_dict_list(stacks)
