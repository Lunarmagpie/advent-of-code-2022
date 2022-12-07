from __future__ import annotations
from typing import Any

DIRECTORY = {}
DIR = ""


def insert_file(file_name: str, file_size: int):
    next = DIRECTORY

    if not DIR:
        next[file_name] = file_size
        return
    for folder in DIR.split("/"):
        next = next[folder]

    next[file_name] = file_size


def get_commands(lines: str) -> list[list[str]]:
    out = []
    lines = lines.splitlines()

    for line in lines:
        if "$" in line:
            out.append([line])
        else:
            out[-1].append(line)

    return out


def handle_commands(commands: list[list[str]]):
    for command in commands:
        if command[0].startswith("$ ls"):
            handle_ls(command)
        else:
            handle_cd(command[0])


def handle_ls(command: list[str]):
    for f in command[1:]:
        words = f.split(" ")
        if "dir" == words[0]:
            insert_file(words[1], {})
        else:
            insert_file(words[1], int(words[0]))


def handle_cd(command: str):
    global DIR
    thingy, action, dir = command.split(" ")

    if dir == "..":
        *d, end = DIR.split("/")
        DIR = str("/".join(d))
    elif dir == "/":
        DIR = ""
    else:
        if len(DIR) == 0:
            DIR = dir
        else:
            DIR = f"{DIR}/{dir}"


def recursive_sum(dir: dict[str, Any] | int) -> int:
    if isinstance(dir, dict):
        total = 0
        for v in dir.values():
            total += recursive_sum(v)
        return total
    return dir


def sum_dirs(path: str, dir: dict[str, Any] | int) -> list[(str, int)]:

    if isinstance(dir, int):
        return []

    out = []

    for name, dir in dir.items():
        next_path = f"{path}{name}/"
        if isinstance(dir, int):
            continue
        out.append((next_path, recursive_sum(dir)))
        out.extend(sum_dirs(next_path, dir))

    return out


def main():
    with open("input.txt") as f:
        commands = get_commands(f.read())
    handle_commands(commands)

    res = sum_dirs("/", DIRECTORY)
    res.append(("/", recursive_sum(DIRECTORY)))

    total = 0

    for name, value in res:
        if value <= 100000:
            total += value

    print("part 1:", total)

    possibilities = []

    empty = 70000000 - recursive_sum(DIRECTORY)

    needed_space = 30000000 - empty

    for name, value in res:
        if value < needed_space:
            continue
        possibilities += [value]

    print("Part 2:", min(possibilities))


if __name__ == "__main__":
    main()
