with open("input.txt") as f:
    input = f.read()

import enum

# X LOSE
# Y DRAW
# Z WIN


class Shape(enum.Enum):
    ROCK = 1
    PAPER = 2
    SCISORS = 3


class WIN_DRAW(enum.Enum):
    LOSE = 0
    TIE = 3
    WIN = 6


shapes = []


for line in input.splitlines():
    a, b = line.split(" ")

    if a == "A":
        left = Shape.ROCK
    elif a == "B":
        left = Shape.PAPER
    elif a == "C":
        left = Shape.SCISORS
    else:
        raise RuntimeError("a: " + a)

    if b == "X":
        right = WIN_DRAW.LOSE
    elif b == "Y":
        right = WIN_DRAW.TIE
    elif b == "Z":
        right = WIN_DRAW.WIN
    else:
        raise RuntimeError("b: " + b)

    shapes.append((left, right))


score = 0

for left_shape, win_draw_tie in shapes:
    score += win_draw_tie.value

    if win_draw_tie == WIN_DRAW.TIE:
        shape = left_shape

    # find the shape
    elif left_shape == Shape.ROCK and win_draw_tie == WIN_DRAW.LOSE:
        shape = Shape.SCISORS
    elif left_shape == Shape.ROCK and win_draw_tie == WIN_DRAW.WIN:
        shape = Shape.PAPER

    elif left_shape == Shape.PAPER and win_draw_tie == WIN_DRAW.LOSE:
        shape = Shape.ROCK
    elif left_shape == Shape.PAPER and win_draw_tie == WIN_DRAW.WIN:
        shape = Shape.SCISORS

    elif left_shape == Shape.SCISORS and win_draw_tie == WIN_DRAW.LOSE:
        shape = Shape.PAPER
    elif left_shape == Shape.SCISORS and win_draw_tie == WIN_DRAW.WIN:
        shape = Shape.ROCK

    score += shape.value

print(score)
