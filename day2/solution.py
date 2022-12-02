with open("input.txt") as f:
    input = f.read()

import enum

# X Rock
# Y Paper
# Z Scisors


class Shape(enum.Enum):
    ROCK = 1
    PAPER = 2
    SCISORS = 3


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
        right = Shape.ROCK
    elif b == "Y":
        right = Shape.PAPER
    elif b == "Z":
        right = Shape.SCISORS
    else:
        raise RuntimeError("b: " + b)

    shapes.append((left, right))


score = 0

for left_shape, right_shape in shapes:
    score += right_shape.value

    if left_shape == right_shape:
        score += 3
        continue

    if left_shape == Shape.ROCK and right_shape == Shape.PAPER:
        score += 6
        continue
    if left_shape == Shape.ROCK and right_shape == Shape.SCISORS:
        continue

    if left_shape == Shape.PAPER and right_shape == Shape.ROCK:
        continue
    if left_shape == Shape.PAPER and right_shape == Shape.SCISORS:
        score += 6
        continue

    if left_shape == Shape.SCISORS and right_shape == Shape.ROCK:
        score += 6
        continue
    if left_shape == Shape.SCISORS and right_shape == Shape.PAPER:
        continue

print(score)
