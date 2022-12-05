part1 = max(
    sum(int(x) for x in group.split("\n") if x)
    for group in open("input.txt").read().split("\n\n")
)

part2 = sum(
    sorted(
        sum(int(x) for x in group.split("\n") if x)
        for group in open("input.txt").read().split("\n\n")
    )[-3:]
)
