print('hewwo :3')


def part1():
    file = open('day2.txt', 'r')

    rise = 0
    sink = 0
    forwards = 0
    result = 0

    for line in file:
        line.split(' ')
        print(line.split(' '))

        direction = line.split(" ")[0]
        value = line.split(" ")[1]

        if direction == 'forward':
            forwards = forwards + int(value)

        if direction == 'up':
            rise = rise + int(value)

        if direction == 'down':
            sink = sink + int(value)

        depth = sink - rise
        result = depth * forwards

    print(result)
    file.close()


part1()


def part2():
    file = open('day2.txt', 'r')

    aim = 0
    forwards = 0
    result = 0
    depth = 0

    for line in file:
        print(line.split(' '))

        direction = line.split(" ")[0]
        X = int(line.split(" ")[1])

        if direction == 'up':
            aim = aim - X

        elif direction == 'down':
            aim = aim + X

        elif direction == 'forward':
            forwards = forwards + X
            depth = depth + aim*X

        result = depth * forwards

    print(result)
    file.close()


part2()




