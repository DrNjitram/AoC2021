print('hello world')


def part1():
    file = open('day1.txt', 'r')

    counter = 0
    previous_number = 10000

    for line in file:
        number = int(line)

        if number > previous_number:
            counter = counter+1

        previous_number = number

    print(counter)
    file.close()


part1()


def part1m(offset: int, lines):
    return sum([1 for index, line in enumerate(lines[:-offset]) if int(line) < int(lines[index + offset])])


def part2():
    file = open('day1.txt', 'r')

    counter = 0
    large_number = 1000000

    previous_previous_previous_number = 1000000
    previous_previous_number = 1000000
    previous_number = 1000000

    for line in file:
        number = int(line)

        if number+previous_number+previous_previous_number > previous_number+previous_previous_number+previous_previous_previous_number:
            counter = counter+1

        previous_previous_previous_number = previous_previous_number
        previous_previous_number = previous_number
        previous_number = number

    print(counter)
    file.close()

lines = open('day1.txt', 'r').readlines()
print(part1m(1, lines))
print(part1m(3, lines))





