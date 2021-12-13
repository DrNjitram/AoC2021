
def part1():
    file = open('day3.txt', 'r')

    gamma = ""
    epsilon = ""

    lines = file.readlines()
    lines = [line.strip() for line in lines]
    length = len(lines[0])

    for n in range(length):
        count1 = 0
        count0 = 0
        for line in lines:
            if line[n] == "1":
                count1 += 1

            else:
                count0 += 1

        if count1 > count0:
            gamma += "1"
            epsilon += "0"

        else:
            gamma += "0"
            epsilon += "1"

    print(gamma)
    print(epsilon)
    power = int(gamma, 2) * int(epsilon, 2)

    print(power)

    file.close()


part1()


def part2():
    file = open('day3.txt', 'r')

    oxygen = ""
    scrubber = ""

    lines = file.readlines()
    lines = [line.strip() for line in lines]
    length = len(lines[0])
    oxylines = lines[:]
    scrublines = lines[:]

    for n in range(length):
        count1 = 0
        count0 = 0

        # oxygen
        for line in oxylines:
            if line[n] == "1":
                count1 += 1

            else:
                count0 += 1

        newoxylines = []
        if count1 >= count0:
            for line in oxylines:
                if line[n] == "1":
                    newoxylines.append(line)
            oxylines = newoxylines

        else:
            for line in oxylines:
                if line[n] == "0":
                    newoxylines.append(line)
            oxylines = newoxylines

        if len(newoxylines) == 1:
            oxygen = newoxylines[0]

        count1 = 0
        count0 = 0

        # scrubber
        for line in scrublines:
            if line[n] == "1":
                count1 += 1

            else:
                count0 += 1

        newscrublines = []
        if count0 <= count1:
            for line in scrublines:
                if line[n] == "0":
                    newscrublines.append(line)
            scrublines = newscrublines

        else:
            for line in scrublines:
                if line[n] == "1":
                    newscrublines.append(line)
            scrublines = newscrublines

        if len(newscrublines) == 1:
            scrubber = newscrublines[0]

    print(oxygen)
    print(scrubber)
    life_support = int(oxygen, 2) * int(scrubber, 2)

    print(life_support)

    file.close()


part2()
