print('why')

def part1():
    file = open('day4.txt', 'r')

    lines = file.readlines()
    bingoset = lines[0].strip().split(",")

    bingocards = lines[1:]

    for i,line in enumerate(bingocards):
        if line == "\n":
            bingocard = bingocards[i+1:i+6]
            print(bingocard)
            bingocard = [line.strip().split() for line in bingocard]
            print(bingocard)
            for n in bingoset:
                for y,row in enumerate(bingocard):
                    for x,number in enumerate(row):
                        if number == n:
                            bingocard[y][x] = "x"
            print(bingocard)
            exit(0)

    #for n in bingoset:

part1()