graph = {
    'S': {3: "A"},
    'A': {4: "Z"},
    'Z': {4: "JC"},
    'JC': {3: "D", 4: "K"},
    'D': {3: "G"},
    'G': {4: "H"},
    'H': {3: "Z"},
    'K': {3: "M"},
    'M': {3: "N"},
    'N': {3: "Z"},
}


def main():
    while True:
        try:
            current = graph['S']
            data = input()
            assert bool(data)
            # print("S", end=' ')
            char = None
            for i in data:
                if i.isdigit() and 3 <= (i := int(i)) <= 4:
                    current = graph[(char := current[i])]
                    # print(char, end=' ')
                else:
                    raise ValueError()
            # print('Цепочка проходит' if char in graph['Z'] else '2. Не достигнут конечный пункт')
            print('Цепочка проходит' if char == 'Z' else 'Не достигнут конечный пункт')
        except KeyError:
            print('Нет перехода')
        except AssertionError:
            print('Введена пустая строка')
        except ValueError:
            print('Введен некорректный символ')


if __name__ == '__main__':
    main()
