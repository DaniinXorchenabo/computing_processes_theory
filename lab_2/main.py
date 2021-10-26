graph = {
    'S': ["C", "B", "A"],
    'A': [None, "Z", "A"],
    'B': ['Z', "B", None],
    'C': ['C', None, "Z"],
    'Z': ['A', "C", "B"]

}


def main():
    while True:
        try:
            current = graph['S']
            data = input()
            assert bool(data)
            print("S", end=' ')
            char = None
            for i in data:
                if i.isdigit() and 0 < (i := int(i)) < 4:
                    current = graph[(char := current[i - 1])]
                    print(char, end=' ')
                else:
                    raise ValueError()
            # print('Цепочка проходит' if char in graph['Z'] else '2. Не достигнут конечный пункт')
            print('Цепочка проходит' if char == 'Z' else '2.) Не достигнут конечный пункт')
        except KeyError:
            print('4.) Нет перехода')
        except AssertionError:
            print('1.) Введена пустая строка')
        except ValueError:
            print('3.) Введен некорректный символ')


if __name__ == '__main__':
    main()
