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
            # print("S", end=' ')
            char = None
            for i in data:
                if i.isdigit() and 0 < (i := int(i)) < 4:
                    current = graph[(char := current[i - 1])]
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


# def ggg():
#     try:
#         current = graph['S']
#         data = input()
#         assert bool(data)
#         char = None
#         for i in data:
#             if i.isdigit() and 0 < (i := int(i)) < 4:
#                 current = graph[(char := current[i - 1])]
#                 print(char, end=' ')
#             else:
#                 raise ValueError()
#         print('Цепочка проходит' if char == 'Z' else '2.) Не достигнут конечный пункт')
#     except KeyError:
#         print('Нет перехода')
#     except AssertionError:
#         print('Введена пустая строка')
#     except ValueError:
#         print('Введен некорректный символ')


# from pycallgraph import PyCallGraph
# from pycallgraph.output import GraphvizOutput

# with PyCallGraph(output=GraphvizOutput()):
#     try:
#         current = graph['S']
#         data = input()
#         assert bool(data)
#         char = None
#         for i in data:
#             if i.isdigit() and 0 < (i := int(i)) < 4:
#                 current = graph[(char := current[i - 1])]
#                 print(char, end=' ')
#             else:
#                 raise ValueError()
#         print('Цепочка проходит' if char == 'Z' else '2.) Не достигнут конечный пункт')
#     except KeyError:
#         print('4.) Нет перехода')
#     except AssertionError:
#         print('1.) Введена пустая строка')
#     except ValueError:
#         print('3.) Введен некорректный символ')


# if __name__ == '__main__':
#     main()


# graph = {
#     'S': ["C", "B", "A"],
#     'A': [None, "Z", "A"],
#     'B': ['Z', "B", None],
#     'C': ['C', None, "Z"],
#     'Z': ['A', "C", "B"]
# }
# try:
#     current = graph['S']
#     data = input()
#     assert bool(data)
#     char = None
#     for i in data:
#         if i.isdigit() and 0 < (i := int(i)) < 4:
#             current = graph[(char := current[i - 1])]
#         else:
#             raise ValueError()
#     print('Цепочка проходит' if char == 'Z' else 'Не достигнут конечный пункт')
# except KeyError:
#     print('Нет перехода')
# except AssertionError:
#     print('Введена пустая строка')
# except ValueError:
#     print('Введен некорректный символ')

# from fastapi.responses import JSONResponse
# from fastapi import FastAPI
#
# try:
#     assert False, JSONResponse({"detail": "example"}, headers={})
# except AssertionError as e:
#     print(e.args)
#     # e.args[0]
#
# app = FastAPI()
# @app.exception_handler(AssertionError)
# async def unicorn_exception_handler(request: Request, exc: AssertionError):
#     return exc.args[0]