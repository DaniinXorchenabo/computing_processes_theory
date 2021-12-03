import enum
import re

KEYWORDS = ["VAR", "END", "BEGIN", "LOGICAL"]
# ^\s*(?!\s*VAR\s+)(?!\s*END\s+)\s*[A-Z]+
WORDS = r"%s\s*[A-Za-z]+" % "".join([rf"(?!\s*{i}(?:[^A-Za-z]+|$))" for i in KEYWORDS])
TYPE_WORD_LIST = ['LOGICAL']
TYPE_WORD =  r"%s\s*[A-Za-z]+" % "".join([rf"(?!\s*{i}(?:[^A-Za-z]+|$))" for i in KEYWORDS if i not in TYPE_WORD_LIST])
LITERAL = r"(?:0|1)"
PARAM_TYPE = r"(?:LOGICAL)"
UNARY_OPs = frozenset([r".NOT."])
UNARY = rf"(?:%s)" % "|".join([i.replace('.', r'\.') for i in UNARY_OPs])
BINARY_OPs = frozenset([r".OR.", r".IMP.", r".AND."])
# BINARY = r"(?:%s)" % r"|".join([i.replace(r'.', r'.') for i in BINARY_OPs])
BINARY = r"(?:\.OR\.|\.IMP\.|\.AND\.)"


class Stats(enum.Enum):
    s00 = "s00"  # стартовое состояние
    s11 = "s11"  # объявление переменной
    s12 = "s12"  # после получения запятой ждём считывания еще одного слова
    s13 = "s13"  # Ожидаем считывания типа переменной
    s14 = "s14"  # Ожидаем завершения части программы головы
    s20 = "s20"  # Ожидаем начала тела программы
    s21 = "s21"  # считываем инициалирируемую переменную
    s22 = "s22"  # Ожидаем считывание знака равенства для инициализируемой переменной
    s23 = "s23"  # ожидаем конца выражения, т.е. ;
    s30 = "s30"  # Начинаем считывание выражения при присваивании
    s31 = "s31"  # Выражение в процессе считывания
    s32 = "s32"  # Ввели первую часть скобки
    s33 = "s33"  # Поиск бинарной операции
    s34 = "s34"  # Часть условия со скобками, которая идёт после бинарной операции
    s35 = 's35'  # Часть условия со скобками с учётом бинарного типа в магазине
    s98 = "s98"  # Ожидаем окончания программы
    s99 = "s99"  # Конечное состояние


class ShopOperation(enum.Enum):
    add = "+"
    del_ = "-"
    change = "-+"


class ReadDataStatus(enum.Enum):
    not_read = 'not_read'


S = Stats
SOp = ShopOperation

START_STATS = [S.s00]
FINISH_STATS = [S.s99]
SHOP_DEEP = 12

ALL_SHOP_STATES = [".NOT.", "(", None]


variable_declaration_stats = [S.s11, S.s12]
variable_init_stats = [S.s21]
init_variable_end_stats = [S.s32, S.s23]
use_variable_stats = [S.s30, S.s31, S.s33]



def exclude_shop_stats(*args):
    return [i for i in ALL_SHOP_STATES if i not in args]


graph = {
    (r"VAR\s+", S.s00, None): (S.s11, None, None),
    (rf"{WORDS}", S.s11, None): (S.s11, None, None),
    (r",", S.s11, None): (S.s12, None, None),
    (rf"{WORDS}", S.s12, None): (S.s11, None, None),
    (r"\:", S.s11, None): (S.s13, None, None),
    (rf"{PARAM_TYPE}", S.s13, None): (S.s14, None, None),
    (rf";", S.s14, None): (S.s20, None, None),
    (rf"BEGIN\s+", S.s20, None): (S.s21, None, None),
    (rf"{WORDS}", S.s21, None): (S.s22, None, None),
    (rf"=", S.s22, None): (S.s30, None, None),

    (rf"{WORDS}", S.s30, None): (S.s32, None, None),
    (rf"{WORDS}", S.s30, UNARY_OPs): (S.s32, SOp.del_, None),
    (rf"{LITERAL}", S.s30, None): (S.s32, None, None),
    (rf"{LITERAL}", S.s30, UNARY_OPs): (S.s32, SOp.del_, None),

    (rf"{UNARY}", S.s30, None): (S.s30, SOp.add, None),

    (rf"\(", S.s30, None): (S.s31, SOp.add, None),
    (rf"\(", S.s30, UNARY_OPs): (S.s31, SOp.change, None),

    (rf"{UNARY}", S.s31, "("): (S.s31, SOp.add, None),
    (rf"\(", S.s31, "("): (S.s31, SOp.add, None),
    (rf"\(", S.s31, UNARY_OPs): (S.s31, SOp.change, None),

    (rf"{WORDS}", S.s31, "("): (S.s32, None, None),
    (rf"{WORDS}", S.s31, UNARY_OPs): (S.s32, SOp.del_, None),

    (rf"{LITERAL}", S.s31, "("): (S.s32, None, None),
    (rf"{LITERAL}", S.s31, UNARY_OPs): (S.s32, SOp.del_, None),

    (rf"{BINARY}", S.s32, None): (S.s33, SOp.add, None),
    (rf"{BINARY}", S.s32, "("): (S.s33, SOp.add, None),

    (rf"\)", S.s32, "("): (S.s32, SOp.del_, None),
    (rf"\)", S.s32, BINARY_OPs): (S.s32, SOp.del_, ReadDataStatus.not_read),
    (rf";", S.s32, None): (S.s21, None, None),

    # (rf"{WORDS}", S.s33, "("): (S.s34, None, None),
    (rf"{WORDS}", S.s33, BINARY_OPs): (S.s35, None, None),
    # (rf"{WORDS}", S.s33, None): (S.s23, None, None),
    # (rf"{LITERAL}", S.s33, "("): (S.s34!, None, None),
    (rf"{LITERAL}", S.s33, BINARY_OPs): (S.s35, None, None),
    # (rf"{LITERAL}", S.s33, None): (S.s23, None, None),
    (rf"\(", S.s33, BINARY_OPs): (S.s31, SOp.add, None),
    # (rf"\(", S.s33, "("): (S.s31, SOp.add, None),
    # (rf"\(", S.s33, None): (S.s31, SOp.add, None),

    (rf"\)", S.s34, "("): (S.s32, SOp.del_, None),

    (rf";", S.s35, BINARY_OPs): (S.s23, SOp.del_, ReadDataStatus.not_read),
    (rf"\)", S.s35, BINARY_OPs): (S.s34, SOp.del_, ReadDataStatus.not_read),

    # (rf"{WORDS}", S.s30, "("): (S.s23, None, None),
    # (rf"{LITERAL}", S.s30, "("): (S.s23, None, None),
    # (rf"{WORDS}", S.s30, UNARY_OPERATIONS): (S.s30, SOp.del_, None),
    # (rf"{LITERAL}", S.s30, UNARY_OPERATIONS): (S.s23, SOp.del_, None),

    (r";", S.s23, None): (S.s21, None, None),
    (r";", S.s23, BINARY_OPs): (S.s21, SOp.del_, None),
    # (r";", S.s23, None): (S.s98, None, None),
    (r"END\s*$", S.s21, None): (S.s99, None, None)
}
new_graph = dict()
for _key, _val in graph.items():
    if isinstance(_key[2], (list, set, frozenset, tuple)):
        for new_key in _key[2]:
            _n_key = (_key[0], _key[1], new_key, *_key[2:])
            assert _n_key not in new_graph
            new_graph[_n_key] = _val
    else:
        assert _key not in new_graph, f"{_key} -> {_val} ||{new_graph[_key]}"
        new_graph[_key] = _val

graph = new_graph
# print(*graph.items(), sep='\n')

def _get_graph_state(current_state: Stats, shop_state, data: str) -> tuple:
    reg = None
    print(current_state,"|", shop_state,"|", data)
    current_choices: list[tuple] = [(key, val, reg) for key, val in graph.items() if
                                    key[1] == current_state and key[2] == shop_state and (
                                        reg := re.search(
                                            (_r := rf"^\s*((?:{key[0]}))\s*(.*)$"),
                                            data
                                        )
                                    ) and (_rr := _r)
                                    ]
    assert len(current_choices) == 1, f"Множественный Выбор: {str(current_choices)}, {current_state}, {shop_state}"
    reg = current_choices[0][2]
    # print(((reg[0], reg[1]) if reg else ""), reg, current_choices[0][2][1], _rr)
    return current_choices[0]


def parser(data: str):
    VARIABLES = dict()
    current_state: Stats = S.s00
    shop = [None]
    while current_state not in FINISH_STATS:
        print(data, shop)
        key, val, reg, *_ = _get_graph_state(current_state, shop[-1], data)

        if re.search(rf"^\s*{WORDS}\s*$", ' ' + reg[1] + ' ') and  current_state in variable_declaration_stats:
            print(VARIABLES)
            assert reg[1] not in VARIABLES, "Переменная объявлена дважды"
            VARIABLES[reg[1]] = None
        if re.search(rf"^\s*{WORDS}\s*$", ' ' + reg[1] + ' ') and current_state in variable_init_stats:
            assert reg[1] in VARIABLES, "Переменная не была объявлена"
            VARIABLES[reg[1]] = S.s21
        if re.search(rf"^\s*{WORDS}\s*$", ' ' + reg[1] + ' ') and current_state in use_variable_stats:
            assert reg[1] in VARIABLES, "Переменная не была объявлена"
            assert VARIABLES.get(reg[1], None) is not None and not isinstance(VARIABLES.get(reg[1], None), Stats), "Переменная используется, хотя была объявлена, но не инициализирована"
        if current_state in init_variable_end_stats:
            if bool(_keys := [key for key, val in VARIABLES.items() if isinstance(val, Stats)]):
                VARIABLES[_keys[0]] = True

        if val[2] != ReadDataStatus.not_read:
            data = reg[2]
        current_state = val[0]
        print(data, VARIABLES)



        # print(data[0], key, val, [data[0]])
        if val[1] == SOp.add:
            shop.append(reg[1])
        elif val[1] == SOp.del_:
            shop.pop(-1)
        elif val[1] == SOp.change:
            shop[-1] = reg[1]
        assert 0 < len(shop) < SHOP_DEEP, f"Магазин либо переполнен, либо попытка удалить пустой магазин {shop}"
    assert shop == [None], "Магазин должен быть пустым в конце программы"



# test()







