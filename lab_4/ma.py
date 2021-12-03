import enum
import re

KEYWORDS = ["VAR", "END", "BEGIN", "LOGICAL"]
# ^\s*(?!\s*VAR\s+)(?!\s*END\s+)\s*[A-Z]+
WORDS = r"%s\s*[A-Za-z]+" % "".join([rf"(?!\s*{i}\s+)" for i in KEYWORDS])
TYPE_WORD_LIST = ['LOGICAL']
TYPE_WORD =  r"%s\s*[A-Za-z]+" % "".join([rf"(?!\s*{i}\s+)" for i in KEYWORDS if i not in TYPE_WORD_LIST])
LITERAL = r"(?:0|1)"
PARAM_TYPE = r"(?:LOGICAL)"
UNARY_OPs = frozenset([r".NOT."])
UNARY = rf"(?:%s)" % "|".join([i.replace('.', r'\.') for i in UNARY_OPs])
BINARY_OPs = frozenset([r"\.OR\.", r"\.IMP\.", r"\.AND\."])
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
    unr = "unr"  # Состояние после унарной операции
    s98 = "s98"  # Ожидаем окончания программы
    s99 = "s99"  # Конечное состояние


class ShopOperation(enum.Enum):
    add = "+"
    del_ = "-"
    change = "-+"


S = Stats
SOp = ShopOperation

START_STATS = [S.s00]
FINISH_STATS = [S.s99]
SHOP_DEEP = 12

ALL_SHOP_STATES = [".NOT.", "(", None]


def exclude_shop_stats(*args):
    return [i for i in ALL_SHOP_STATES if i not in args]


graph = {
    ("VAR", S.s00, None): (S.s11, None, None),
    (rf"{WORDS}", S.s11, None): (S.s11, None, None),
    (r",", S.s11, None): (S.s12, None, None),
    (rf"{WORDS}", S.s12, None): (S.s11, None, None),
    (r"\:", S.s11, None): (S.s13, None, None),
    (rf"{PARAM_TYPE}", S.s13, None): (S.s14, None, None),
    (rf";", S.s14, None): (S.s20, None, None),
    (rf"BEGIN", S.s20, None): (S.s21, None, None),
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

    (rf"{BINARY}", S.s32, None): (S.s33, None, None),
    (rf"{BINARY}", S.s32, "("): (S.s33, None, None),

    (rf"\)", S.s32, "("): (S.s32, SOp.del_, None),
    (rf";", S.s32, None): (S.s21, None, None),

    (rf"{WORDS}", S.s33, "("): (S.s34, None, None),
    (rf"{WORDS}", S.s33, None): (S.s23, None, None),
    (rf"{LITERAL}", S.s33, "("): (S.s34, None, None),
    (rf"{LITERAL}", S.s33, None): (S.s23, None, None),
    (rf"\(", S.s33, "("): (S.s31, SOp.add, None),
    (rf"\(", S.s33, None): (S.s31, SOp.add, None),

    (rf"\)", S.s34, "("): (S.s32, SOp.del_, None),

    # (rf"{WORDS}", S.s30, "("): (S.s23, None, None),
    # (rf"{LITERAL}", S.s30, "("): (S.s23, None, None),
    # (rf"{WORDS}", S.s30, UNARY_OPERATIONS): (S.s30, SOp.del_, None),
    # (rf"{LITERAL}", S.s30, UNARY_OPERATIONS): (S.s23, SOp.del_, None),

    (r";", S.s23, None): (S.s21, None, None),
    # (r";", S.s23, None): (S.s98, None, None),
    (r"END", S.s21, None): (S.s99, None, None)
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
    # print(current_state, shop_state, data)
    current_choices: list[tuple] = [(key, val, reg) for key, val in graph.items() if
                                    key[1] == current_state and key[2] == shop_state and (
                                        reg := re.search(
                                             rf"^\s*((?:{key[0]}))\s*(.*)$",
                                            data
                                        )
                                    )
                                    ]
    assert len(current_choices) == 1, f"Множественный Выбор: {str(current_choices)}, {current_state}, {shop_state}"
    # print(data[0], _r, current_choices[0][2][1])
    return current_choices[0]


def parser(data: str):
    current_state: Stats = S.s00
    shop = [None]
    while current_state not in FINISH_STATS:
        print(data, shop)
        key, val, reg, *_ = _get_graph_state(current_state, shop[-1], data)
        data = reg[2]
        current_state = val[0]
        print(data)
        # print(data[0], key, val, [data[0]])
        if val[1] == SOp.add:
            shop.append(reg[1])
        elif val[1] == SOp.del_:
            shop.pop(-1)
        elif val[1] == SOp.change:
            shop[-1] = reg[1]
        assert 0 < len(shop) < SHOP_DEEP, f"Магазин либо переполнен, либо попытка удалить пустой магазин {shop}"
    assert shop == [None], "Магазин должен быть пустым в конце программы"


parser("VAR SDFG : LOGICAL; BEGIN AS = (.NOT. (1 .OR. (SDFF .OR. 0))) ; END  ")

def test():
    parser("VAR SDFG : LOGICAL; BEGIN AS = 1; END  ")
    parser("VAR SDFG, SDF, SDF : LOGICAL; BEGIN AS = 1; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = 1; DE = 0; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = JSDFSEGF; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = 1; AS = 1; AS = 1; AS = 1; AS = 1; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = .NOT. 1 ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = 1 .AND. SD ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = 0 .OR. 1 ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = SDF .IMP. 1 ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = .NOT. 1 .AND. 0 ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = (1) ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = .NOT. (1) ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = (.NOT. 1) ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = (1 .AND. 0) ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = (.NOT. 1 .AND. 0) ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = (((((0))))) ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = (.NOT. (1)) ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = (.NOT. (1 .OR. SDFF)) ; END  ")
    parser("VAR SDFG : LOGICAL; BEGIN AS = (.NOT. (1 .OR. (SDFF .OR. 0))) ; END  ")









