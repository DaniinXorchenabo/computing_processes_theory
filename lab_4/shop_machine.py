import enum
import re

KEYWORDS = ["VAR", "END", "BEGIN", "LOGICAL"]
# ^\s*(?!\s*VAR\s+)(?!\s*END\s+)\s*[A-Z]+
WORDS = r"%s\s*[A-Za-z]{1,12}" % "".join([rf"(?!\s*{i}(?:[^A-Za-z]+|$))" for i in KEYWORDS])
TYPE_WORD_LIST = ['LOGICAL']
TYPE_WORD = r"%s\s*[A-Za-z]+" % "".join([rf"(?!\s*{i}(?:[^A-Za-z]+|$))" for i in KEYWORDS if i not in TYPE_WORD_LIST])
LITERAL_list = [0, 1]
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
    s15 = "s15"  # состояние после ввода слова
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
    (r"VAR\s+", S.s00, None): ((S.s11, None, None), f"Не получается найти корректное имя переменной\n"
                                                    f" (должно состоять из латинских букв \n"
                                                    f"и не являться ключевым словом \n"
                                                    f"({', '.join([f'`{i}`' for i in KEYWORDS])}))"),
    (rf"{WORDS}", S.s11, None): ((S.s15, None, None), "Не получается найти объявление следующей\n"
                                                      " переменной или `:` для типа переменных"),
    (r",", S.s15, None): ((S.s11, None, None), "Не получается найти объявление следующей переменной"),
    (r"\:", S.s15, None): ((S.s13, None, None), "Ожидаемый тип переменной не найден"),
    (rf"{PARAM_TYPE}", S.s13, None): ((S.s14, None, None), "Не найден ожидаемый символ `;` "),
    (rf";", S.s14, None): ((S.s20, None, None), "Ожидаемая операторная скобка `BEGIN` не найдена"),
    (rf"BEGIN\s+", S.s20, None): ((S.s21, None, None), f"Не найдено корректное название переменной \n"
                                                       f"(должно состоять из латинских букв \n"
                                                       f"и не являться ключевым словом\n"
                                                       f" ({', '.join([f'`{i}`' for i in KEYWORDS])}))"),
    (rf"{WORDS}", S.s21, None): ((S.s22, None, None), "Ожидаемый символ `=` не был найден"),
    (rf"=", S.s22, None): ((S.s30, None, None), (_t2 := f"Корректное слово (должно состоять из \n"
                                                        f"латинских букв и не являться\n"
                                                        f" ключевым словом \n({', '.join([f'`{i}`' for i in KEYWORDS])})),\n"
                                                        f" литерал ({', '.join([f'`{i}`' for i in LITERAL_list])}),\n"
                                                        f" открывающая скобка или \n"
                                                        f"унарная операция {', '.join([f'`{i}`' for i in UNARY_OPs])}\n "
                                                        f"не были найдена")),

    (rf"{WORDS}", S.s30, None): ((S.s32, None, None), (_t1 := f" Ожидается одна из бинарных операций \n"
                                                              f"({', '.join([f'`{i}`' for i in BINARY_OPs])}) \n"
                                                              f"или символ завершения строки (`;`) ")),
    (rf"{WORDS}", S.s30, UNARY_OPs): ((S.s32, SOp.del_, None), _t1),
    (rf"{LITERAL}", S.s30, None): ((S.s32, None, None), _t1),
    (rf"{LITERAL}", S.s30, UNARY_OPs): ((S.s32, SOp.del_, None), _t1),

    (rf"{UNARY}", S.s30, None): (
        (S.s30, SOp.add, None), (_t3 := f"Корректное слово (должно состоять из латинских букв\n"
                                        f" и не являться  ключевым словом \n"
                                        f"({', '.join([f'`{i}`' for i in KEYWORDS])})),\n"
                                        f" литерал ({', '.join([f'`{i}`' for i in LITERAL_list])}),\n"
                                        f" или открывающая скобка не были найдена")),

    (rf"\(", S.s30, None): ((S.s31, SOp.add, None), _t2),
    (rf"\(", S.s30, UNARY_OPs): ((S.s31, SOp.change, None), _t3),

    (rf"{UNARY}", S.s31, "("): ((S.s31, SOp.add, None), _t3),
    (rf"\(", S.s31, "("): ((S.s31, SOp.add, None), _t2),
    (rf"\(", S.s31, UNARY_OPs): ((S.s31, SOp.change, None), _t2),

    (rf"{WORDS}", S.s31, "("): ((S.s32, None, None), (_t4 := f" Ожидается одна из бинарных операций \n"
                                                             f"({', '.join([f'`{i}`' for i in BINARY_OPs])}) \n"
                                                             f", символ завершения строки (`;`) \n"
                                                             f"или , если магазин не пуст, закрывающая скобка (`)`) ")),
    (rf"{WORDS}", S.s31, UNARY_OPs): ((S.s32, SOp.del_, None), _t4),

    (rf"{LITERAL}", S.s31, "("): ((S.s32, None, None), _t4),
    (rf"{LITERAL}", S.s31, UNARY_OPs): ((S.s32, SOp.del_, None), _t4),

    (rf"{BINARY}", S.s32, None): ((S.s33, SOp.add, None), _t3),
    (rf"{BINARY}", S.s32, "("): ((S.s33, SOp.add, None), _t3),

    (rf"\)", S.s32, "("): ((S.s32, SOp.del_, None), _t4),
    (rf"\)", S.s32, BINARY_OPs): (
        (S.s32, SOp.del_, ReadDataStatus.not_read), (_t5 := "Ожидается символ завершения строки (`;`)\n"
                                                            " (если магазин пуст)\n"
                                                            f"или , если магазин не пуст, закрывающая скобка (`)`) ")),
    (rf";", S.s32, None): (
        (S.s21, None, None), "Ожидается либо закрывающая \nоператорная скобка `END` как конец строки \n"
                             "либо корректное имя переменной \n"
                             f"(должно состоять из латинских букв \n"
                             f"и не являться ключевым словом\n"
                             f" ({', '.join([f'`{i}`' for i in KEYWORDS])}))"),
    (rf";", S.s32, BINARY_OPs): ((S.s21, SOp.del_, None), "Ничего не ожидается, этот такт пустой"),

    (rf"{WORDS}", S.s33, BINARY_OPs): ((S.s35, None, None), _t5),
    (rf"{LITERAL}", S.s33, BINARY_OPs): ((S.s35, None, None), _t5),
    (rf"\(", S.s33, BINARY_OPs): ((S.s31, SOp.add, None), _t2),

    (rf"\)", S.s34, "("): ((S.s32, SOp.del_, None), (_t4 := f" Ожидается одна из бинарных операций, \n"
                                                            f"если ни одна из бинарных операций \n"
                                                            f"еще не была использована, \n"
                                                            f"({', '.join([f'`{i}`' for i in BINARY_OPs])}) \n"
                                                            f", символ завершения строки (`;`) \n"
                                                            f"или , если магазин не пуст, закрывающая скобка (`)`) ")),

    (rf";", S.s35, BINARY_OPs): ((S.s23, SOp.del_, ReadDataStatus.not_read), "Ничего не ожидается, этот такт пустой"),
    (rf"\)", S.s35, BINARY_OPs): ((S.s34, SOp.del_, ReadDataStatus.not_read), "Ничего не ожидается, этот такт пустой"),

    (r";", S.s23, None): (
        (S.s21, None, None), "Ожидается либо закрывающая \nоператорная скобка `END` как конец строки \n"
                             "либо корректное имя переменной \n"
                             f"(должно состоять из латинских букв \n"
                             f"и не являться ключевым словом"
                             f" ({', '.join([f'`{i}`' for i in KEYWORDS])}))"),
    (r";", S.s23, BINARY_OPs): ((S.s21, SOp.del_, None), "Ничего не ожидается, этот такт пустой"),
    (r"END\s*$", S.s21, None): ((S.s99, None, None), "Конечное состояние"),
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
    # print(current_state, "|", shop_state, "|", data)
    current_choices: list[tuple] = [(key, val, reg) for key, val in graph.items() if
                                    key[1] == current_state and key[2] == shop_state and (
                                        reg := re.search(
                                            (_r := rf"^\s*((?:{key[0]}))\s*(.*)$"),
                                            data
                                        )
                                    ) and (_rr := _r)
                                    ]
    assert len(current_choices) < 2, f"Множественный Выбор: {str(current_choices)}, {current_state}, {shop_state}"
    assert len(current_choices) > 0, f"Не найдено перехода!: {str(current_choices)}, {current_state}, {shop_state}"

    reg = current_choices[0][2]
    # print(((reg[0], reg[1]) if reg else ""), reg, current_choices[0][2][1], _rr)
    return current_choices[0]


def parser(data: str):
    last_data = data
    start_data_len = len(data)
    VARIABLES = dict()
    current_state: Stats = S.s00
    shop = [None]
    error_str = 'Строка должна начинаться с ключевого слова VAR'
    print(data)
    reg = '  '

    while current_state not in FINISH_STATS:
        # print(data, shop)
        try:
            key, [val, error_str], reg, *_ = _get_graph_state(current_state, shop[-1], data)
        except AssertionError as e:
            raise AssertionError(str(e) + "\n" + '\n\t'.join(
                [i.strip() for i in error_str.split('\n')]) + "\nОшибка парсинга: \n" + last_data + '\nМежду `' + reg[
                                     1] + "` и `" + data + '`') from e

        if re.search(rf"^\s*{WORDS}\s*$", ' ' + reg[1] + ' ') and current_state in variable_declaration_stats:
            # print(VARIABLES)
            assert reg[1] not in VARIABLES, "Переменная объявлена дважды"
            VARIABLES[reg[1]] = None
        if re.search(rf"^\s*{WORDS}\s*$", ' ' + reg[1] + ' ') and current_state in variable_init_stats:
            assert reg[1] in VARIABLES, "Переменная не была объявлена"
            if VARIABLES[reg[1]] is None:
                VARIABLES[reg[1]] = S.s21
        if re.search(rf"^\s*{WORDS}\s*$", ' ' + reg[1] + ' ') and current_state in use_variable_stats:
            assert reg[1] in VARIABLES, "Переменная не была объявлена"
            assert VARIABLES.get(reg[1], None) is not None and not isinstance(VARIABLES.get(reg[1], None),
                                                                              Stats), "Переменная используется, хотя была объявлена, но не инициализирована"
        if reg[1] == ";" and current_state in init_variable_end_stats:
            if bool(_keys := [key for key, val in VARIABLES.items() if isinstance(val, Stats)]):
                VARIABLES[_keys[0]] = True

        if val[2] != ReadDataStatus.not_read:
            last_data = data
            data = str(reg[2])
        current_state = val[0]
        print(data.rjust(start_data_len), "\t", current_state, "\t|", VARIABLES, "\t|", shop)

        if val[1] == SOp.add:
            shop.append(reg[1])
        elif val[1] == SOp.del_:
            shop.pop(-1)
        elif val[1] == SOp.change:
            shop[-1] = reg[1]
        assert 0 < len(shop), f"Попытка удалить конечный символ магазина: {shop}"
        assert len(shop) < SHOP_DEEP, f"Магазин переполнен: {shop}"
    assert shop == [None], f"Магазин должен быть пустым в конце программы: {shop}"
    return 'Цепочка подходит'


def test():
    parser("VAR AS : LOGICAL; BEGIN AS = 1; END  ")
    parser("VAR AS : LOGICAL;BEGIN AS = (1) .AND. 1;  END")
    parser("VAR AS : LOGICAL;BEGIN AS = .NOT. 0;  END")
    parser("VAR VARR : LOGICAL;BEGIN VARR = 1;  END  ")
    parser("VAR AS, SDF, SDFF : LOGICAL; BEGIN AS = 1; END  ")
    parser("VAR AS, DE : LOGICAL; BEGIN AS = 1; DE = 0; END  ")
    parser("VAR AS, JSDFSEGF : LOGICAL; BEGIN JSDFSEGF = 1; AS = JSDFSEGF; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = 1; AS = 1; AS = 1; AS = 1; AS = 1; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = .NOT. 1 ; END  ")
    parser("VAR AS, SD : LOGICAL; BEGIN SD=0; AS = 1 .AND. SD ; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = 0 .OR. 1 ; END  ")
    parser("VAR AS, SDF : LOGICAL; BEGIN SDF=0;AS = SDF .IMP. 1 ; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = .NOT. 1 .AND. 0 ; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = (1) ; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = .NOT. (1) ; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = (.NOT. 1) ; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = (1 .AND. 0) ; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = (.NOT. 1 .AND. 0) ; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = (((((0))))) ; END  ")
    parser("VAR AS : LOGICAL; BEGIN AS = (.NOT. (1)) ; END  ")
    parser("VAR AS : LOGICAL;BEGIN AS = (((((((((1) .AND. 1) .AND. 1)))))));  END")
    parser("VAR AS, SDFF : LOGICAL; BEGIN SDFF=0;AS = (.NOT. (1 .OR. SDFF)) ; END  ")
    parser("VAR AS, SDFF : LOGICAL; BEGIN SDFF=0;AS = (.NOT. (1 .OR. (SDFF .OR. 0))) ; END  ")
    parser("VAR AS : LOGICAL;BEGIN AS = 1 .OR. ((1 .OR. 1) .OR. 1);  END")
    parser("VAR AS : LOGICAL;BEGIN AS = 1 .OR. (.NOT. 0) ;  END")
    parser("VAR AS : LOGICAL;BEGIN AS = (1 .OR. ((0 .OR. 1) .OR. (( (.NOT. 0 .OR. 0)) .OR. 1)) ) .AND. 0 ;  END")
    parser(
        "VAR SDFG, AS, AW, SDFF, DS : LOGICAL; BEGIN SDFG=1; AS = (1 .OR. 0); AW=0; SDFG = (AW .OR. ((1 .AND. 1) .IMP. (.NOT. 0 .OR. (SDFG .OR. (0 .AND. 1))))); END ")

    error_tests = [
        "",
        "VAR",
        "VAR  : LOGICAL; BEGIN AS = 1; END  ",
        "VAR: ; BEGIN AS = 1; END  ",
        "VAR , : LOGICAL;BEGIN AS = 0;  END",
        "VAR AS RF : LOGICAL;BEGIN AS = 0;  END",
        "VAR AS, RF DF : LOGICAL;BEGIN AS = 0;  END",
        "VAR, AS, RF : LOGICAL;BEGIN AS = 0;  END",
        "VARDD : LOGICAL; BEGIN AS = 1; END  ",
        "VAR DD, : LOGICAL; BEGIN AS = 1; END  ",
        "VAR DD, DF, : LOGICAL; BEGIN AS = 1; END  ",
        "VAR DD, DF, SD  LOGICAL; BEGIN AS = 1; END  ",
        "VAR DD, DF, SD,  LOGICAL; BEGIN AS = 1; END  ",
        "VAR DD, DF, SD,  LOGICAL : LOGICAL; BEGIN AS = 1; END  ",
        "VAR END : LOGICAL; BEGIN AS = 1; END  ",
        "VAR BEGIN : LOGICAL; BEGIN AS = 1; END  ",
        "VAR SD : LOGICAL1; BEGIN AS = 1; END  ",
        "VAR SD : BOOL; BEGIN AS = 1; END  ",
        "VAR SD : LOGICAL SD; BEGIN AS = 1; END  ",
        "VAR SD : LOGICAL, SD; BEGIN AS = 1; END  ",
        "VAR SD : LOGICAL, BEGIN SD = 1; END  ",
        "VAR SD : LOGICAL, BEGIN; SD = 1; END  ",
        "VAR SD : LOGICAL;  SD = 1; END  ",
        "VAR SD : LOGICAL;BEGIN1  SD = 1; END  ",
        "VAR SD : LOGICAL;BEGINSD = 1; END  ",
        "VAR SD, SD : LOGICAL;BEGIN SD = 1; END  ",
        "VAR SD : LOGICAL;BEGIN AS = 1; END  ",
        "VAR SD : LOGICAL;BEGIN SD = SD; END  ",
        "VAR SD : LOGICAL;BEGIN SD = AS; END  ",
        "VAR SD : LOGICAL;BEGIN SD = 1 .AND. AS ; END  ",
        "VAR SD : LOGICAL;BEGIN SD = 1 .AND. ASF ; END  ",
        "VAR SD : LOGICAL;BEGIN 1 = 0;  END  ",
        "VAR SD : LOGICAL;BEGIN BEGIN = 0;  END  ",
        "VAR SD : LOGICAL;BEGIN SD = END;  END  ",
        "VAR SD : LOGICAL;BEGIN SD = END;  END  ",
        "VAR AS : LOGICAL;BEGIN AS = 1  END  ",
        "VAR AS : LOGICAL;BEGIN AS = ;  END  ",
        "VAR AS : LOGICAL;BEGIN AS ;  END  ",
        "VAR AS : LOGICAL;BEGIN AS  END  ",
        "VAR AS, DE : LOGICAL;BEGIN AS =0; DE=1 END  ",
        "VAR AS, DE : LOGICAL;BEGIN AS =0; DE=1;  ",
        "VAR AS, DE : LOGICAL;BEGIN AS =0; DE=1;  ENDD",
        "VAR AS, DE : LOGICAL;BEGIN AS =0; DE=1;  END;",
        "VAR AS, DE : LOGICAL;BEGIN AS =0; DE=1;  END AS = 0",
        "VAR AS : LOGICAL;BEGIN AS = ();  END",
        "VAR AS : LOGICAL;BEGIN AS = (((())));  END",
        "VAR AS : LOGICAL;BEGIN AS = .NOT. (((())));  END",
        "VAR AS : LOGICAL;BEGIN AS = .NOT.;  END",
        "VAR AS : LOGICAL;BEGIN AS = 0 .NOT.;  END",
        "VAR AS : LOGICAL;BEGIN AS = .NOT. .OR.;  END",
        "VAR AS : LOGICAL;BEGIN AS = .NOT. 0 .NOT. 1;  END",
        "VAR AS : LOGICAL;BEGIN AS = .NOT. (0 .NOT. 1);  END",
        "VAR AS : LOGICAL;BEGIN AS = .NOT. (.NOT. 0;  END",
        "VAR AS : LOGICAL;BEGIN AS = ( .NOT. );  END",
        "VAR AS : LOGICAL;BEGIN AS = ( .AND. );  END",
        "VAR AS : LOGICAL;BEGIN AS = (0 .AND. .NOT. 1);  END",
        "VAR AS : LOGICAL;BEGIN AS = .AND. 1;  END",
        "VAR AS : LOGICAL;BEGIN AS = (1) .AND. ;  END",
        "VAR AS : LOGICAL;BEGIN AS = (1) .AND. 1 .AND. 1;  END",
        "VAR AS : LOGICAL;BEGIN AS = (1) .NOT. 1 .AND. 1;  END",
        "VAR AS : LOGICAL;BEGIN AS = ((((((((((((((((1) .AND. 1) .AND. 1))))))))))))));  END",
        "VAR AS : LOGICAL;BEGIN AS = (1) (.AND.) (1);  END",
        "VAR AS : LOGICAL;BEGIN AS = (1) .AND. (1)(0 .OR. 1);  END",
        "VAR AS : LOGICAL;BEGIN AS = (1) .AND. (1) .OR. (0 .OR. 1);  END",
        "VAR AS : LOGICAL;BEGIN AS = 1 .OR. (1 .OR. 1) .OR. 1;  END",
        "VAR AS : LOGICAL;BEGIN AS = 01010101 ;  END",
        "VAR AS : LOGICAL;BEGIN 00 = 1 ;  END",
        "VAR AS : LOGICAL;BEGIN AS = (1 .OR. (0 .OR. 1) .OR. 0) ;  END"
        "VAR AS : LOGICAL;BEGIN AS = (1 .OR. ((0 .OR. 1) .OR. (0 .OR. 0)) .OR. 1) ;  END",
        "VAR AS : LOGICAL;BEGIN AS = (1 .OR. ((0 .OR. 1) .OR. ( (.NOT. 0 .OR. 0)) .OR. 1) ) ;  END",
        "VAR AS DEEE : LOGICAL;BEGIN AS = (1 .OR. (0 .OR. (0 .OR. 1))) .AND. 0 ;  END",

    ]
    for ind, i in enumerate(error_tests):
        try:
            parser(i)
            print('Тест номер', ind, "провален")
            raise Exception()
        except (AssertionError) as e:
            pass
    print('Все тесты прошли успешно')


# test()

if __name__ == "__main__":
    try:
        parser("VAR AS : LOGICAL;BEGIN AS = (1 .OR. ((0 .OR. 0) .OR. 1)) .AND. 0 ;  END ")
        raise Exception()
    except (AssertionError) as e:
        print(e)
        print('Ok')
