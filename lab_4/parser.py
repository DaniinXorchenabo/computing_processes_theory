import re
import regex

UNARY = r"(?:\.NOT\.)"  # \.NOT\.(?![A-Za-z])
CONSTANTS = r"(?:0|1)"
BINARY = r"(?:\.AND\.|\.OR\.|\.IMP\.)"
VARIABLES = r"(?:[A-Za-z]+)"

# https://dev-gang.ru/article/rekursivnye-reguljarnye-vyrazhenija-v-python-wz6w1co7jp/
BRACKETS_PARSE = lambda \
        i: fr"(\((?:(?<=[()])[^()]*(?=[()]))*(?:(?{i})*(?:(?<=[()])[^()]*(?=[()])))*(?:(?<=[()])[^()]*(?=[()]))*\))"


class BaseItem(object):
    key = r".*"

    def __init__(self, data):
        self.data = data

    def __new__(cls, *args, **kwargs):
        # super().__new__(*args, **kwargs)
        return args, kwargs


class Program(BaseItem):
    key = r"^\s*([^;]+\s*;)\s*(.+)$"

    def __new__(cls, data: str) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        return [data[1], data[2]], {0: True, 1: True}


class Init(BaseItem):
    key = r"^\s*VAR\s+([^:]+):\s*([^;]+)\s*;\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        # print(cls.key)
        assert (data := re.search(cls.key, data))
        return ["VAR", data[1], ":", data[2], ";"], {1: True}


class ReadManyWords(BaseItem):
    key = fr"^\s*({VARIABLES})\s*,(\s*{VARIABLES}\s*(?:\s*,\s*{VARIABLES}\s*)*)\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print('===', [data[0], "|", data[1], "|", data[2]])
        return [data[1]] + ([",", data[2]] if bool(data[2]) else []), {0: True, 2: True}


class ReadWord(BaseItem):
    key = fr"^\s*({VARIABLES})\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|")
        return [data[1]], dict()


class ReadConst(BaseItem):
    key = fr"^\s*({CONSTANTS})\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := regex.search(cls.key, data))
        # print(data[0], "|", data[1])
        return [data[1]], dict()


class Body(BaseItem):
    key = r"^\s*BEGIN\s+(.*)\s+END\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[1], "|")
        return ["BEGIN", data[1], "END"], {1: True}


class StringList(BaseItem):
    key = r'^\s*([^;]+);\s*((?:\s*[^;]+;\s*)*)\s*$'

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|", data[1], "|", data[2])
        return [data[1], ";"] + ([data[2]] if bool(data[2]) else []), {0: True, 2: True}


class ParseString(BaseItem):
    key = fr"^\s*({VARIABLES}\s*=)\s*([^;]+)\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|", data[1], "|", data[2])
        return [data[1], data[2]], {0: True, 1: True}


class AssignmentVariables(BaseItem):
    key = fr"^\s*({VARIABLES})\s*=\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|", data[1], "|", data[2])
        return [data[1], " ="], {0: True}


class Expression(BaseItem):
    key = fr'^\s*[^=]*$'

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|", data[1], "|", data[2])
        return [data[0]], {0: True}


class BracketGroup(BaseItem):
    key = fr'^\s*\(\s*(.*)\s*\)\s*$'

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|", data[1], "|", data[2])
        return [data[1]], {0: True}


class BinaryOperations(BaseItem):
    key = fr'^\s*({BRACKETS_PARSE(2)}|{CONSTANTS}|{VARIABLES})\s*({BINARY})\s*({BRACKETS_PARSE(5)}|{CONSTANTS}|{VARIABLES})\s*$'

    def __new__(cls, data) -> tuple[list, dict]:
        # print(cls.key)
        assert (data := regex.search(cls.key, data))
        # print(data[0], "|", data[1], "|", data[2])
        return [data[1], data[3], data[4]], {0: True, 2: True}


class UnaryOperations(BaseItem):
    key = fr'^\s*({UNARY})\s*((?:(?:{BRACKETS_PARSE(3)}|{CONSTANTS}|{VARIABLES})\s*(?:{BINARY})\s*(?:{BRACKETS_PARSE(4)}|{CONSTANTS}|{VARIABLES}))|{BRACKETS_PARSE(5)}|{CONSTANTS}|{VARIABLES})\s*$'

    def __new__(cls, data) -> tuple[list, dict]:
        # print(cls.key)
        assert (data := regex.search(cls.key, data))
        # print(data[0], "|", data[1], "|", data[2])
        return [data[1], data[2]], {0: True, 1: True}


# print(UnaryOperations(
#     " .NOT. ((d(d(d(df)f(dfg)f(g))f)d(d)d(d)d(d)f)) .AND. (---(d(d(d( df)f(dfg)f(g))f)d(d)d(d)d(d)f))  "))

# print(Init("VAR SDFG : LOGICAL;"))

# print(Program("dfsgvsfgvsf VAR ASD=4 : LOGICAL ; sfdgvsdfv ; ; ; "))
# print(InitializingVariables("dfsgvsfgvsf VAR ASD=4 : LOGICAL ; sfdgvsdfv"))
# print('----------', GetListVars("asdfkjh ASD, SDFKJ, DSF, KJDSF asdofjae"))
# print(ReadWord(" ASD "))
# print(ProgramBody("dfsgvsfgvsfVAR ASD=4 : LOGICAL ; sfdgvsdfv BEGIN dkfj jkn? seo, srfks   sdf END"))
# print(AssignmentList(" AAD = 45;  SDF=2345; DSF =345; SDF= 235; ASFS = 2354 ; "))
# print(OneAssignment(" AAD = 4 5 ;  SDF=2345; DSF =345; SDF= 235; ASFS = 2354 ; "))
# print(OneUnaryExpression(" .NOT.(ASR OR 0 AND (1 OR (HELP AND NOT(1 AND 1))) (())) () "))
# print(OneBinaryExpression(" ((d(d(d(df)f(dfg)f(g))f)d(d)d(d)d(d)f)) .AND. (---(d(d(d( df)f(dfg)f(g))f)d(d)d(d)d(d)f))"))
# print(UnaryOperation(" .NOT. "))
# print(BinaryOperation(" .AND. "))
# print(SubExpression(" (DFG()(())) "))
# print(ReadConst(" 1 "))

GRAPH = {
    Program: [Init, Body],
    Init: [ReadWord, ReadManyWords],
    ReadManyWords: [ReadWord, ReadManyWords],
    ReadWord: [],
    # --
    Body: [StringList],
    StringList: [ParseString, StringList],
    ParseString: [AssignmentVariables, Expression],
    AssignmentVariables: [ReadWord],
    Expression: [ReadWord, ReadConst, BracketGroup, BinaryOperations, UnaryOperations],
    BracketGroup: [Expression],
    BinaryOperations: [Expression],
    UnaryOperations: [Expression],
    ReadConst: [],

}

START = Program


def parser(data):
    result = [None]

    tasks = [(START, data, result, 0)]
    next_tasks = []
    counter = 0
    while bool(tasks):
        next_tasks = []
        for [current_pr, current_data, result_arr, index] in tasks:

            local_res, _is_finish = current_pr(current_data)
            result_arr[index] = [None] * len(local_res)
            print(result, "\t|\t", local_res, ">>", current_pr)
            for ind, local_data in enumerate(local_res):
                # print(local_data, _is_finish.get(ind), local_res, current_pr)
                find = False
                for pr in GRAPH[current_pr]:
                    # print('\t\t', local_data, regex.search(pr.key, local_data), pr)
                    if _is_finish.get(ind) and regex.search(pr.key, local_data):
                        next_tasks.append((pr, local_data, result_arr[index], ind))
                        # print('\t\t', local_data, pr)
                        find = True
                        continue
                if find is False and bool(local_data):
                    result_arr[index][ind] = local_data

        counter += 1
        tasks = next_tasks[:]
        # if counter > 20:
        #     break
        print(result)

    def recursion_chain(list_: list):
        return [j for i in list_ for j in (recursion_chain(i) if isinstance(i, list) else (i and [i]) or [])]

    print(counter)
    print(result)
    print(tasks)
    print(next_tasks)
    print(' '.join(recursion_chain(result + [None])))

parser("""VAR SDFG, AS, AW, SDFF, DS, SDFG : LOGICAL; BEGIN AS = (1 .OR. 0) ; SDFG = (AW .OR. ((1 .AND. 1) .IMP. (.NOT. 0 .OR. (SDFG .OR. (.NOT. 0 .AND. 1))))); END""")

# parser("""VAR AS, AW, SDFF, DS, SDFG : LOGICAL; BEGIN AS = (1 .OR. (AW .IMP. 0) .AND. 1 .AND. 1 .AND. (1 .OR. .NOT. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. 1))))))))))))))) ; AW = 0; END""")
# print(OneBinaryExpression("((1) .OR. (0))"))
