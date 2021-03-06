import re
import regex

UNARY_OPERATION = r"\.NOT\."  # \.NOT\.(?![A-Za-z])
CONSTANTS = r"0|1"
BINARY_OPERATIONS = r"\.AND\.|\.OR\.|\.IMP\."

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
    key = r".*"

    def __new__(cls, data: str) -> tuple[list, dict]:
        arr = data.split(';', 1)
        return [arr[0] + ";", arr[1]], {0: True, 1: True}


class InitializingVariables(BaseItem):
    key = r"VAR\s+(.*)\s*:\s*(.*)\s*;"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        return ["VAR", data[1], ":", data[2], ";"], {1: True}


class GetListVars(BaseItem):
    key = r"\s*(?:,\s*)"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(r"(?:\s*,\s*)*([A-Z]+)((?:\s*,\s*[A-Z]+)*)", data))
        # print('===', [data[0], "|", data[1], "|", data[2]])
        return [data[1]] + ([",", data[2]] if bool(data[2]) else []), {0: True, 2: True}


class ReadWord(BaseItem):
    key = r"^\s*([A-Z]+)\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|")
        return [data[1]], dict()


class ProgramBody(BaseItem):
    key = r"BEGIN\s+(.*)\s+END"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[1], "|")
        return ["BEGIN", data[1], "END"], {1: True}


class AssignmentList(BaseItem):
    key = r"((?:[^=]+\s*=\s*[^;]+\s*;)+)"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(r"([^=]+\s*=\s*[^;]+\s*);\s*((?:\s*[^=]+\s*=\s*[^;]+\s*;\s*)*)", data))
        # print(data[0], "|", data[1], "|", data[2])
        return [data[1], ";"] + ([data[2]] if bool(data[2]) else []), {0: True, 2: True}


class OneAssignment(BaseItem):
    key = r"\s*([^=\s]+)\s*=\s*([^;]+)\s*"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|", data[1], "|", data[2])
        return [str(data[1]) + " =", data[2]], {0: True, 1: True}


class AssignmentVariables(BaseItem):
    key = r"\s*([^=\s]+)\s*="

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|", data[1], "|", data[2])
        return [data[1], " ="], {0: True}


class OneUnaryExpression(BaseItem):
    key = fr"^(\s*(?:{UNARY_OPERATION})?)(\s*{BRACKETS_PARSE(2)}\s*?|\s*[A-Za-z]+\s*?|\s*{CONSTANTS}\s*?)$"

    def __new__(cls, data) -> tuple[list, dict]:
        # print(cls.key)
        assert (data := regex.search(cls.key, data))
        # print(data)
        # print(data[0], "|", data[1], "|", data[2], "|", data[3], "|")
        return ([data[1]] if bool(data[1]) else []) + [data[2]], {0: True, 1: True}


class OneBinaryExpression(BaseItem):
    key = fr"^((\s*{BRACKETS_PARSE(3)}\s*?|\s*[A-Za-z]+\s*?|\s*{CONSTANTS}\s*?)(?:\s*({BINARY_OPERATIONS}))(\s*{BRACKETS_PARSE(6)}\s*?|\s*[A-Za-z]+\s*?|\s*{CONSTANTS}\s*?))$"

    def __new__(cls, data) -> tuple[list, dict]:
        # print(cls.key)
        assert (data := regex.search(cls.key, data))
        # print(data)
        # print(data[0], "|", data[1], "|", data[2], "|", data[3], "|",  data[4], "|",  data[5], "|",  data[6])
        return [data[3], data[4], data[6]], {0: True, 1: True, 2: True}


class UnaryOperation(BaseItem):
    key = fr"^\s*({UNARY_OPERATION})\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|", data[1])
        return [data[1]], dict()


class BinaryOperation(BaseItem):
    key = fr"^\s*({BINARY_OPERATIONS})\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := re.search(cls.key, data))
        # print(data[0], "|", data[1])
        return [data[1]], dict()


class SubExpression(BaseItem):
    key = fr"^\s*{BRACKETS_PARSE(0)}\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := regex.search(cls.key, data))
        # print(data[0], "|", data[1])
        return ["(", str(data[1])[1:-1], ")"], {1: True}


class ReadConst(BaseItem):
    key = fr"^\s*({CONSTANTS})\s*$"

    def __new__(cls, data) -> tuple[list, dict]:
        assert (data := regex.search(cls.key, data))
        # print(data[0], "|", data[1])
        return [data[1]], dict()


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
    Program: [InitializingVariables, ProgramBody],
    InitializingVariables: [GetListVars, ReadWord],
    GetListVars: [GetListVars, ReadWord],
    ReadWord: [],
    ProgramBody: [AssignmentList],
    AssignmentList: [OneAssignment, AssignmentList],
    OneAssignment: [AssignmentVariables, OneBinaryExpression, OneUnaryExpression],
    AssignmentVariables: [ReadWord],
    OneUnaryExpression: [UnaryOperation, SubExpression, ReadWord, ReadConst],
    OneBinaryExpression: [SubExpression, BinaryOperation],
    SubExpression: [OneUnaryExpression, ReadWord, ReadConst],
    ReadConst: [],
    UnaryOperation: [],
    BinaryOperation: [],
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


# parser("""VAR SDFG : LOGICAL; BEGIN AS = (1 .OR. 0) ;  END""")

# parser("""VAR AS, AW, SDFF, DS, SDFG : LOGICAL; BEGIN AS = (1 .OR. (AW .IMP. 0) .AND. 1 .AND. 1 .AND. (1 .OR. .NOT. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. (0 .OR. 1))))))))))))))) ; AW = 0; END""")
# print(OneBinaryExpression("((1) .OR. (0))"))
