import re
import regex

UNARY_OPERATION = r"\.NOT\."  # \.NOT\.(?![A-Za-z])
CONSTANTS = r"(?:0|1)"
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

    def __new__(cls, data):
        return [data]


class InitializingVariables(BaseItem):
    key = r"VAR\s+(.*)\s*:\s*(.*)\s*;"

    def __new__(cls, data) -> list:
        assert (data := re.search(cls.key, data))
        return [data[i] for i in range(1, 3)]


print(InitializingVariables("dfsgvsfgvsfVAR ASD=4 : LOGICAL ; sfdgvsdfv"))


class GetListVars(BaseItem):
    key = r"([A-Z][^,]|(?:[A-Z],\s*)+)"

    def __new__(cls, data) -> list:
        assert (data := re.search(r"([A-Z]+)((?:\s*,\s*[A-Z]+)*)", data))
        print(data[0], "|", data[1], "|", data[2])
        return [data[i] for i in range(1, 3)]


print(GetListVars("asdfkjh ASD, SDFKJ, DSF, KJDSF asdofjae"))


class ReadWord(BaseItem):
    key = r"^\s*([A-Z]+)\s*$"

    def __new__(cls, data) -> list:
        assert (data := re.search(cls.key, data))
        print(data[0], "|")
        return [data[0]]


print(ReadWord("ASD"))


class ProgramBody(BaseItem):
    key = r"BEGIN\s+(.*)\s+END"

    def __new__(cls, data) -> list:
        assert (data := re.search(cls.key, data))
        print(data[1], "|")
        return [data[1]]


print(ProgramBody("dfsgvsfgvsfVAR ASD=4 : LOGICAL ; sfdgvsdfv BEGIN dkfj jkn? seo, srfks   sdf END"))


class AssignmentList(BaseItem):
    key = r"((?:[^=]+\s*=\s*[^;]+\s*;)+)"

    def __new__(cls, data) -> list:
        assert (data := re.search(r"([^=]+\s*=\s*[^;]+\s*;)\s*((?:\s*[^=]+\s*=\s*[^;]+\s*;\s*)*)", data))
        print(data[0], "|", data[1], "|", data[2])
        return [data[1], data[2]]


print(AssignmentList(" AAD = 45;  SDF=2345; DSF =345; SDF= 235; ASFS = 2354 ; "))


class OneAssignment(BaseItem):
    key = r"\s*([^=\s]+)\s*=\s*([^;]+)\s*;"

    def __new__(cls, data) -> list:
        assert (data := re.search(cls.key, data))
        print(data[0], "|", data[1], "|", data[2])
        return [data[1], data[2]]


print(OneAssignment(" AAD = 4 5 ;  SDF=2345; DSF =345; SDF= 235; ASFS = 2354 ; "))


class OneUnaryExpression(BaseItem):
    # ((?:NOT(?![A-Za-z])|-))(\s*\([^)]+\)\s*?|\s*[A-Za-z]+\s*?|\s*[01]\s*?)
    #
    # ((?:{UNARY_OPERATION})?)(\s*\([^)]+\)\s*|\s+[A-Za-z]+\s*|\s+[01]\s*)
    #
    # ((\(([^()]*?)*?(?0)*?([^()]*?)*?\)([^()]*?)*?)([^()]*)*(?0)*?)
    # ((\(([^()]*?(?=[()]))*?(?0)*?([^()]*?(?=[()]))*?\)([^()]*?(?=[()]))*?)([^()]*(?=[()]))*(?0)*?)

    # (.*?)(\([^()]*(?=[()])(?<=[()])(?1)     \))
    # (\(((?<=[()])[^()]*(?=[()]))*((?0)*((?<=[()])[^()]*(?=[()])))*((?<=[()])[^()]*(?=[()]))*\))

    # ((?:{UNARY_OPERATION})?)(\s*{BRACKETS_PARSE(2)}\s*?|\s*[A-Za-z]+\s*?|\s*[01]\s*?)
    key = fr"(\s*(?:{UNARY_OPERATION})?)(\s*{BRACKETS_PARSE(2)}\s*?|\s*[A-Za-z]+\s*?|\s*{CONSTANTS}\s*?)"

    def __new__(cls, data) -> list:
        print(cls.key)
        assert (data := regex.search(cls.key, data))
        print(data)
        print(data[0], "|", data[1], "|", data[2], "|", data[3], "|")
        return [data[1], data[2]]


print(OneUnaryExpression(" NOT(ASR OR 0 AND (1 OR (HELP AND NOT(1 AND 1))) (())) () "))


class OneBinaryExpression(BaseItem):
    # ((?:NOT(?![A-Za-z])|-))(\s*\([^)]+\)\s*?|\s*[A-Za-z]+\s*?|\s*[01]\s*?)
    #
    # ((?:{UNARY_OPERATION})?)(\s*\([^)]+\)\s*|\s+[A-Za-z]+\s*|\s+[01]\s*)
    #
    # ((\(([^()]*?)*?(?0)*?([^()]*?)*?\)([^()]*?)*?)([^()]*)*(?0)*?)
    # ((\(([^()]*?(?=[()]))*?(?0)*?([^()]*?(?=[()]))*?\)([^()]*?(?=[()]))*?)([^()]*(?=[()]))*(?0)*?)

    # (.*?)(\([^()]*(?=[()])(?<=[()])(?1)     \))
    # (\(((?<=[()])[^()]*(?=[()]))*((?0)*((?<=[()])[^()]*(?=[()])))*((?<=[()])[^()]*(?=[()]))*\))

    # ((?:{UNARY_OPERATION})?)(\s*{BRACKETS_PARSE(2)}\s*?|\s*[A-Za-z]+\s*?|\s*[01]\s*?)
    key = fr"((\s*{BRACKETS_PARSE(3)}\s*?|\s*[A-Za-z]+\s*?|\s*{CONSTANTS}\s*?)(?:\s*({BINARY_OPERATIONS}))(\s*{BRACKETS_PARSE(6)}\s*?|\s*[A-Za-z]+\s*?|\s*{CONSTANTS}\s*?))"

    def __new__(cls, data) -> list:
        # print(cls.key)
        assert (data := regex.search(cls.key, data))
        # print(data)
        # print(data[0], "|", data[1], "|", data[2], "|", data[3], "|",  data[4], "|",  data[5], "|",  data[6])
        return [data[3], data[4], data[6]]


print(OneBinaryExpression(" ((d(d(d(df)f(dfg)f(g))f)d(d)d(d)d(d)f)) .AND. (---(d(d(d( df)f(dfg)f(g))f)d(d)d(d)d(d)f))   "))


class UnaryOperation(BaseItem):
    key = fr"^\s*({UNARY_OPERATION})\s*$"

    def __new__(cls, data) -> list:
        assert (data := re.search(cls.key, data))
        print(data[0], "|", data[1])
        return [data[1]]


print(UnaryOperation(" .NOT. "))


class BinaryOperation(BaseItem):
    key = fr"^\s*({BINARY_OPERATIONS})\s*$"

    def __new__(cls, data) -> list:
        assert (data := re.search(cls.key, data))
        print(data[0], "|", data[1])
        return [data[1]]


print(BinaryOperation(" .AND. "))


class SubExpression(BaseItem):
    key = fr"\s*{BRACKETS_PARSE(0)}\s*"

    def __new__(cls, data) -> list:
        assert (data := regex.search(cls.key, data))
        print(data[0], "|", data[1])
        return [str(data[1])[1:-1]]


print(SubExpression(" (DFG()(())) "))


class ReadConst(BaseItem):
    key = fr"\s*({CONSTANTS})\s*"

    def __new__(cls, data) -> list:
        assert (data := regex.search(cls.key, data))
        print(data[0], "|", data[1])
        return [data[1]]


print(ReadConst(" 1 "))

GRAPH = {
    Program: [InitializingVariables, ProgramBody],
    InitializingVariables: [GetListVars],
    GetListVars: [GetListVars, ReadWord],
    ReadWord: [],
    ProgramBody: [AssignmentList],
    AssignmentList: [OneAssignment, AssignmentList],
    OneAssignment: [ReadWord, OneUnaryExpression, OneBinaryExpression],
    OneUnaryExpression: [UnaryOperation, SubExpression],
    OneBinaryExpression: [SubExpression, BinaryOperation],
    UnaryOperation: [],
    BinaryOperation: [],
    SubExpression: [OneUnaryExpression, ReadWord, ReadConst],
    ReadConst: [],
}
