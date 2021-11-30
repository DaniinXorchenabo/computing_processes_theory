import re


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


def get_variables_list(data):
    return [data]



def program_body_read(data):
    return data


def read_word(data):
    return data


def read_one_char(data):
    return data


GRAPH = {
    Program: [InitializingVariables],
    InitializingVariables: [GetListVars],
    GetListVars: [GetListVars, ReadWord],
    ReadWord: [],
}
