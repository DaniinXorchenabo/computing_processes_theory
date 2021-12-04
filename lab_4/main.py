import os

from shop_machine import parser

if __name__ == '__main__':
    if os.environ.get("MODE", "console") == "console":
        while True:
            try:
                data = input()
                print(parser(data))
            except AssertionError as e:
                print(e)
    else:
        input_file_name = os.environ.get("INPUT_FILE", "input.txt")
        output_file_name = os.environ.get("OUTPUT_FILE", "output.txt")
        with open(input_file_name, 'r', encoding='utf-8') as f:
            data = " ".join(f.readlines()).replace('\n', " ")
            print(data)
        try:
            answer = parser(data)
        except AssertionError as e:
            answer = str(e)
        with open(output_file_name, 'w', encoding='utf-8') as f:
            print(answer, file=f)
