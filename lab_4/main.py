import os

# from recursive_parser import parser

if __name__ == '__main__':

    if os.environ.get("ENVIRONMENT_IS_LOADED", 'false') != 'true':

        from dotenv import load_dotenv
        from os.path import dirname, join, split

        os.environ["ENVIRONMENT_IS_LOADED"] = 'true'

        if os.environ.get("PARSER_TYPE") is None:

            path = dirname(__file__)
            dotenv_path = join(path,  os.environ.get("PATH_TO_ENV_FILE", '.env'))
            print(dotenv_path, __file__)
            if os.path.exists(dotenv_path):
                load_dotenv(dotenv_path)
                os.environ.update({key.split("#")[0].replace(" ", ""): val.split("#")[0].replace(" ", "")
                                   for key, val in os.environ.items()})
            else:
                print("В корне проекта обязательно должен быть файл .env!!!!")
                raise FileNotFoundError("\n.env file must be in project root\n")

    if os.environ.get("PARSER_TYPE", "shop") == 'recursion':
        from recursive_parser import parser
    else:
        from shop_machine import parser


    if os.environ.get("MODE", "console") == "console":
        while True:
            try:
                data = input()
                print(parser(data))
            except (AssertionError, ValueError, TypeError) as e:
                print(e)
    else:
        input_file_name = os.environ.get("INPUT_FILE", "input.txt")
        output_file_name = os.environ.get("OUTPUT_FILE", "output.txt")
        with open(input_file_name, 'r', encoding='utf-8') as f:
            data = " ".join(f.readlines()).replace('\n', " ")
            print(data)
        try:
            answer = parser(data)
        except (AssertionError, ValueError, TypeError) as e:
            answer = str(e)
        with open(output_file_name, 'w', encoding='utf-8') as f:
            print(answer, file=f)
