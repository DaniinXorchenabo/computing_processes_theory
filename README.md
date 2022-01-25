# computing_processes_theory

лабораторные работы по предмету "Теория Вычислительных Процессов"

# Description

## lab 4

Магазинный автомат, реализующий следующую грамматику

```
VAR AS : LOGICAL;
BEGIN
  AS = 1; 
END  
```

```
VAR AS : LOGICAL;
BEGIN 
  AS = (1 .OR. ((0 .OR. 1) .OR. (( (.NOT. 0 .OR. 0)) .OR. 1)) ) .AND. 0 ;
END
```

```
VAR SDFG, AS, AW, SDFF, DS : LOGICAL;
BEGIN
  SDFG=1; 
  AS = (1 .OR. 0); 
  AW=0; 
  SDFG = (AW .OR. ((1 .AND. 1) .IMP. (.NOT. 0 .OR. (SDFG .OR. (0 .AND. 1))))); 
END 
```

При этом

* Нельзя использовать переменные, не объяленные в `VAR`
* Переменные нельзя объявлять более одного раза (дважды, трижды и т.д.)
* Пространство имён переменных не включает ключевые слова (`LOGICAL`, `VAR`, `BEGIN`, `END`). Т.е. имена переменных не могут быть ключевыми словами
* Нельзя использовать в выражениях объявленные, но не инициализированные переменные (те, которым ещё не было присвоено значение)

### Run

#### Install requirements

Для запуска программы необходимо установить зависимости

```
pip install -r lab_4/requirements.txt
```

папка `lab_4` содержит два парсера языка: рекурсивный и магазинный

#### Choice parser type

1. Запустить рекурсивный парсер:

  Чтобы запустить рекурсивный парсер необходимо в [`.env`](lab_4/.env) файле установить переменную среды `PARSER_TYPE=recursion` 

2. Запустить магазинный парсер

  Чтобы запустить магазинный парсер необходимо в [`.env`](lab_4/.env) файле установить переменную среды `PARSER_TYPE=shop` 

#### Choice output type

Существует два формата ввода/вывода:
1. В консоль
2. В файл

Для вывода в консоль необходимо в файле [`.env`](lab_4/.env) установить `MODE=file`

Для вывода в консоль необходимо в файле [`.env`](lab_4/.env) установить `MODE=console`

Имена файлов ввода и вывода устанавливаются так же через переменные среды `INPUT_FILE` и `OUTPUT_FILE` соответственно

#### Run program

После выбора парсера необходимо запустить программу ([`main.py`](lab_4/main.py)):

```
cd lab_4
python main.py
```



### Demonstration

#### 1. Рекурсивный парсер

  ![image](https://user-images.githubusercontent.com/45897837/150975102-dba32dd2-d6d1-401c-96f7-db45c1aa7e7d.png)
  
  *Работа рекурсивного парсера 1*
  
  ![image](https://user-images.githubusercontent.com/45897837/150978503-cf059186-526d-4bd2-a75d-64207ac9af30.png)
  
  *Работа рекурсивного парсера 2*
  
  ![image](https://user-images.githubusercontent.com/45897837/150978573-1cd3133d-f643-41c4-96e6-85ee6d158811.png)
  
  *Вывод рекурсивного парсера в файл*

#### 2. Магазинный парсер

  ![image](https://user-images.githubusercontent.com/45897837/150979083-87fed167-72aa-4096-b48a-0254d1df9fdb.png)
  
  *Работа магазинного парсера 1 (обработка двойного объявления переменной)*

  ![image](https://user-images.githubusercontent.com/45897837/150979167-41521d4f-adc8-4ef2-917f-0df8649b90c1.png)

  *Работа магазинного парсера 2 (использование неинициализированной переменной)*

  ![image](https://user-images.githubusercontent.com/45897837/150979557-bd341987-3fc7-40a1-a3d6-8536521b693c.png)
  
  *Работа магазинного парсера 3 (ошибка в расстановке скобок)*

  ![image](https://user-images.githubusercontent.com/45897837/150979614-cc50be6a-503d-4b10-8015-cd937a7daded.png)
  
  *Работа магазинного парсера 4 (с корректной цепочкой)*
  
  ![image](https://user-images.githubusercontent.com/45897837/150979843-680e68ba-1c8e-446a-ac2e-60b5f59ab61a.png)
  
  *Работа магазинного парсера 5 (полный вывод этапов парсинга строки и магазинного стека памяти)*

## lab 3

## lab 2

## lab 1 (not interesting)

Не заслуживает никакого внимания


# Documents

Отчёт (лабораторная № 2) [отчёт.docx](https://github.com/DaniinXorchenabo/computing_processes_theory/files/7924979/default.docx)

Отчёт (Лабораторная № 3) [отчёт.docx](https://github.com/DaniinXorchenabo/computing_processes_theory/files/7924999/_3.docx)

Отчёт (Лабораторная № 4) [отчёт.docx](https://github.com/DaniinXorchenabo/computing_processes_theory/files/7925027/_4.docx)


