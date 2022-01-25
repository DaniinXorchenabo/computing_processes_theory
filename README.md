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
* Пространство имён переменных не включает ключевые слова (`LOGICAL`, `VAR`, `BEGIN`, `END`). Т.е. имена переменных не могут быть ключевыми словами
* Нельзя использовать в выражениях объявленные, но не инициализированные переменные (те, которым ещё не было присвоено значение)

### Run

Для запуска программы необходимо установить зависимости

```
pip install -r lab_4/requirements.txt
```

папка `lab_4` содержит два парсера языка: рекурсивный и магазинный

1. Запустить рекурсивный парсер:

```
cd lab_4
python recursive_parser.py
```

2. Запустить магазинный парсер

```
cd lab_4
python main.py
```

чтобы запустить рекурсивный парсер необходимо в [`.env`](lab_4/.env) файле 

# Documents

Отчёт (лабораторная № 2) [отчёт.docx](https://github.com/DaniinXorchenabo/computing_processes_theory/files/7924979/default.docx)

Отчёт (Лабораторная № 3) [отчёт.docx](https://github.com/DaniinXorchenabo/computing_processes_theory/files/7924999/_3.docx)

Отчёт (Лабораторная № 4) [отчёт.docx](https://github.com/DaniinXorchenabo/computing_processes_theory/files/7925027/_4.docx)


