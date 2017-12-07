# 1. Горячие клавишы:
1. `alt + ctrl + ~` - создать package
2. `alt + ctrl + 1` - создать scala sheet файл
3. `alt + ctrl + 2` - создать scala class
4. `alt + ctrl + 3` - создать файл
5. `alt + ctrl + 4` - закрыть текущую вкладку
6. `alt + ctrl + 5` - закрыть все вкладки, кроме этой
7. `alt + ctrl + down` - создать мультикурсор вниз
8. `alt + ctrl + up` - создать мультикурсор вверх
9. `alt + ctrl + shift + p` - git pull
10. `alt + ctrl + shift + F10` - debug класс, на котором курсор

# 2. Шаблоны быстрой подстановки:
### 1. Create *Case Class*:
`case class $NAME$($ARGS$)$END$`
### 2. Create *File Reader*:
`val $NAME$ = scala.io.Source.fromFile("$PATH$")$END$`
### 3. Create *Print Writer*:
`val $NAME$ = new java.io.PrintWriter(new java.io.File("$FILE_NAME$"))$END$`


# 3. Прогон тестов:
### 1. Через IDEA:
  - Создаём Scala Test таску
  - Выбираем корневой package

### 2. Через SBT:
  - Создаём SBT Task
  - В строке команды пишем: test
