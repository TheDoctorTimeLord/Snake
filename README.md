Игра "Змейка". Игра реализована полностью на ассемблере (TASM) для операционной системы MS-DOS.

Для запуска требуется компиляция программы компилятором TASM и последующая линковака файла.
При запуске игры, перед началом сеанса отрисовывается меню управления и информации.
При запуске игры можно указать следующие аргументы:

 -h -H          вывод спривки
 -i -I <0/1>    выбрать тип действий при самопересечении. 
                0 - пересечение приводит к откусыванию хвоста
                1 - пересечение приводит к концу игры
 -a -A <0-255>  добавить на поле указанное число яброк (увеличивает размер змейки на 1)
 -l -L <0-255>  добавить на поле указанное число яда (уменьшает размер змейки на 1)
 -d -D <0-255>  добавить на поле указанное число черепов (мгновенная смерть)
