# Лабораторная работа № 4

Выполнили:

Аганин Егор Владимирович, Дмитриев Андрей Иванович

## Описание

В ходе этой работы мы будем реализовывать LLVM-based компилятор с уклоном в SIMD программирование.

## Требования

- Трансляция исходоного кода

## Реализация

- Определяем абстрактное синтаксическое дерево [AST.hs](https://github.com/noPlanBonlyA/func_prog_project/blob/main/src/AST.hs)

- Генерация LLVM IR [Codegen.hs](https://github.com/noPlanBonlyA/func_prog_project/blob/main/src/Codegen.hs)

- Определяем лексер, который преобразует текст программы в последовательность токенов [Lexer.hs](https://github.com/noPlanBonlyA/func_prog_project/blob/main/src/Lexer.hs)

- Осуществляем парсинг токенов, полученных от лексера, для построения AST [Parser.hs](https://github.com/noPlanBonlyA/func_prog_project/blob/main/src/Parser.hs)

- Main файл [Main.hs](https://github.com/noPlanBonlyA/func_prog_project/blob/main/src/Main.hs)

## Ввод/Вывод

- Ввод 

```
func main() {
    x: i32 = 10
    y: i32 = 20
    z: i32 = x + y
    return z
}
```

- Вывод AST

```
=== Generated AST ===
[Function "main" [] [DefVar "x" (Primitive I32) (Number 10),DefVar "y" (Primitive I32) (Number 20),DefVar "z" (Primitive I32) (BinOp Plus (Variable "x") (Variable "y")),Return (Variable "z")]]
====================
```

- LLVM IR Вывод в байт коде

```
0$JY�f���� Q�L    !   +     !          �#�A� I  29� � %     �b� E B� B$ 2 8  K
2 �Hp�!#D �� A� d� �  CF� � 2 �X �� D����  � �   ��%  :t�  �       "f  �B�I RB�I�q�PH
 &A� B & ��   ! i0�@-�� ��0   �  �   3 � �� f  =�C8�ÌB� yx s�q �  �  � 3 B �� Ρ f0 =�C8�� � =�C=� =�x�tp {  yH�pp zp vx�p � �  � �0 n0 �� �P 3 � �! �! �a f0�;��;�C9� <��<� ;�� v` {h 7h�rh 7��p��p` v( v� vx�w��_ �q �r��y��,�� �� �� �0 bȡ �  � �a �! ā �a ֐C9�C9�C9�C9��8�C8� ;��/��<��;� ;�� �i�pX�rp�th x`�t �t�� �S �  �P � �@ �  �P 3 ( �� �A �! ܁ �� �� �  f Q8�C:��;�P$v` {h 7`�wx x�QL�� �P 3 j �a �! �� ~  � �! �a T��8��;�C=�C9��<�C;��;�Ì�
�y��w �t  z( r��\�  �� �P �0#��A �� �� �  fH ;��=�� ��8�C9��<��9��;� <�H�q  v` q �qX� �� �` �� �  �0 �  �P n  �0 �0 �� �� �P �0#��a  �� �! �! �! �! �! f �;�C=� 9��9�X�pp wx z  zH�wp� �� �0 �� �@ � �0�  s� w �_��pp�t��tЇr���A9��8�C=�C9�@Ġ ʡ �A �� f$c0 �� �0 �@ �0C!�u  sH�_��|��r��� <��<��8�C:��;�Ì� H! Ba �! �� R� fLg0 �  �� �P �0 �@ �� �  �� �0�@�vh y   �   -    
r(�w� zXp�C=��8�C9�Â� ơ
�A �� �! �! ��  4�` �P �  �@ �  �P ���� y(�p` vx�q  z( rXp��8� ;��=�� k �! �� �  �a �  � �a С �a  �a�  �  �P �  �u  sH� �8��;�C9��9��;�C9� =� ;    �        �<��;� ;� =��<�C8��    q       2  "        ]         � 19.1.7module
```

## Вывод

В ходе этой работы мы реализовали LLVM-based компилятор с уклоном в SIMD программирование. Мы поработали с Parsec, изучили, как можно относительно легко и быстро писать парсеры с помощью языка Haskell, используя парсер-комбинаторы. Также мы поработали с llvm-hs, который предоставляет Haskell обертки для LLVM C API. Качество поддержки llvm-hs оставляет желать лучшего, так как master с llvm-12 просто не собирается, поэтому пришлось использовать llvm-9 (в марте релизится llvm-16).


