### Упражнения


#### 1.1. Простые выражения. Введите в оболочку Erlang следующие выражения и объясните результат.
 - `B = 1.` Binding variable to value.
 - `1 = B.` Pattern matching.
 - `1 = C.` error: pattern matching on unbound variable C.
 - `C = 1.` Binding variable to value.
 - `C = B.` Pattern matching on bound variables C and B.
 - `A = F = D = B.` Since variables A, F, D are unbound ones, they are binded to the value of the variable B.

#### 1.2. Сопоставление с образцом.

 - Объясните, почему выражение `Path = "/bar/foo", Bar ++ "/foo" = Path` не может быть вычислено интерпретатором и порождает ошибку?
   This is an “illegal pattern” in Erlang because there is no way to match out a unbound prefix out of a list like this. 
 - А сработает ли следующее выражение: `"/bar/" ++ Foo = Path.`? Что является его результатом?
   Yes, it will work. This matching expression can be written either as "/bar/Foo = /bar/foo" or as [$/, $b, $a, $r, $/ | Foo] = [$/, $b, $a, $r, $/, $f, $o, $o]. The tail of a list is always part of the list structure.
 - Каков результат следующего выражения?
   It won't work because the variable Name in the first matching clause (name := Name) is assigned to the value "Mike" and when it's time to pattern match second time, surname value ("Williams") matched against binded variable Name which leads to exception being thrown.  

```erlang
Person = #{name => "Mike", surname => "Williams", phone  => [1,2,3,4]}.
#{
    name := Name,
    surname := Name,
    phone := Phone
} = Person.
```

#### 1.3. Сравнение

 - `atom1 > atom2.` (False). Atoms are compared using their string value, codepoint by codepoint.
 - `atom10 < atom2.` (True). If the characters are the same then the longer atom is larger (cat < cats => true). But in this case characters are different.
 - `#{a => 0} < #{a => 1}.` (True). Two maps with the same size are compared by keys in ascending term order and then by values in key order.
 - `#{a => 1} < #{a => 0, b => 0}.` (True). Maps are ordered by size.
 - `{a, 1, 0} > {a, 0, 0}.` (True). Tuples are ordered by size, two tuples with the same size are compared element by element.

#### 1.4. Atoms, erlang shell

 - Какие атомы нельзя использовать? 
 These are: after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse query receive rem try when xor.
 - В чем отличие строго равенства от нестрогого? 
 =:= is the sctrict equal operator (Exactly equal to), it returns true only for same terms in the same way as pattern matching. While is the less strict operator (Equal to), it will transfer both elements into the same format to match.
 - Как заставить интерпретатор "забыть" значение какой-либо переменной? А как
   заставить "забыть" все связывания?
 f(Variable). to erase Variable binding, f() to erase all variables bindings.

#### 1.5. Lists/binary comprehensions

 - Напишите LC, который выдаст декартово умножение двух списков.
Answer:
```erlang
List1 = [1,2,3].
List2 = [5,6,7,8].
[{X,Y} || X <- List1, Y <- List2].
```

 - Имеется список вида - [[1,2,3], [4,5,6], [7,8,9]]. С помощью какого LC мы можем получить плоский список?
Answer:
```erlang
NestedList = [[1,2,3], [4,5,6], [7,8,9]].
[Y || X <- NestedList, Y <- X].
```

 - Имеется список словарей вида:
```erlang
Dicts = [
    #{
        tags => [awesome, erlang]
    },
    #{
        tags => [simple_tag]
    },
    #{
        tags => [just_atom, 'I am ok']
    }
].
```
Какой list comprehension выдаст список вида: `[awesome, erlang, simple_tag,
just_atom, 'I am ok']` ?.
Answer:
```erlang
Extract_values_on_tag = fun(Map, Tag) when is_map(Map), is_atom(Tag) -> #{Tag := V} = Map, V; (_,_) -> error end.
[Flatten || V <- [Extract_values_on_tag(X, tags) || X <- Dicts], Flatten <- V]. 
```
or 
```erlang
[Flatten || V <- [maps:get(tags, V) || V <- Dicts], Flatten <- V]. 
```

 - Попробуйте поменять порядок генераторов в полученных LC. Объясните результат.
Changing order of generators either break logic of a program or make it uncompilable.

 - Имеется следующий список:
```erlang
MixedList = [
    john,
    doe,
    {age, 19},
    {height, 182},
    {weight, 72},
    london,
    britain
].
```
С помощью какого LC можно получить список вида? (см. ниже):
```erlang
 [
    {age, 19},
    {height, 182},
    {weight, 72}
 ]
```
Answer: 
```erlang
[V || V <- MixedList, is_tuple(V)].
```

 - Какой LC выдаст список, в котором только атомы `[john, doe, london, britain]`?
Answer: 
```erlang
[V || V <- MixedList, not is_tuple(V)].
```

 - Имеется список объектов, представляющих прямоугольники на плоскости, следующего вида:
```erlang
Shapes = [
    {{0, 0}, {10, 10}},
    {{0, 1}, {2, 30}},
    {{30, 31}, {40, 41}},
    {{32, 56}, {5, 9}}
].
```
Написать LC, результатом которого будет список прямоугольников с площадью меньше
N.
Answer: 
```erlang
RectanlgeArea = fun({{L1,L2},{R1,R2}}) -> abs((L1 - R1) * (L2- R2)) end. 
fun(N) -> [V || V <- Shapes, RectanlgeArea(V) < N] end.
```
or
```erlang 
fun(N) -> [V || V = {{L1,L2},{R1,R2}} <- Shapes, abs((L1 - R1) * (L2- R2)) < N] end.
```

 - Написать binary comprehension, который сериализует список прямоугольников в бинарное представление.
Answer: 
```erlang 
<< <<L1:8, L2:8, R1:8, R2:8>> || {{L1,L2},{R1,R2}} <- Shapes >>.
```

- Напишите list comprehension, распакует бинарную строку в список, эквивалентный списку Shapes.
Answer: 
```erlang 
[ {{L1, L2},{R1,R2}} || <<L1:8, L2:8, R1:8, R2:8>> <= BinaryList ].
```

#### 1.6. Булевые операции
- [bool.erl](https://github.com/toxish666/erlang_repo/erlang_course_tasks/1-basic/bool.erl)

#### 1.7.  Работа со списками.
- [mlist.erl](https://github.com/toxish666/erlang_repo/erlang_course_tasks/1-basic/mlist.erl)

#### 1.8.  Работа с JSON объектами.
- [mjson.erl](https://github.com/toxish666/erlang_repo/erlang_course_tasks/1-basic/mjson.erl)
