and运算符是有顺序的，先计算前面的，再根据前面的匹配结果计算后面的，所以当计算
(outranked-by (Bitdiddle Ben) ?who)
会首先计算or子句
(supervisor (Bitdiddle Ben) ?who)
扫描数据库找到一个结果
(supervisor (Bitdiddle Ben) (Warbucks Oliver))
接下来计算and子句
(outranked-by ?middle-manager ?who)
继续应用outranked-by规则，计算or子句
(supervisor ?middle-manager-2 ?who)
扫描数据库会找到多条，计算and子句
(outranked-by ?middle-manager-2 ?who)
可以看到这会形成一个死循环，永远不会终止。
原来的规则是先计算
(supervisor ?staff-person ?middle-manager)
具体就是
(supervisor (Bitdiddle Ben) ?middle-manager)
在数据库只有一条，
(supervisor (Bitdiddle Ben) (Warbucks Oliver))
再经过过滤
(outranked-by (Warbucks Oliver) ?who)
为空，程序就终止了
