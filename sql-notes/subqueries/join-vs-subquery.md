# Inner JOIN vs WHERE exists sub-query

As is the case in many languages where its usually possible to solve the same problem in more ways than one, the same is true for SQL.

Consider a scenario where you need to JOIN 2 tables where you only need the intersecting column.
This problem can be solved by a classical inner join. However, you can also solve this same problem with a WHERE EXISTS sub-query.

## The jury

To judge which approach is canonical we would need to consider the following attributes per approach.

1 - Simplicity and power
2 - Query plan and execution
3 - Possession of declarative intentions

With `JOIN` query

```sql
SELECT
  category :: text
FROM
  factories
JOIN
  departments
USING
  (category)
```

With `WHERE EXISTS` query

```sql
SELECT
  category :: text
FROM
  factories
WHERE
  EXISTS
  (
    SELECT
    FROM
      departments
    WHERE
      category = factories.category
  )
```

### Simplicity and power

The where clause approach is arguably the least powerful and simpler method of the two.

### Query plan and execution (WIP)

I have observed both queries to produce the same query plan and thus execution time.

### Possession of declarative intentions

Both queries appear to possess the same level of declarative intentions.

## Conclusion

Basing on the above examination a WHERE EXISTS query could be a perfect drop in replacement for a simple inner query on account of the simplicity and least powerful attribute.

I would like to further emphasize that for cases where you need to retrieve multiple columns from 2 intersecting tables, an explicit inner JOIN makes more sense.
