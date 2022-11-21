# Inner JOIN vs WHERE EXISTS sub-query

As is the case in many languages where it's usually possible to solve the same problem in more ways than one, the same is true for SQL.

Consider a scenario where you need to assert a column exists in two intersecting tables and retrieve said column.
This problem can be solved by a classic inner join, however, a WHERE EXISTS sub-query can also suffice.

## The jury

To judge which approach is canonical we would need to consider the following attributes per approach.

1. Simplicity and power
2. Query plan and execution

With the `JOIN` query

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

With the `WHERE EXISTS` query

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

The WHERE EXISTS approach is arguably the least powerful and simpler method of the two based on the following.

- A JOIN is a more complex primitive than a WHERE clause.
- The WHERE EXISTS approach doesn't bring into scope columns from the adjoining table in the query's main SELECT. This enables us to keep
  the relation in terms of columns available as narrow as possible.

### Query plan and execution (WIP)

I have observed both queries to produce the same query plan and thus execution time.

### Conclusion

Based on the above examination a WHERE EXISTS query could be a perfect drop-in replacement for a JOIN query on account of the simplicity and least powerful attribute as explained above.

I would like to further emphasize that for cases where you need to retrieve non-intersecting columns from the adjoining tables, an explicit inner JOIN makes more sense on top of being lighter syntax-wise.

The rule of thumb is; If you don't need something explicitly from the table, don't join it. We want to keep the relation (in terms of columns available) as narrow as possible.
