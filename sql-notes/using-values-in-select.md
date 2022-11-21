# Places where using VALUE shines in SELECT statements (beginner material)

```sql
SELECT
  side::text
, type::text
, maximum_age::interval
FROM
  (
    VALUES
      ('side-a', 'type-a',   '3 days')
    , ('side-b', 'type-b',    '100 days')
  ) _ (side, type, maximum_age)
```
