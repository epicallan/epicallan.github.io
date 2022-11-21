# Abstract

Ordering by a created at date is a common pitfall:

> Sometimes I’ll even order by a non-key, but then use a key as a tie-breaker, eg: ORDER BY created_at, id  — so in this case, I want to order from oldest to newest, but when two or more rows have the same value for created_at, I want to then order them by the id so that I am guaranteed to always produce deterministic results. We call that a “tie-breaker”; it’s our own term.
