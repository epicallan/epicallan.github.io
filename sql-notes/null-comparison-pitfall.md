# Null comparison pitfall

Ordinary comparison operators yield null (signifying “unknown”), not true or false, when either input is null. For example, 7 = NULL yields null, as does 7 <> NULL. When this behavior is not suitable, use the IS [ NOT ] DISTINCT FROM predicates:

a IS DISTINCT FROM b
a IS NOT DISTINCT FROM b
