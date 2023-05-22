(in-ns 'guestbook.db.core)
(conman/bind-connection *db* "sql/queries.sql")
(get-messages)
