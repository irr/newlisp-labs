(module "mysql.lsp")

(MySQL:init)
(MySQL:connect "127.0.0.1" "root" "mysql" "web")

(MySQL:query "SET @@autocommit=0")

(MySQL:query "SELECT @@autocommit")
(println (MySQL:fetch-all))

(MySQL:query "START TRANSACTION")
(MySQL:query "SELECT @I:=users.id, @N:=users.name FROM users ORDER BY users.id DESC LIMIT 1 FOR UPDATE")
(MySQL:query "INSERT INTO users VALUES (@I + 1, 'irr')")
(MySQL:query "COMMIT")

(MySQL:query "SELECT * from users")
(println (MySQL:fetch-all))

(MySQL:query "START TRANSACTION")
(MySQL:query "DELETE FROM users WHERE name='irr'")
(MySQL:query "COMMIT")

(MySQL:query "SELECT * from users")
(println (MySQL:fetch-all))

(MySQL:close-db)

(exit 0)
