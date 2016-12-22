#include <stddef.h>
#ifdef MARIADB_CLIENT
#include <mysql/mysql.h>
#else
#include <mariadb/mysql.h>
#endif

int
main(void)
{
    MYSQL *m = mysql_init(NULL);
    mysql_close(m);
    return 0;
}
