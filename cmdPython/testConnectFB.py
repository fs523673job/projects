import firebird.driver as fdb
con = fdb.connect(r"localhost:C:\github\bases\firebird\TESTDB_F30.FDB",
                  user="sysdba", password="master")
print("Conectou ->", con)
con.close()