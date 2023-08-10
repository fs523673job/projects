import fdb
import subprocess
import os

def insert_into_firebird(input_name, input_sha, branch_name):
    # Configurações de conexão com o banco Firebird
    DB_HOST = 'localhost'
    DB_NAME = r'C:\github\bases\firebird\TESTDB_F30.FDB'
    DB_USER = 'SYSDBA'
    DB_PASSWORD = 'masterkey'

    # Conexão com o banco de dados
    con = fdb.connect(host=DB_HOST, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
    cur = con.cursor()
    cur.execute("SELECT COUNT(*) FROM FEATURE_CRIADAS")
    count = cur.fetchone()[0]
    print(f"Total de registros na tabela FEATURE_CRIADAS: {count}")

    # SQL para inserir os dados
    sql = """
    INSERT INTO FEATURE_CRIADAS (NAME_FEATURE, NAME_BRANCH, SHA_ORIGEM) 
    VALUES (?, ?, ?)
    """

    # Executa a SQL
    cur.execute(sql, (input_name, input_sha, branch_name))
    con.commit()
    con.close()


def main():
    # Muda para o diretório correto
    os.chdir(r'c:/apdata_x64')

    # Chama o script bash e obtém a saída
    sha_to_find = input("Digite o nome da SHA: ")
    feature_name = input("Digite o nome da feature: ")

    args = ['C:/Program Files/Git/bin/git.exe', 'fscreatefeaturefromsha', sha_to_find, feature_name]

    result = subprocess.run(args, capture_output=True, text=True)
    print("Saída completa do Bash:\n", result.stdout)

    data = {}
    for line in result.stdout.split("\n"):
        if "=" in line:
            key, value = line.split("=")
            data[key.strip()] = value.strip()

    print("Data extraído do Bash:", data)

    if 'NAME_FEATURE' in data and 'NAME_BRANCH' in data and 'SHA_ORIGEM' in data:
        insert_into_firebird(data['NAME_FEATURE'], data['NAME_BRANCH'], data['SHA_ORIGEM'])
    else:
        print("Erro: Não foi possível obter todas as informações necessárias da saída do script Bash.")

if __name__ == '__main__':
    main()
