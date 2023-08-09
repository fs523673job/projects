import fdb
import subprocess

def insert_into_firebird(input_name, input_sha, branch_name):
    # Configurações de conexão com o banco Firebird
    DB_HOST = 'localhost'
    DB_NAME = 'C:\github\\bases\\firebird\\TESTDB_F30.FDB'
    DB_USER = 'SYSDBA'
    DB_PASSWORD = 'masterkey'

    # Conexão com o banco de dados
    con = fdb.connect(host=DB_HOST, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
    cur = con.cursor()

    # SQL para inserir os dados
    sql = """
    INSERT INTO FEATURE_CRIADAS (NAME_FEATURE, NAME_BRANCH, SHA_ORIGEM) 
    VALUES (:NAME_FEATURE, :NAME_BRANCH, :SHA_ORIGEM)
    """

    # Executa a SQL
    cur.execute(sql, (input_name, input_sha, branch_name))
    con.commit()
    con.close()

def main():
    # Chama o script bash e obtém a saída
	result = subprocess.run(['C:/Program Files/Git/bin/bash.exe', '-l', 'C:/github/fs523673job/projects/cmdGIT/git-fscreatefeaturefrombranch'], capture_output=True, text=True)

    # Aqui você pode extrair as informações da saída do script bash
    # Vamos supor que o script bash imprime as informações da seguinte forma:
    # input_name=input_value
    # input_sha=sha_value
    # branch_name=branch_value
    
	output = result.stdout
	data = {}
	for line in output.split("\n"):
		if "=" in line:
			key, value = line.split("=")
			data[key] = value

	# Insere os dados no banco Firebird
	if 'NAME_FEATURE' in data and 'NAME_BRANCH' in data and 'SHA_ORIGEM' in data:
		insert_into_firebird(data['NAME_FEATURE'], data['NAME_BRANCH'], data['SHA_ORIGEM'])
	else:
		print("Erro: Não foi possível obter todas as informações necessárias da saída do script Bash.")

if __name__ == '__main__':
    main()
