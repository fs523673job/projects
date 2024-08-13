import fdb

def atualizar_link_firebird(chamado_numero: str, link: str):
    # Configurações de conexão com o banco Firebird
    DB_HOST = 'localhost'
    DB_NAME = r'C:\github\bases\firebird\TESTDB_F30.FDB'
    DB_USER = 'SYSDBA'
    DB_PASSWORD = 'master'

    # Conexão com o banco de dados
    con = fdb.connect(host=DB_HOST, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
    cur = con.cursor()

    # Construir o nome da feature
    nome_feature = f"PT_{chamado_numero}_MR"

    # Verificar se a feature existe
    cur.execute("SELECT COUNT(*) FROM FEATURE_CRIADAS WHERE NAME_FEATURE = ?", (nome_feature,))
    count = cur.fetchone()[0]

    if count == 0:
        print(f"Não foi encontrado o chamado com o nome {nome_feature}.")
        return

    # SQL para atualizar o link
    sql = """
    UPDATE FEATURE_CRIADAS 
    SET LINK_MR = ? 
    WHERE NAME_FEATURE = ?
    """

    # Executa a SQL
    cur.execute(sql, (link, nome_feature))
    con.commit()
    con.close()

    print(f"Link atualizado para o chamado {chamado_numero}.")

def main():
    # Solicitar informações do usuário
    chamado_numero = input("Digite o número do chamado: ")
    link = input("Digite o link: ")

    # Atualizar o link no banco de dados
    atualizar_link_firebird(chamado_numero, link)

if __name__ == '__main__':
    main()
