import fdb
import webbrowser

def buscar_e_abrir_link(chamado_numero: str):
    # Configurações de conexão com o banco Firebird
    DB_HOST = 'localhost'
    DB_NAME = r'C:\github\bases\firebird\TESTDB_F30.FDB'
    DB_USER = 'SYSDBA'
    DB_PASSWORD = 'masterkey'

    # Conexão com o banco de dados
    con = fdb.connect(host=DB_HOST, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
    cur = con.cursor()

    # Construir o nome da feature
    nome_feature = f"PT_{chamado_numero}_MR"

    # Buscar o link associado ao chamado
    cur.execute("SELECT LINK_MR FROM FEATURE_CRIADAS WHERE NAME_FEATURE = ?", (nome_feature,))
    result = cur.fetchone()

    if result is None:
        print(f"Não foi encontrado o chamado com o nome {nome_feature}.")
        return

    link = result[0]

    # Abrir o link no navegador padrão
    webbrowser.open(link)
    print(f"Link {link} aberto no navegador.")

    con.close()

def main():
    # Solicitar número do chamado do usuário
    chamado_numero = input("Digite o número do chamado: ")

    # Buscar e abrir o link associado ao chamado
    buscar_e_abrir_link(chamado_numero)

if __name__ == '__main__':
    main()
