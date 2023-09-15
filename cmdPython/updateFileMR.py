import fdb
import os
import re

def buscar_links():
    # Configurações de conexão com o banco Firebird
    DB_HOST = 'localhost'
    DB_NAME = r'C:\github\bases\firebird\TESTDB_F30.FDB'
    DB_USER = 'SYSDBA'
    DB_PASSWORD = 'masterkey'

    # Conexão com o banco de dados
    con = fdb.connect(host=DB_HOST, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
    cur = con.cursor()

    # Buscar todos os registros com LINK_MR
    cur.execute("SELECT NAME_FEATURE, LINK_MR FROM FEATURE_CRIADAS WHERE LINK_MR IS NOT NULL")
    resultados = cur.fetchall()

    con.close()

    return resultados

def atualizar_arquivo_com_link(diretorio: str, chamado_numero: str, link: str):
    arquivo_nome = os.path.join(diretorio, f"{chamado_numero}.txt")

    if not os.path.exists(arquivo_nome):
        print(f"Arquivo {arquivo_nome} não encontrado.")
        return

    with open(arquivo_nome, 'r', encoding='utf-8') as arquivo:
        linhas = arquivo.readlines()

    linhas_originais = linhas.copy()

    for i, linha in enumerate(linhas):
        if "MR:" in linha:
            # Se já tiver um link após "MR:", não alterar o arquivo
            if "http" not in linha:
                linhas[i] = f"MR: {link}\n"
                break
        elif "Colateral (PT):" in linha and not any("MR:" in l for l in linhas[i:]):
            linhas.insert(i + 1, f"MR: {link}\n")
            break

    # Verificar se houve alterações comparando as listas de linhas
    if linhas != linhas_originais:
        with open(arquivo_nome, 'w', encoding='utf-8') as arquivo:
            arquivo.writelines(linhas)
        print(f"Arquivo {arquivo_nome} atualizado com sucesso!")
    else:
        print(f"Nenhuma alteração necessária para o arquivo {arquivo_nome}.")

def main():
    # Diretório onde os arquivos estão localizados
    diretorio = r"C:\Users\flsantos\OneDrive - Apdata do Brasil Software Ltda\Chamados"  # Altere para o diretório correto

    # Buscar todos os registros com LINK_MR
    registros = buscar_links()

    for registro in registros:
        nome_feature = registro[0]
        link = registro[1]

        # Extrair o número do chamado do nome da feature usando regex
        match = re.search(r'(\d+)', nome_feature)
        if match:
            chamado_numero = match.group(1)
            # Atualizar o arquivo com o link
            atualizar_arquivo_com_link(diretorio, chamado_numero, link)

if __name__ == '__main__':
    main()
