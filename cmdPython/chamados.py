import fdb
import os
import shutil
from datetime import datetime

def solicitar_dados_inicio_chamado():
    PT_Nome = input("Digite o PT_Nome: ")
    Descricao = input("Digite a Descrição: ")
    Versao_Cliente = input("Digite a Versão do Cliente: ")
    Emergencial = input("É emergencial? (Sim/Não): ")
    Emergencial = 1 if Emergencial.lower() == "sim" else 0
    Analista = input("Digite o nome do Analista: ")
    Palavras_Chave = input("Digite as Palavras-Chaves (separadas por vírgula): ")

    return PT_Nome, Descricao, Versao_Cliente, Emergencial, Analista, Palavras_Chave

def registro_existe(PT_Nome):
    DB_HOST = 'localhost'
    DB_NAME = r'C:\github\bases\firebird\TESTDB_F30.FDB'
    DB_USER = 'SYSDBA'
    DB_PASSWORD = 'masterkey'

    con = fdb.connect(host=DB_HOST, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
    cur = con.cursor()

    cur.execute("SELECT COUNT(*) FROM Chamados WHERE PT_Nome = ?", (PT_Nome,))
    count = cur.fetchone()[0]
    con.close()

    return count > 0

def gravar_no_banco(dados):
    DB_HOST = 'localhost'
    DB_NAME = r'C:\github\bases\firebird\TESTDB_F30.FDB'
    DB_USER = 'SYSDBA'
    DB_PASSWORD = 'masterkey'

    con = fdb.connect(host=DB_HOST, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
    cur = con.cursor()

    sql = """
    INSERT INTO Chamados (PT_Nome, Descricao, Versao_Cliente, Emergencial, Analista, Palavras_Chave) 
    VALUES (?, ?, ?, ?, ?, ?)
    """
    cur.execute(sql, dados)
    con.commit()
    con.close()

def duplicar_e_preencher_arquivo(PT_Nome, Descricao):
    diretorio = r"C:\Users\flsantos\OneDrive - Apdata do Brasil Software Ltda\Chamados"
    origem = os.path.join(diretorio, "Modelo_Chamado.Txt")
    destino = os.path.join(diretorio, f"{PT_Nome}.txt")

    if not os.path.exists(destino):
        shutil.copy(origem, destino)

        with open(destino, 'a', encoding='utf-8') as arquivo:
            arquivo.write(f"\nChamado: {PT_Nome}\n")
            arquivo.write(f"Descricao: {Descricao}\n")
            arquivo.write(f"Data Início Análise: {datetime.now().strftime('%d/%m/%Y')}\n")

def main():
    acao = input("Qual é a ação? (Inicio Chamado [I], Atualizar Chamado [A], Finalizar Chamado [F]): ")

    if acao == "i":
        dados = solicitar_dados_inicio_chamado()
        PT_Nome = dados[0]

        if not registro_existe(PT_Nome):
            gravar_no_banco(dados)
            duplicar_e_preencher_arquivo(PT_Nome, dados[1])
            print("Chamado iniciado com sucesso!")
        else:
            print(f"O chamado {PT_Nome} já existe.")

    # Aqui você pode adicionar as outras ações (Atualizar Chamado, Finalizar Chamado) conforme necessário.

if __name__ == '__main__':
    main()
