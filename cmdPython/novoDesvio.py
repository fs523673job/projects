import fdb
import datetime

# Configurações de conexão com o banco Firebird
DB_HOST = 'localhost'
DB_NAME = r'C:\github\bases\firebird\TESTDB_F30.FDB'
DB_USER = 'SYSDBA'
DB_PASSWORD = 'masterkey'

# Conexão com o banco de dados
con = fdb.connect(host=DB_HOST, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
cur = con.cursor()

def insert_into_table(chamadoatual, motivo, descricao, chamadodesvio, tipodesvio):
    try:
        cur.execute("""
            INSERT INTO Desvios (ChamadoAtual, Motivo, Descricao, ChamadoDesvio, TipoDesvio)
            VALUES (?, ?, ?, ?, ?)
        """, (chamadoatual, motivo, descricao, chamadodesvio, tipodesvio))
        con.commit()
        print("Dados inseridos com sucesso!")
    except Exception as e:
        print(f"Erro ao inserir dados: {e}")
        con.rollback()

def update_table(chamadoatual):
    try:
        cur.execute("""
            UPDATE Desvios
            SET DataFinal = ?
            WHERE ChamadoAtual = ? AND DataFinal IS NULL
        """, (datetime.datetime.now(), chamadoatual))
        con.commit()
        print("Dados atualizados com sucesso!")
    except Exception as e:
        print(f"Erro ao atualizar dados: {e}")
        con.rollback()

action = input("Você deseja fazer uma 'abertura' ou 'fechamento'? ")

if action == "abertura":
    chamadoatual = int(input("Digite o chamado atual: "))
    motivo = input("Digite o motivo: ")
    descricao = input("Digite a descrição: ")
    chamadodesvio = input("Digite o chamado desvio: ")
    while True:
        tipodesvio = input("Digite o tipo de desvio (reuniao, ajuda, chamados, emergencial, outros): ").lower()
        if tipodesvio in ['reuniao', 'ajuda', 'chamados', 'emergencial', 'outros']:
            break
        else:
            print("Tipo de desvio inválido. Tente novamente.")
    insert_into_table(chamadoatual, motivo, descricao, chamadodesvio, tipodesvio)
elif action == "fechamento":
    chamadoatual = int(input("Digite o chamado atual para fechamento: "))
    update_table(chamadoatual)
else:
    print("Opção inválida!")

con.close()
