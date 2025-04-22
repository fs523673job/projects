import firebird.driver as fdb
import subprocess
import os
from unidecode import unidecode
import re

def insert_into_firebird(input_name, input_sha, branch_name, regex_search, feature_desc, feature_type, name_pt):
    # Configurações de conexão com o banco Firebird
    DB_HOST = 'localhost'
    DB_NAME = r'C:\github\bases\firebird\TESTDB_F30.FDB'
    DB_USER = 'SYSDBA'
    DB_PASSWORD = 'master'

    dsn = f"{DB_HOST}:{DB_NAME}"

    # Remove acentuação
    regex_search = unidecode(regex_search)
    feature_desc = unidecode(feature_desc)

    # Remove tudo que não é dígito de name_pt
    name_pt = re.sub(r'\D', '', name_pt)

    # Conexão com o banco de dados
    con = fdb.connect(dsn, user=DB_USER, password=DB_PASSWORD)
    cur = con.cursor()
    cur.execute("SELECT COUNT(*) FROM FEATURE_CRIADAS")
    count = cur.fetchone()[0]
    print(f"Total de registros na tabela FEATURE_CRIADAS: {count}")

    # SQL para inserir os dados
    sql = """
    INSERT INTO FEATURE_CRIADAS (NAME_FEATURE, NAME_BRANCH, SHA_ORIGEM, REGEX_SEARCH, TYPE_FEATURE, DESC_PT, NAME_PT)
    VALUES (?, ?, ?, ?, ?, ?, ?)
    """

    # Executa a SQL
    cur.execute(sql, (input_name, input_sha, branch_name, regex_search, feature_type, feature_desc, name_pt))
    con.commit()
    con.close()


def get_feature_type(feature_name: str) -> str:
    return feature_name.split('_')[-1]


def extract_feature_number(feature_name: str) -> str:
    parts = feature_name.split('_')
    if len(parts) >= 2:
        return parts[1]
    else:
        print(f"Erro: O formato da feature '{feature_name}' não contém o número esperado.")
        return ""


def update_feature_file(feature_name: str):
    feature_number = extract_feature_number(feature_name)
    if not feature_number:
        print("Erro: Não foi possível extrair o número da feature.")
        return

    file_path = fr"C:\Users\flsantos\OneDrive - Apdata do Brasil Software Ltda\Chamados\{feature_number}.txt"

    # Verifica se o arquivo existe antes de tentar abrir
    if not os.path.exists(file_path):
        print(f"Erro: Arquivo {file_path} não encontrado.")
        return

    try:
        updated = False

        # 'r+' para leitura e escrita
        with open(file_path, 'r+', encoding='utf-8') as file:
            lines = file.readlines()

            # Volta ao início do arquivo para começar a escrever
            file.seek(0)
            file.truncate()  # Limpa o arquivo

            for line in lines:
                if line.startswith("Features Criadas:"):
                    parts = line.strip().split(": ")
                    existing_features = parts[1] if len(parts) > 1 else ""
                    updated_features = f"{existing_features}, {feature_name}" if existing_features else feature_name
                    line = f"Features Criadas: {updated_features}\n"
                    updated = True
                file.write(line)

            if not updated:
                file.write(f"Features Criadas: {feature_name}\n")

        print(f"Arquivo '{file_path}' atualizado com sucesso.")
    except UnicodeDecodeError as e:
        print(f"Erro ao decodificar o arquivo: {e}")
    except FileNotFoundError:  # Embora já tenhamos verificado a existência do arquivo, isso é por precaução.
        print(f"Erro: Arquivo {file_path} não encontrado.")
    except Exception as e:
        print(f"Erro desconhecido: {e}")


def main():
    diretorios_disponiveis = [
        r"c:/apdata_x64",
        r"c:/apdata_xwt",
        # Adicione outros caminhos futuramente, se desejar
    ]

    print("Selecione o diretório desejado para a operação:")
    for i, d in enumerate(diretorios_disponiveis, start=1):
        print(f"{i} - {d}")

    escolha = input("Digite o número da opção: ")

    try:
        escolha_int = int(escolha)
        if 1 <= escolha_int <= len(diretorios_disponiveis):
            diretorio_escolhido = diretorios_disponiveis[escolha_int - 1]
        else:
            print("Opção inválida. Encerrando.")
            return
    except ValueError:
        print("Entrada inválida. Encerrando.")
        return

    os.chdir(diretorio_escolhido)

    branch_name = input("Digite o nome da branch: ")
    feature_name = input("Digite o nome da feature: ")
    feature_desc = input("Digite a descricao do chamado: ")
    regex_search = input("Digite o regex de busca: ")
    feature_type = get_feature_type(feature_name)

    # Remove acentuação
    feature_desc = unidecode(feature_desc)
    regex_search = unidecode(regex_search)

    args = ['C:/Program Files/Git/bin/git.exe', 'fscreatefeaturefrombranch', branch_name, feature_name]
    result = subprocess.run(args, capture_output=True, text=True)
    print("Saída completa do Bash:\n", result.stdout)

    data = {}
    for line in result.stdout.split("\n"):
        if "=" in line:
            key, value = line.split("=")
            data[key.strip()] = value.strip()

    print("Data extraído do Bash:", data)

    if 'NAME_FEATURE' in data and 'NAME_BRANCH' in data and 'SHA_ORIGEM' in data:
        # Remove tudo que não é dígito de name_pt
        name_pt = re.sub(r'\D', '', data['NAME_FEATURE'])
        insert_into_firebird(data['NAME_FEATURE'], data['NAME_BRANCH'], data['SHA_ORIGEM'], regex_search, feature_desc, feature_type, name_pt)
        update_feature_file(data['NAME_FEATURE'])  # Atualiza o arquivo
    else:
        print("Erro: Não foi possível obter todas as informações necessárias da saída do script Bash.")


if __name__ == '__main__':
    main()
