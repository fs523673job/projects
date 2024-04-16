import requests
from bs4 import BeautifulSoup
import os
import re

def check_mr_status(file_path):
    has_mr = False  # Flag para verificar se o arquivo contém um MR
    headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'}
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            for line in file:
                if line.startswith('MR:'):
                    has_mr = True
                    parts = line.split()
                    if len(parts) > 1:
                        mr_url = parts[1].strip()
                        response = requests.get(mr_url, headers=headers)
                        if response.status_code == 200:
                            soup = BeautifulSoup(response.text, 'html.parser')
                            status_tag = soup.find('span', class_='gl-display-none gl-sm-display-block gl-ml-2')
                            if status_tag:
                                return status_tag.text.strip()
                        return "Não Encontrado"
    except Exception as e:
        print(f"Erro ao ler o arquivo: {e}")
    
    if not has_mr:
        return "Sem MR"

def main():
    default_path = r"C:\Users\flsantos\OneDrive - Apdata do Brasil Software Ltda\Chamados"
    user_input = input(f"Por favor, insira o caminho do diretório dos arquivos ou pressione Enter para usar o padrão ({default_path}): ")
    directory_path = user_input.strip() or default_path

    if not os.path.exists(directory_path):
        print(f"O diretório especificado não existe: {directory_path}")
        return

    files = [f for f in os.listdir(directory_path) if f.endswith(".txt") and re.fullmatch(r'\d+\.txt', f)]
    for filename in files:
        file_path = os.path.join(directory_path, filename)
        chamado_name = filename[:-4]
        status = check_mr_status(file_path)
        if status not in [None, "Sem MR"]:
            print(f"Analisando {chamado_name} Status: {status}")

if __name__ == '__main__':
    main()
