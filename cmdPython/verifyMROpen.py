import sys
import os
import re
import subprocess
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

def update_file_with_status(file_path, mr_url, status):
    updated = False
    lines = []
    with open(file_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()
    
    with open(file_path, 'w', encoding='utf-8') as file:
        for line in lines:
            if line.strip() == f'MR: {mr_url}' and not updated:
                file.write(line)  # Reescreve a linha do MR
                next_index = lines.index(line) + 1
                if next_index < len(lines) and lines[next_index].strip().startswith('Status MR:'):
                    file.write(f'Status MR: {status}\n')  # Atualiza o status se já existe
                    updated = True
                else:
                    file.write(f'Status MR: {status}\n')  # Adiciona o status se não existir
                    updated = True
            else:
                file.write(line)
        if not updated:
            # Se o MR não foi encontrado no arquivo, adiciona como novo
            file.write(f'MR: {mr_url}\n')
            file.write(f'Status MR: {status}\n')

def check_mr_status(driver, file_path):
    mr_url = None
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read()
    
    if "Status MR: Merged" in content or "Status MR: Closed" in content:
        return "Already Processed", None  # Verifica se o arquivo já foi processado por MRs Merged ou Closed

    with open(file_path, 'r', encoding='utf-8') as file:
        for line in file:
            if 'MR:' in line:
                mr_url = line.split('MR:')[1].strip()
                if not mr_url.startswith("http"):
                    continue
                try:
                    driver.get(mr_url)
                    print(f"URL: {mr_url}")
                    selectors = ["span[aria-label='Open']", "span[aria-label='Merged']", "span[aria-label='Closed']"] 
                    for selector in selectors:
                        try:
                            status_tags = WebDriverWait(driver, 10).until(
                                EC.presence_of_all_elements_located((By.CSS_SELECTOR, selector))
                            )
                            for status_tag in status_tags:
                                status_text = status_tag.text.strip()
                                if status_text in ["Open", "Merged", "Closed"]:
                                    if status_text in ["Merged", "Closed"]:
                                        update_file_with_status(file_path, mr_url, status_text)
                                    return status_text, mr_url
                        except:
                            continue
                except Exception as e:
                    print(f"Erro ao tentar verificar o status do MR em {mr_url}: {e}")
    return None, None

def main():
    try:
        subprocess.run(['taskkill', '/F', '/IM', 'chrome.exe'], check=True)
    except subprocess.CalledProcessError as e:
        print(f"Não foi possível fechar as instâncias do Chrome: {e}")

    chrome_options = Options()
    user_data_dir = r'C:\Users\flsantos\AppData\Local\Google\Chrome\User Data'
    profile_directory = 'Default'
    chrome_options.add_argument(f"user-data-dir={user_data_dir}")
    chrome_options.add_argument(f"profile-directory={profile_directory}")
    chrome_options.add_experimental_option("excludeSwitches", ["enable-automation"])
    chrome_options.add_experimental_option('useAutomationExtension', False)

    service = Service(executable_path=r'C:\ChromeDriver\Win64\chromedriver.exe')
    driver = webdriver.Chrome(service=service, options=chrome_options)

    directory_path = sys.argv[1] if len(sys.argv) > 1 else r"C:\Users\flsantos\OneDrive - Apdata do Brasil Software Ltda\Chamados"
    
    if not os.path.exists(directory_path):
        print("O diretório especificado não existe: {}".format(directory_path))
        driver.quit()
        return

    files = [f for f in os.listdir(directory_path) if f.endswith(".txt") and re.fullmatch(r'\d+\.txt', f)]
    for filename in files:
        file_path = os.path.join(directory_path, filename)
        chamado_name = filename[:-4]
        status, linkMR = check_mr_status(driver, file_path)
        if status and status != "Already Processed":
            print(f"Analisando {chamado_name} Status: [{status}] LinkMR: [{linkMR}]")

    driver.quit()

if __name__ == '__main__':
    main()
