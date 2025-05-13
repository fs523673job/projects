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

def update_file_with_status(file_path, mr_url, status, date=None):
    lines = []
    with open(file_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()

    updated = False
    i = 0
    while i < len(lines):
        line = lines[i]
        if line.strip() == f'MR: {mr_url}':
            # Encontrou o MR, agora procura pelo status e data
            i += 1
            while i < len(lines) and (lines[i].strip().startswith('Status Merge Request:') or lines[i].strip().startswith('Data Merge Request:')):
                if lines[i].strip().startswith('Status Merge Request:'):
                    lines[i] = f'Status Merge Request: {status}\n'  # Atualiza o status existente
                elif date and lines[i].strip().startswith('Data Merge Request:'):
                    lines[i] = f'Data Merge Request: {date}\n'  # Atualiza a data existente
                i += 1
            # Se não encontrou 'Status Merge Request:', adiciona após o MR
            if not any('Status Merge Request:' in line for line in lines):
                lines.insert(i, f'Status Merge Request: {status}\n')
                i += 1
            # Se não encontrou 'Data Merge Request:' e a data existe, adiciona após o status
            if date and not any('Data Merge Request:' in line for line in lines):
                lines.insert(i, f'Data Merge Request: {date}\n')
            updated = True
            break  # MR encontrado e atualizado, sai do loop
        i += 1
    if not updated:
        # Se o MR não foi encontrado no arquivo, adiciona no final
        lines.append(f'\nMR: {mr_url}\n')
        lines.append(f'Status Merge Request: {status}\n')
        if date:
            lines.append(f'Data Merge Request: {date}\n')

    # Escreve de volta no arquivo
    with open(file_path, 'w', encoding='utf-8') as file:
        file.writelines(lines)

def check_mr_status(driver, file_path):
    mr_url = None
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read()

    if "Status Merge Request: Merged" in content or "Status Merge Request: Closed" in content:
        return "Already Processed", None, None  # Verifica se o arquivo já foi processado por MRs Merged ou Closed

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
                                    # Extrai a data do elemento <time>
                                    try:
                                        time_element = driver.find_element(By.CSS_SELECTOR, "time.js-timeago.gl-inline-block")
                                        mr_date = time_element.get_attribute('datetime')
                                    except Exception as e:
                                        print(f"Erro ao extrair a data do MR: {e}")
                                        mr_date = None
                                    if status_text in ["Merged", "Closed"]:
                                        update_file_with_status(file_path, mr_url, status_text, mr_date)
                                    else:
                                        update_file_with_status(file_path, mr_url, status_text)
                                    return status_text, mr_url, mr_date
                        except:
                            continue
                except Exception as e:
                    print(f"Erro ao tentar verificar o status do MR em {mr_url}: {e}")
        return None, None, None

def main():
    try:
        subprocess.run(['taskkill', '/F', '/IM', 'chrome.exe'], check=True)
    except subprocess.CalledProcessError as e:
        print(f"Não foi possível fechar as instâncias do Chrome: {e}")

    chrome_options = Options()
    user_data_dir = r'C:\Users\flsantos\AppData\Local\Google\Chrome\User Data'
    profile_directory = 'Profile 3'
    chrome_options.add_argument(f"user-data-dir={user_data_dir}")
    chrome_options.add_argument(f"profile-directory={profile_directory}")
    chrome_options.add_argument(f"--remote-debugging-port=9222")
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
        status, linkMR, mr_date = check_mr_status(driver, file_path)
        if status and status != "Already Processed":
            print(f"Analisando {chamado_name} Status: [{status}] LinkMR: [{linkMR}] Data: [{mr_date}]")

    driver.quit()

if __name__ == '__main__':
    main()
