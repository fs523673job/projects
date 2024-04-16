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

def check_mr_status(driver, file_path):
    has_mr = False
    with open(file_path, 'r', encoding='utf-8') as file:
        for line in file:
            if 'MR:' in line:
                has_mr = True
                mr_url = line.split('MR:')[1].strip()
                if not mr_url.startswith("http"):
                    continue
                try:
                    driver.get(mr_url)
                    # Lista de seletores para verificar o status
                    selectors = [
                        ".gl-display-none.gl-sm-display-block.gl-ml-2",  # Selector original
                        "",  # Adicione outros seletores conforme necessário
                    ]
                    for selector in selectors:
                        try:
                            status_tag = WebDriverWait(driver, 10).until(
                                EC.visibility_of_element_located((By.CSS_SELECTOR, selector))
                            )
                            if status_tag:
                                return status_tag.text.strip()
                        except:
                            continue
                    return "Não Encontrado"
                except Exception as e:
                    print(f"Erro ao tentar verificar o status do MR em {mr_url}: {e}")
                    continue
    return None

def main():
    # Fecha todas as instâncias do Chrome
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
        status = check_mr_status(driver, file_path)
        if status:
            print(f"Analisando {chamado_name} Status: {status}")

    driver.quit()

if __name__ == '__main__':
    main()
