# verify_mr_db.py
import firebird.driver as fdb
import subprocess
import os
import sys
from datetime import datetime

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

# --------------------------------------------------------------------------------------
# CONFIGURAÇÃO – AJUSTE APENAS SE NECESSÁRIO
# --------------------------------------------------------------------------------------
DB_HOST = 'localhost'
DB_NAME = r'C:\github\bases\firebird\TESTDB_F30.FDB'
DB_USER = 'SYSDBA'
DB_PASSWORD = 'master'
DSN = f"{DB_HOST}:{DB_NAME}"

CHROMEDRIVER = r'C:\ChromeDriver\win64\chromedriver.exe'      # caminho do chromedriver
PROFILE_DIR  = r"C:\ChromeDriverProfiles\GitLabBot"           # perfil dedicado à automação

# --------------------------------------------------------------------------------------
def kill_chrome():
    """Fecha instâncias pré‑existentes do Chrome."""
    try:
        subprocess.run(['taskkill', '/F', '/IM', 'chrome.exe'], check=False, stderr=DEVNULL)
    except Exception as e:
        print(f"Não foi possível fechar todas instâncias do chrome. {e}")


def get_driver():
    """Cria/retorna um webdriver Chrome com perfil dedicado."""
    chrome_options = Options()
    user_data_dir = r'C:\Users\flsantos\AppData\Local\Google\Chrome\User Data'
    profile_directory = 'Default'
    chrome_options.add_argument(f"user-data-dir={user_data_dir}")
    chrome_options.add_argument(f"profile-directory={profile_directory}")
    chrome_options.add_argument('--no-sandbox')
    chrome_options.add_argument('--disable-dev-shm-usage')
    chrome_options.add_argument('--remote-debugging-port=9222')
    chrome_options.add_experimental_option("excludeSwitches", ["enable-automation"])
    chrome_options.add_experimental_option('useAutomationExtension', False)


    service = Service(CHROMEDRIVER)
    return webdriver.Chrome(service=service, options=chrome_options)

def fetch_pending_mrs(conn):
    """Retorna lista de (ID, link, status_atual) a processar."""
    sql = """
        SELECT ID, LINK_MR, COALESCE(STATUS_MR, '') AS STATUS_MR
          FROM FEATURE_CRIADAS
         WHERE LINK_MR IS NOT NULL
           AND (STATUS_MR IS NULL OR STATUS_MR NOT IN ('Merged', 'Closed'))
    """
    cur = conn.cursor()
    cur.execute(sql)
    return cur.fetchall()  # lista de tuplas (id, link, status)

def update_status(conn, feature_id, new_status):
    """Persiste o novo status do MR no banco."""
    cur = conn.cursor()
    cur.execute("UPDATE FEATURE_CRIADAS SET STATUS_MR=? WHERE ID=?", (new_status, feature_id))
    conn.commit()

def check_status(driver, mr_url):
    """
    Abre a URL do MR e devolve 'Open', 'Merged' ou 'Closed'.
    Retorna (status, data_utc_iso) – a data pode ser None se não encontrada.
    """
    driver.get(mr_url)
    selectors = ["span[aria-label='Open']",
                 "span[aria-label='Merged']",
                 "span[aria-label='Closed']"]

    for selector in selectors:
        try:
            tag = WebDriverWait(driver, 5).until(
                EC.presence_of_element_located((By.CSS_SELECTOR, selector))
            )
            status = tag.text.strip()   # Open, Merged ou Closed
            if status in ("Open", "Merged", "Closed"):
                # tenta capturar <time class="js-timeago ... datetime=...">
                try:
                    time_el = driver.find_element(By.CSS_SELECTOR, "time.js-timeago.gl-inline-block")
                    mr_date = time_el.get_attribute('datetime')
                except Exception:
                    mr_date = None
                return status, mr_date
        except Exception:
            # selector não encontrado, tenta o próximo
            pass
    return None, None

# --------------------------------------------------------------------------------------
def main():
    kill_chrome()
    driver = get_driver()

    # conecta ao Firebird
    conn = fdb.connect(dsn=DSN, user=DB_USER, password=DB_PASSWORD)

    try:
        for feature_id, link_mr, current_status in fetch_pending_mrs(conn):
            if not link_mr.lower().startswith("http"):
                continue  # link inválido

            status, mr_date = check_status(driver, link_mr)

            if status and status != current_status:
                update_status(conn, feature_id, status)
                print(f"[{datetime.now():%Y-%m-%d %H:%M:%S}] "
                      f"ID {feature_id}: {link_mr} -> {status} "
                      f"{'(' + mr_date + ')' if mr_date else ''}")
            elif status:
                print(f"ID {feature_id}: status permanece {status}")
            else:
                print(f"ID {feature_id}: não foi possível obter status")

    finally:
        driver.quit()
        conn.close()

if __name__ == "__main__":
    main()

