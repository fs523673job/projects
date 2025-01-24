import argparse
import time

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager

from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


def parse_args():
    """
    Lê os parâmetros de linha de comando:
      --headless (true/false) -> se deve rodar sem interface gráfica
      --url (str)             -> URL alvo do sistema
      --times (int)           -> quantidade de repetições
    """
    parser = argparse.ArgumentParser(description="Script de login via Selenium com parâmetros.")
    parser.add_argument("--headless", help="Executar em modo headless (true/false)", default="false")
    parser.add_argument("--url", help="URL do sistema", default="http://localhost/559")
    parser.add_argument("--times", help="Quantidade de vezes que o script será executado", default="1")
    return parser.parse_args()


def main():
    args = parse_args()
    
    # Converte 'times' para inteiro
    times_to_run = int(args.times)
    
    # Mensagem inicial
    print(f"URL alvo: {args.url}")
    print(f"Headless: {args.headless}")
    print(f"Repetições: {times_to_run}")
    
    for i in range(1, times_to_run + 1):
        print(f"\n--- Iniciando iteração {i} de {times_to_run} ---")
        
        # 1. Configurar opções do Chrome
        chrome_options = Options()
        if args.headless.lower() == "true":
            chrome_options.add_argument("--headless")
            chrome_options.add_argument("--disable-gpu")
            chrome_options.add_argument("--window-size=1920,1080")
        
        # 2. Instanciar o driver do Chrome (com webdriver_manager)
        service = Service(ChromeDriverManager().install())
        driver = webdriver.Chrome(service=service, options=chrome_options)
        
        # Criar um WebDriverWait para uso posterior
        wait = WebDriverWait(driver, 10)  # até 10s de espera
        
        try:
            # 3. Abrir a página indicada
            driver.get(args.url)
            print(f"[Iteração {i}] Acessando: {args.url}")
            
            # 4. (Opcional) Clicar no botão "Confirmar preferências" (caso exista)
            try:
                botao_confirmar = wait.until(
                    EC.element_to_be_clickable((By.XPATH, "//span[text()='Confirmar preferências']"))
                )
                botao_confirmar.click()
                print(f"[Iteração {i}] Botão 'Confirmar preferências' encontrado e clicado.")
            except Exception:
                print(f"[Iteração {i}] Botão 'Confirmar preferências' não encontrado, prosseguindo.")
            
            # 5. Localizar e preencher campo de usuário (ex.: .loginField.username)
            user_field = wait.until(
                EC.visibility_of_element_located((By.CSS_SELECTOR, ".loginField.username"))
            )
            user_field.send_keys("teste30")
            
            # 6. Localizar e preencher campo de senha (ex.: .loginField.pass)
            pass_field = driver.find_element(By.CSS_SELECTOR, ".loginField.pass")
            pass_field.send_keys("85&$g1Sdux")

            # 7. Localizar e clicar no botão de login (ex.: id="ext-131")
            botao_entrar = driver.find_element(By.ID, "ext-131")
            botao_entrar.click()
            print(f"[Iteração {i}] Clique no botão de login realizado.")
            
            # 8. Aguardar um elemento na tela pós-login (ex.: .dashboard)
            wait.until(
                EC.presence_of_element_located((By.CSS_SELECTOR, ".dashboard"))
            )
            
            print(f"[Iteração {i}] Login efetuado com sucesso!")
            print(f"[Iteração {i}] Título da página pós-login: {driver.title}")
            
            # Pausa opcional para ver o estado (no modo headless não tem efeito visual)
            time.sleep(2)
            
            print(f"[Iteração {i}] Saindo sem executar o logout!")
            driver.get("https://www.google.com.br")
            
            time.sleep(1)

        except Exception as e:
            print(f"[Iteração {i}] Erro: {e}")
        finally:
            # 9. Fechar o navegador
            driver.quit()


if __name__ == "__main__":
    main()
