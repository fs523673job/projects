from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
import time

def main():
    url = "http://localhost/559"  # Ajuste conforme o necessário
    
    service = Service(ChromeDriverManager().install())
    driver = webdriver.Chrome(service=service)
    
    try:
        driver.get(url)
        
        wait = WebDriverWait(driver, 10)
        
        # 1. Aguardar e clicar no botão "Confirmar preferências"
        #    Localizando pelo texto da <span>:
        botao_confirmar = wait.until(
            EC.element_to_be_clickable((By.XPATH, "//span[text()='Confirmar preferências']"))
        )
        botao_confirmar.click()
        
        # 2. Aguardar para garantir que o modal sumiu ou a página esteja disponível
        #    (isso depende do comportamento do site; se precisar, aumente o tempo ou faça outro wait)
        time.sleep(1)  # Pequena pausa apenas se necessário
        
        # 3. Continuar o fluxo de login (exemplo)
        user_field = wait.until(
            EC.presence_of_element_located((By.CSS_SELECTOR, ".loginField.username"))
        )
        pass_field = driver.find_element(By.CSS_SELECTOR, ".loginField.pass")

        user_field.send_keys("teste30")
        pass_field.send_keys("85&$g1Sdux")

        # Supondo que tenha um botão com ID "ext-131" (classe btLogin):
        botao_entrar = driver.find_element(By.ID, "ext-131")
        botao_entrar.click()
        
        # 4. Caso precise aguardar a tela pós-login
        wait.until(
            EC.presence_of_element_located((By.CSS_SELECTOR, ".dashboard"))
        )
        
        print("Login realizado com sucesso!")
        print("Título da página:", driver.title)
        time.sleep(3)
    
    except Exception as e:
        print(f"Erro durante o processo: {e}")
    finally:
        driver.quit()

if __name__ == "__main__":
    main()
