import os
import shutil
import subprocess
from tqdm import tqdm

# Constants for SQL server details and file paths
SERVER = "APDNSON0096"
USERNAME = "sa"
PASSWORD = "18|=S1=aHbU{T1Tn"
CMD_SQL_PATH = r"C:\github\fs523673job\projects\cmdSQL"
BASES_LOCAL_PATH = r"C:\Bases_Local"

# Functions to execute SQL Commands
def run_sql(file_name):
    sql_file = os.path.join(CMD_SQL_PATH, file_name)
    cmd = ["sqlcmd", "-S", SERVER, "-U", USERNAME, "-P", PASSWORD, "-i", sql_file]
    if file_name == "PREPARE_BASE.sql":
        cmd.extend(["-v", f"fileName={database}"])
    elif file_name == "StoredProcedure_Integration.sql":
        cmd.extend(["-o", os.path.join(BASES_LOCAL_PATH, "logData.txt")])
    subprocess.run(cmd)

def delete_file_if_exists(path):
    if os.path.exists(path):
        os.remove(path)
        print(f"Arquivo deletado: {path}")

def copy_file_with_progress(src, dest):
    with open(src, 'rb') as fsrc:
        with open(dest, 'wb') as fdst:
            shutil.copyfileobj(fsrc, fdst, length=16*1024*1024)  # Copy with 16MB chunks

def main():
    # User Inputs
    restaurar = input("Restaurar Base Dados e adicionar dados default (*no* apenas restauracao) [yes, no]: ").lower()
    base_beta = input("Restaruar Base Beta [yes, no]: ").lower()

    # Set database based on user input
    global database
    database = "Oficial_31202.BAK" if base_beta == "yes" else "Oficial_55900.BAK"

    # Start process
    print(f"Database a ser restaurado: {database}")
    delete_file_if_exists(os.path.join(BASES_LOCAL_PATH, database))
    delete_file_if_exists(os.path.join(BASES_LOCAL_PATH, "logData.txt"))

    # Copying file
    print(f"Copiando arquivo de '\\192.168.10.209\BackupsOficiais\{database}'")
    source_path = f"\\\\192.168.10.209\\BackupsOficiais\\{database}"
    destination_path = os.path.join(BASES_LOCAL_PATH, database)

    # Show progress bar while copying
    print("Iniciando copia do arquivo...")
    copy_file_with_progress(source_path, destination_path)
    print("Copia finalizada.")

    # Restoring database
    print(f"Iniciando restauracao da base de dados: {database}")
    run_sql("SET_OFFLINE.SQL")
    run_sql("PREPARE_BASE.sql")
    print("Restauracao da base de dados finalizada.")

    if restaurar == "yes":
        print("Iniciando restauracao de dados default.")
        run_sql("StoredProcedure_Integration.sql")
    else:
        print("Dados default nao restaurados.")

    # Log File Verification
    view_file_content = input("Verificar arquivo de log [yes, no]: ").lower()
    if view_file_content == "yes":
        log_path = os.path.join(BASES_LOCAL_PATH, "logData.txt")
        if os.path.exists(log_path):
            with open(log_path, 'r') as log_file:
                print(log_file.read())
        else:
            print("Arquivo de log nao gerado.")

    print("Restauracao finalizada.")

if __name__ == "__main__":
    main()
