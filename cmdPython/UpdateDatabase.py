import os
import shutil
import subprocess
from tqdm import tqdm
from datetime import datetime

# Constants for SQL server details and file paths
SERVER = "APDNSON0220"
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

def rename_file_if_exists(path):
    if os.path.exists(path):
        directory, filename = os.path.split(path)
        name, extension = os.path.splitext(filename)
        current_time = datetime.now().strftime("%Y%m%d_%H%M%S")
        new_filename = f"{name}_{current_time}{extension}"
        new_path = os.path.join(directory, new_filename)
        os.rename(path, new_path)
        print(f"Arquivo renomeado para: {new_path}")

def copy_file_with_progress(src, dest):
    # Make sure the source file exists before trying to copy
    if not os.path.exists(src):
        print("O arquivo fonte não existe.")
        return

    filesize = os.path.getsize(src)
    with open(src, 'rb') as fsrc, open(dest, 'wb') as fdst, tqdm(
            total=filesize, unit='B', unit_scale=True, desc="Copiando") as bar:
        while True:
            buffer = fsrc.read(1024 * 1024)  # Read 1MB at a time
            if not buffer:
                break
            fdst.write(buffer)
            bar.update(len(buffer))

def main():
    # Solicitando entrada do usuário
    copiar_arquivo = input("Copiar novo backup da base? [yes, no]: ").lower()
    restaurar = input("Restaurar Base Dados e adicionar dados default (*no* apenas restauracao) [yes, no]: ").lower()
    base_beta = input("Restaurar Base Beta [yes, no]: ").lower()

    # Set database based on user input
    global database
    database = "Oficial_31202.BAK" if base_beta == "yes" else "Oficial_55900.BAK"
    source_path = rf"\\172.26.7.209\BackupsOficiais\{database}"

    if (copiar_arquivo == "yes"):
        if not os.path.isfile(source_path):
            print(f"Arquivo do database {source_path} nao encontrado e o script sera finalizado")
            exit()

    # Start process
    print(f"Database a ser restaurado: {database}")
    if copiar_arquivo == "yes":
        rename_file_if_exists(os.path.join(BASES_LOCAL_PATH, database))
        delete_file_if_exists(os.path.join(BASES_LOCAL_PATH, database))
    delete_file_if_exists(os.path.join(BASES_LOCAL_PATH, "logData.txt"))

    if copiar_arquivo == "yes":
        # Copying file
        destination_path = os.path.join(BASES_LOCAL_PATH, database)
        print(f"Copiando arquivo de '{source_path}'")

        # Show progress bar while copying
        print("Iniciando copia do arquivo...")
        copy_file_with_progress(source_path, destination_path)
        print("Copia finalizada.")
    else:
        destination_path = os.path.join(BASES_LOCAL_PATH, database)
        if not os.path.isfile(destination_path):
            print(f"O arquivo {destination_path} não existe localmente. Não é possível restaurar sem uma cópia válida.")
            exit()

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
