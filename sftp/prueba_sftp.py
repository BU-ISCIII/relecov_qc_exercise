import paramiko
from getpass import getpass

# Tengo que maniobrar en ssh-add...

CLAVE = getpass('Clave: ')
HOST = ''
PUERTO = 22
USUARIO = ''

# Inicia un cliente SSH
ssh_client = paramiko.SSHClient()
# Establecer política por defecto para localizar la llave del host localmente
ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())

ssh_client.connect(HOST, 22, USUARIO, CLAVE, allow_agent=False,look_for_keys=False)

entrada, salida, error = ssh_client.exec_command('cd path | ls -la')
print (salida.read())
ssh_client.close()
