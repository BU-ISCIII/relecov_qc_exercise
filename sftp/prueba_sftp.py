import paramiko
from getpass import getpass

# Tengo que maniobrar en ssh-add...

CLAVE = getpass('Clave: ')
HOST = 'asterix.isciii.es'
PUERTO = 22
USUARIO = 'alberto.lema'

# Inicia un cliente SSH
ssh_client = paramiko.SSHClient()
# Establecer política por defecto para localizar la llave del host localmente
ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())

ssh_client.connect(HOST, 22, USUARIO, CLAVE, allow_agent=False,look_for_keys=False)

entrada, salida, error = ssh_client.exec_command('cd /data/bi/research/20211020_VIRALRECON-TEST_AL-EK-SM-SV_P/ANALYSIS | ls -la')
print (salida.read())
ssh_client.close()
