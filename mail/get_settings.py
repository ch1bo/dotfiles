import subprocess

def get_pass(service, cmd):
    return subprocess.check_output(cmd, )

def get_token(email_address):
    return subprocess.run(["mailctl", "access", email_address], capture_output=True, text=True).stdout
