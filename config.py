import os

settings = {
    'cosmos_url': os.environ.get('COSMOS_URL', 'https://ams-ant-nf.documents.azure.com:443/'),
    'cosmos_key': os.environ.get('COSMOS_KEY', 'sxXXHZm30fUOz4j3rWWwt8RjyCWp3SvqTpalmhAJShXXV79Doj0msmf6NL0ACYAvIdoZSgoYTPAhACDb7IvZPA=='),
    'cosmos_database': os.environ.get('COSMOS_DATABASE', 'datos-nivel-freatico'),
    'cosmos_container_jigsaw' : os.environ.get('COSMOS_CONTAINER_JIGSAW', 'jigsaw-variables'),
    'jigsaw_user_nam' : os.environ.get('JIGSAW_USER_NAM', 'mpineda'),
    'jigsaw_user_pwd' : os.environ.get('JIGSAW_USER_PWD', '$Antucoya2022$'),
    'jigsaw_server_add' : os.environ.get('JIGSAW_SERVER_ADD', '10.16.200.248'),
    'modulos_user_nam' : os.environ.get('MODULOS_USER_NAM', 'cprades'),
    'modulos_user_pwd' : os.environ.get('MODULOS_USER_PWD', 'Antucoya2020$'),
    'modulos_server_add' : os.environ.get('MODULOS_SERVER_ADD', '10.16.200.14')
}