import json

with open('renv.lock') as f:
    lock = json.load(f)

std = {'Package','Version','Source','Repository','Hash','Requirements',
       'RemoteType','RemoteHost','RemoteUsername','RemoteRepo','RemoteRef',
       'RemoteSha','RemoteSubdir','RemotePkgRef'}

for pkg in lock.get('Packages', {}).values():
    for k in list(pkg.keys()):
        if k not in std:
            del pkg[k]

with open('renv.lock', 'w') as f:
    json.dump(lock, f, indent=2)
