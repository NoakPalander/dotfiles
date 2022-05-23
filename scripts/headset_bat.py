#!/usr/bin/env python

import subprocess

out, _ = subprocess.Popen('headsetcontrol -cb'.split(), stdout=subprocess.PIPE).communicate()

print(f'🎧 {out.decode()}%', flush=True)
