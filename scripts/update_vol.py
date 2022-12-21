#!/usr/bin/python3

"""
This is purely used for ckb-next on my corsair keyboard, doubt it'll be useful for anything else.
"""

import subprocess
import os
import sys


def main():
    if not os.path.exists('/usr/bin/pamixer'):
        raise FileNotFoundError('increase_vol requires pamixer to be installed.')

    proc = subprocess.Popen('pamixer --get-volume'.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = proc.communicate()
    proc.wait()
    if err:
        raise RuntimeError(err.decode('utf-8'))

    if sys.argv[1] == '--decrease':
        vol = max(int(out) - 2, 0)
    elif sys.argv[1] == '--increase':
        vol = min(int(out) + 2, 100)

    subprocess.run(f'pamixer --set-volume {vol}'.split())


if __name__ == '__main__':
    main()
