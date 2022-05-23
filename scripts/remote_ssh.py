#!/usr/bin/python

import argparse
import os
import subprocess

from termcolor import colored


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-c', '--config', default=os.path.expanduser('~/.config/remote_ssh/config'),
                        help='The configuration file to use, formatted like an env file. Defaults to'
                             ' ~/.config/remote_ssh/config')
    parser.add_argument('-d', '--disconnect', default=False, action='store_true',
                        help='To disconnect or not, defaults to false.')
    parser.add_argument('-v', '--verbose', default=False, action='store_true',
                        help='Prints verbose output.')
    args = parser.parse_args()

    if not os.path.exists(args.config):
        raise FileNotFoundError('Failed to find a config file!')

    configuration = {}
    with open(args.config) as reader:
        for line in reader.readlines():
            key, val = line.strip().split('=')
            configuration[key] = val

    if args.verbose:
        print('Using config:')
        for (key, val) in configuration.items():
            print('{k}={v}'.format(
                k=colored(key, 'blue'),
                v=colored(val, 'green')
            ))

    if args.disconnect:
        command = f'{configuration["ROOT"]} umount {configuration["MOUNT"]}'
        if args.verbose:
            print(f'\nUsing command:\n{command}')

        subprocess.Popen(command.split()).wait()

    else:
        command = '{root} sshfs {path} {mount} -o IdentityFile={identity},allow_other,default_permissions -p {port}'.format(
            root=configuration['ROOT'],
            path=configuration['PATH'],
            mount=configuration['MOUNT'],
            identity=configuration['IDENTITY'],
            port=configuration['PORT']
        )

        if args.verbose:
            print(f'\nUsing connect command:\n{command}')

        subprocess.Popen(command.split()).wait()


if __name__ == '__main__':
    main()
