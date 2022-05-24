#!/usr/bin/bash

echo "Don't forget to 'exit' to finalize the theme install."
curl https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install > install
fish install --path=~/.local/share/omf --config=~/.config/omf
fish -c "omf install kawasaki"
