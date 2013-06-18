set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch green
set __fish_git_prompt_color_dirtystate red
set __fish_git_prompt_color_staged yellow
set __fish_git_prompt_color_upstream green
set __fish_git_prompt_char_dirtystate '• '
set __fish_git_prompt_char_stagedstate '→ '
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'
set __fish_git_prompt_char_upstream_equal '='
set -x fish_greeting

set -x PATH $HOME/dev/git-data/bin $HOME/bin /usr/local/bin /usr/local/checker $PATH /Applications/git-annex.app/Contents/MacOS
set -x LSCOLORS Gxfxcxdxbxegedabagacad
set -x CC clang
set -x CXX clang++
set -x EDITOR vim

. ~/.config/secrets.fish
. /usr/local/share/chruby/chruby.fish
. /usr/local/share/chruby/auto.fish
chruby ruby-2.0