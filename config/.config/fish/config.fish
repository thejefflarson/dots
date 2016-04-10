set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch 666666
set __fish_git_prompt_color_dirtystate red
set __fish_git_prompt_color_staged yellow
set __fish_git_prompt_color_upstream 666666
set __fish_git_prompt_char_dirtystate '• '
set __fish_git_prompt_char_stagedstate '→ '
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'
set __fish_git_prompt_char_upstream_equal '='
set -x fish_greeting

set -x PATH $HOME/dev/git-data/bin $HOME/bin /usr/local/bin /usr/local/sbin /usr/local/checker /usr/local/share/npm/bin $PATH 
set -x LSCOLORS Gxfxcxdxbxegedabagacad
set -x CC clang
set -x CXX clang++
set -x EDITOR emacs
alias python "python3"
alias pip "pip3"

if test -f "~/.config/fish/secrets.fish"
	. ~/.config/fish/secrets.fish
end

if test -d "/usr/local/share/chruby"
	. /usr/local/share/chruby/chruby.fish
	. /usr/local/share/chruby/auto.fish
	chruby ruby-2.2
end
