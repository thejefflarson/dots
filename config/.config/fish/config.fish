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

set -x PATH $HOME/bin $HOME/.cask/bin/ /usr/local/bin $HOME/.cargo/bin $PATH
set -x LSCOLORS Gxfxcxdxbxegedabagacad
set -x CC clang
set -x CXX clang++

set -x EDITOR 'emacsclient -nc -a emacs'
set -x HOMEBREW_NO_ANALYTICS 1
alias e "emacsclient -nc -a emacs"

if test -f "~/.config/fish/secrets.fish"
	. ~/.config/fish/secrets.fish
end

if test -d "/usr/local/share/chruby"
	. /usr/local/share/chruby/chruby.fish
	. /usr/local/share/chruby/auto.fish
	chruby 2.3.0
end

status --is-interactive; and source (pyenv init -|psub)
set -g fish_user_paths "/usr/local/opt/llvm/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/opt/flex/bin" $fish_user_paths
