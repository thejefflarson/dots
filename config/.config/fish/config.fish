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

set -x PATH $HOME/dev/git-data/bin $HOME/bin $HOME/.cargo/bin /usr/local/bin /usr/local/sbin /usr/local/checker /usr/local/share/npm/bin $HOME/.cask/bin $PATH
set -x LSCOLORS Gxfxcxdxbxegedabagacad
set -x CC clang
set -x CXX clang++
if test (uname) = "Darwin"
        set -x EDITOR e
else
        set -x EDITOR emacsclient -nw
end
set -x HOMEBREW_NO_ANALYTICS 1


if test -f "~/.config/fish/secrets.fish"
	. ~/.config/fish/secrets.fish
end

if test -d "/usr/local/share/chruby"
	. /usr/local/share/chruby/chruby.fish
	. /usr/local/share/chruby/auto.fish
	chruby 2.3.0
end

status --is-interactive; and source (pyenv init -|psub)
