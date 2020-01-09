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
set -x LSCOLORS gxfxbEaEBxxEhEhBaDaCaD

set -x EDITOR 'emacsclient -c -a emacs'
set -x HOMEBREW_NO_ANALYTICS 1
set -x PIPENV_SHELL_COMPAT 1
alias e "emacsclient -nc -a emacs"

if test -e $HOME/.config/fish/local.fish
    . ~/.config/fish/local.fish
end

if status is-login
    set -g fish_user_paths ~/bin $fish_user_paths
    set -g fish_user_paths ~/go/bin $fish_user_paths
    set -g fish_user_paths ~/.cask/bin $fish_user_paths
    set -g fish_user_paths ~/.cargo/bin $fish_user_paths
    set -g fish_user_paths /usr/local/bin $fish_user_paths
    set -g fish_user_paths ~/.local/bin $fish_user_paths
    set -g fish_user_paths "/usr/local/opt/llvm/bin" $fish_user_paths
    set -g fish_user_paths "/usr/local/opt/bison/bin" $fish_user_paths
    set -g fish_user_paths "/usr/local/opt/flex/bin" $fish_user_paths
    set -g fish_user_paths ~/dev/esp/xtensa-esp32-elf/bin/ $fish_user_paths
    set -x CC (which clang)
    set -x CXX (which clang++)
    set -x RUST_SRC_PATH (rustc --print sysroot)/lib/rustlib/src/rust/src
    set -x NVM_DIR ~/.nvm
    set -x IDF_PATH ~/dev/esp/esp-idf
    set -g fish_user_paths "/usr/local/sbin" $fish_user_paths
    set -g fish_user_paths "/usr/local/opt/gnu-getopt/bin" $fish_user_paths
end

if test -d "/usr/local/share/chruby"
    . /usr/local/share/chruby/chruby.fish
    . /usr/local/share/chruby/auto.fish
    chruby 2.5.1
end

if status is-login
    pyenv init - | source
    pyenv shell 3.7.3
end
