function compose_secure
	cat | gpg --armor --encrypt --recipient $argv
end
