function compose_secure
	gpg --armor --encrypt --sign --recipient $argv --encrypt-to 29E6B009
end
