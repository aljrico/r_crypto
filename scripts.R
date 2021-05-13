
# hmac --------------------------------------------------------------------
digest::hmac(key = "polar bear", object = "Not Penny's boat.", algo = "sha256")
openssl::sha256(x = "Not Penny's boat.", key = "polar bear")

# Encrypted message -------------------------------------------------------
key <- openssl::sha256(charToRaw("Our shared secret"))
msg <- digest::makeRaw("A very secret message")

encrypted_message <- openssl::aes_cbc_encrypt(msg, key)
rawToChar(encrypted_message)

decrypted_message <- openssl::aes_cbc_decrypt(encrypted_message, key)
rawToChar(decrypted_message)
