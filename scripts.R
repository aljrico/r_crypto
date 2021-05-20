
# hmac --------------------------------------------------------------------
digest::hmac(key = "polar bear", object = "Not Penny's boat.", algo = "sha256")
openssl::sha256(x = "Not Penny's boat.", key = "polar bear")

# Encrypted message -------------------------------------------------------
key <- openssl::sha256(charToRaw("4815162342"))
msg <- digest::makeRaw("Not Penny's boat!")

encrypted_message <- openssl::aes_cbc_encrypt(msg, key)
rawToChar(encrypted_message)

decrypted_message <- openssl::aes_cbc_decrypt(encrypted_message, key)
rawToChar(decrypted_message)


# RSA ---------------------------------------------------------------------
key <- openssl::rsa_keygen()
msg <- digest::makeRaw("Not Penny's boat!")

encrypted_message <- openssl::rsa_encrypt(digest::makeRaw("Not Penny's boat!"), key$pubkey)
rawToChar(encrypted_message[!encrypted_message=='00'])

decrypted_message <- openssl::rsa_decrypt(encrypted_message, key)
rawToChar(decrypted_message)


# Verify signature -----------------------------------------------------
process_data <- function(data){
  data %>% 
    jsonlite::toJSON() %>% 
    as.character() %>% 
    digest::makeRaw()
}
create_signature <- function(data, key){
  
  msg <- process_data(data)

  raw_signature <- openssl::signature_create(msg, hash = openssl::sha256, key = key)
  raw_signature %>% 
    as.character() %>% 
    paste(collapse = ",") %>% 
    RCurl::base64Encode() %>% 
    as.character()
}
verify_signature <- function(data, sig, pubkey){
  
  msg <- process_data(data)
  
  raw_signature <- 
    sig %>% 
    RCurl::base64Decode() %>% 
    stringr::str_split(',') %>% 
    unlist() %>% 
    as.hexmode() %>% 
    as.raw()
  
  openssl::signature_verify(data = msg, sig = raw_signature, hash = openssl::sha256, pubkey = pubkey)
}

key <- openssl::rsa_keygen(512)
data <- list(index = 1, timestamp = Sys.time(), message = "And now it begins", previous_signature = "")
sig <- create_signature(data, key)
print(sig)
verify_signature(data, sig, key$pubkey)




signature <- openssl::signature_create(msg, key = key, hash = openssl::sha256)
RCurl::base64Encode(rawToChar(signature[!signature=='00']))

encrypted_message <- openssl::rsa_encrypt(digest::makeRaw("Not Penny's boat!"), key = key$pubkey)
rawToChar(encrypted_message[!encrypted_message=='00'])

decrypted_message <- openssl::rsa_decrypt(encrypted_message, key = key$pubkey)
rawToChar(decrypted_message)


# Proof of Work -----------------------------------------------------------


number_digits <- 5
for(i in 1:1e9){
  hash <- digest::digest(i, algo = "sha256")
  left_digits <- stringr::str_split(hash, '')[[1]][1:number_digits]
  expected <- rep("0", number_digits)
  if(identical(left_digits, expected)){
    print(i)
    print(hash)
    break
  }
}


is_prime <- function(n) n == 2L || all(n %% 2L:ceiling(sqrt(n)) != 0)

next_prime <- function(n){
  
  while(!is_prime(n)) n <- n + 1
  return(n)
}

next_prime(1e9)

for(i in 1:1e3){
  if(is_prime(i)) print(i)
}
