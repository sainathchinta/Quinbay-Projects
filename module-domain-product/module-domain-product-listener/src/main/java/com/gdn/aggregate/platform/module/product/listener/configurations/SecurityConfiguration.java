package com.gdn.aggregate.platform.module.product.listener.configurations;

import java.security.GeneralSecurityException;

import javax.crypto.Cipher;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SecurityConfiguration {

  private static final String CIPHER_ALGORITHM = "AES";

  @Bean
  public Cipher encryptCipher() throws GeneralSecurityException {
    return Cipher.getInstance(CIPHER_ALGORITHM);
  }

}
