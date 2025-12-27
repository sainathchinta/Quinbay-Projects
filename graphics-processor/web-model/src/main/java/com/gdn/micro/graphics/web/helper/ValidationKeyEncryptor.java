package com.gdn.micro.graphics.web.helper;

import java.nio.charset.StandardCharsets;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import jakarta.xml.bind.DatatypeConverter;

public class ValidationKeyEncryptor {

  private final SecretKeySpec secretKeySpec;

  public ValidationKeyEncryptor(String encryptionKey) throws Exception {
    // Security.addProvider(new BouncyCastleProvider());
    secretKeySpec = new SecretKeySpec(encryptionKey.getBytes(StandardCharsets.US_ASCII), "AES");
  }

  public String decrypt(String encryptionText) throws Exception {
    Cipher cipher = Cipher.getInstance("AES/ECB/NoPadding");
    cipher.init(Cipher.DECRYPT_MODE, secretKeySpec);
    byte[] decryptedBytes = cipher.doFinal(DatatypeConverter.parseHexBinary(encryptionText));
    return new String(decryptedBytes).trim();
  }

  public String encrypt(String value) throws Exception {
    Cipher cipher = Cipher.getInstance("AES/ECB/NoPadding");
    cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec);
    byte[] encrypted =
        cipher.doFinal(getStringMultiple16(value).getBytes(StandardCharsets.US_ASCII));
    return DatatypeConverter.printHexBinary(encrypted);
  }

  private String getStringMultiple16(String toBeEncrypt) throws Exception {
    if (toBeEncrypt.length() % 16 == 0) {
      return toBeEncrypt;
    } else {
      int mod = toBeEncrypt.length() % 16;
      int x = 16 - mod;
      for (int i = 0; i < x; i++) {
        toBeEncrypt = toBeEncrypt + " ";
      }
      return toBeEncrypt;
    }
  }
}
