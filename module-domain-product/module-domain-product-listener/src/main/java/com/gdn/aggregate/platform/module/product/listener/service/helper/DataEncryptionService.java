package com.gdn.aggregate.platform.module.product.listener.service.helper;

import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.DataUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.aggregate.platform.module.product.listener.properties.EncryptionProperties;

import lombok.SneakyThrows;

@Component("ProductDataEncryptionService")
public class DataEncryptionService extends DataUtil {

  private EncryptionProperties encryptionProperties;

  private Cipher cipher;

  private static final String SECRET_KEY_FACTORY_ALGORITHM = "PBKDF2WithHmacSHA256";
  private static final String SECRET_KEY_SPEC_ALGORITHM = "AES";
  private static final int ITERATION_COUNT = 65536;
  private static final int KEY_LENGTH = 256;

  @Autowired
  public DataEncryptionService(ObjectMapper objectMapper, TimeService timeService, EncryptionProperties encryptionProperties, Cipher cipher) {
    super(objectMapper,timeService);
    this.encryptionProperties = encryptionProperties;
    this.cipher = cipher;
    init();
  }

  @SneakyThrows
  public void init() {
    SecretKey secretKey = generateSecretKey(encryptionProperties.getPassword(), encryptionProperties.getSalt());
    cipher.init(Cipher.ENCRYPT_MODE, secretKey);
  }

  private static SecretKey generateSecretKey(String password, String salt) throws NoSuchAlgorithmException, InvalidKeySpecException {
    SecretKeyFactory factory = SecretKeyFactory.getInstance(SECRET_KEY_FACTORY_ALGORITHM);
    KeySpec spec = new PBEKeySpec(password.toCharArray(), salt.getBytes(StandardCharsets.UTF_8), ITERATION_COUNT, KEY_LENGTH);

    return new SecretKeySpec(factory.generateSecret(spec).getEncoded(), SECRET_KEY_SPEC_ALGORITHM);
  }

  @SneakyThrows
  public String encrypt(Object object) {
    String text = toJson(object);
    byte[] cipherText = cipher.doFinal(text.getBytes(StandardCharsets.UTF_8));

    return Base64.getEncoder().encodeToString(cipherText);
  }

}
