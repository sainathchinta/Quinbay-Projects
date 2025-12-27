package com.gdn.x.mta.distributiontask.util;

import java.security.SecureRandom;

/**
 * Created by Alok on 9/21/16.
 */
public class CodeGeneratorUtil {
  private static final String randomString =
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  private static SecureRandom secureRandom = new SecureRandom();

  public static String randomString(int length) {
    StringBuilder sb = new StringBuilder(length);
    for (int i = 0; i < length; i++)
      sb.append(randomString.charAt(secureRandom.nextInt(randomString.length())));
    return sb.toString();
  }
}
