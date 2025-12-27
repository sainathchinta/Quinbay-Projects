package com.gdn.common.util;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public final class GdnDigestUtil {

  public static byte[] getDigestFromString(String algorithm, String charSetName, String text)
      throws NoSuchAlgorithmException, UnsupportedEncodingException {
    MessageDigest md = MessageDigest.getInstance(algorithm);
    md.update(text.getBytes(charSetName));
    return md.digest();
  }

  private GdnDigestUtil() {
    // Nothing To do here
  }
}
