package com.gdn.mta.product.util.validator;

import com.gda.mta.product.dto.RestrictedKeywordsByField;

/**
 * Created by Alok on 6/30/16.
 */
public class EmailValidator {
  public static void getEmailAddressFromProductFields(String email, RestrictedKeywordsByField restrictedKeywordsByField) {
    String ePattern =
        "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$";
    java.util.regex.Pattern p = java.util.regex.Pattern.compile(ePattern);
    java.util.regex.Matcher m = p.matcher(email);
    while (m.find()) {
      String keyword = m.group();
      if (!restrictedKeywordsByField.getKeywords().contains(keyword)) {
        restrictedKeywordsByField.getKeywords().add(keyword);
      }
    }
  }
}
