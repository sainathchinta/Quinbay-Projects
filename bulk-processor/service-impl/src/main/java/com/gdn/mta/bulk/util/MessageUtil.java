package com.gdn.mta.bulk.util;

import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Created by Vishal on 25/01/17.
 */
public class MessageUtil {

  private MessageUtil() {
  }

  public static String getMessage(String key, String lang){
    ResourceBundle bundle = getResourceBundle(lang);
    return bundle.getString(key);
  }

  public static ResourceBundle getResourceBundle(String lang) {
    Locale locale = new Locale.Builder().setLanguage(lang).build();
    return ResourceBundle.getBundle("messages", locale);
  }
}
