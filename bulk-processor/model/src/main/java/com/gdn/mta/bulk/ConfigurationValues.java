package com.gdn.mta.bulk;

public enum ConfigurationValues {
  Pre_live("Pre-live"), Post_live("Post-live"), Neutral("Neutral");

  private String configValue;

  ConfigurationValues(String configValue) {
    this.configValue = configValue;
  }

  public String getConfigValue() {
    return configValue;
  }

  public static String getValueForMerchant(String value) {
    if (Pre_live.configValue.equalsIgnoreCase(value))
      return Pre_live.configValue;
    if (Post_live.configValue.equalsIgnoreCase(value))
      return Post_live.configValue;
    if (Neutral.configValue.equalsIgnoreCase(value))
      return Neutral.configValue;
    return null;
  }

  public static String getValueForCategory(String value) {
    if (Pre_live.configValue.equalsIgnoreCase(value))
      return Pre_live.configValue;
    if (Post_live.configValue.equalsIgnoreCase(value))
      return Post_live.configValue;
    return null;
  }
}
