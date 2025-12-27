package com.gdn.partners.pcu.internal.enums;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum TracerFieldKey {
  CHANNEL_ID("channelId"), CLIENT_ID("clientId"), STORE_ID("storeId"), USER_NAME("username"), REQUEST_ID(
      "requestId"), VENDOR_CODE("vendorCode"), USER_TYPE("userType"), BUSINESSPARTNER_CODE("businessPartnerCode");

  private static final List<String> KEYS =
      Arrays.stream(TracerFieldKey.values()).map(TracerFieldKey::getKey).collect(Collectors.toList());

  private String key;

  TracerFieldKey(String key) {
    this.key = key;
  }

  public static List<String> getKeys() {
    return KEYS;
  }

  public String getKey() {
    return key;
  }
}