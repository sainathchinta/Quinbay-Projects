package com.gdn.mta.product.commons.constant;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

public final class ConstantValues {
  private ConstantValues() {}
  public static final Map<Boolean, String> BOOL_STRING_VALUES = ImmutableMap.<Boolean, String>builder().put(true, "Ya")
      .put(false, "Tidak").build();
}
