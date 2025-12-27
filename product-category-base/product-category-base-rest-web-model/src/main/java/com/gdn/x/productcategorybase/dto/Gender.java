package com.gdn.x.productcategorybase.dto;

public enum Gender {
  MALE, FEMALE, UNISEX;

  public static boolean validGender(String name) {
    for (Gender gender : Gender.values()) {
      if (gender.name().equals(name)) {
        return true;
      }
    }
    return false;
  }
}
