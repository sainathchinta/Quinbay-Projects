package com.gdn.mta.product.enums;

public enum FbbMigrationConstants {

  BACK_FILL_FBB_FLAG("BACK_FILL_FBB_FLAG"),

  INACTIVE("INACTIVE"),

  ACTIVE("ACTIVE"), PICKUP_POINT_CHANGE("PICKUP_POINT_CHANGE");

  private final String migrationConstants;

  FbbMigrationConstants(String migrationConstants) {
    this.migrationConstants = migrationConstants;
  }

  public String getMigrationConstants() {
    return migrationConstants;
  }
}
