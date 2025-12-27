package com.gdn.x.product.domain.event.enums;

public enum ItemPickupPointChangeEventType {
  PRICE_CHANGE("priceChange"),
  DISCOVERABLE_FLAG_CHANGE("discoverableFlagChange"),
  INSURED_AMOUNT_CHANGE("insuredAmountChange"),
  CNC_DISCOVERABLE_FLAG_CHANGE("cncDiscoverableFlagChange"),
  TAKE_DOWN("takeDown"),
  FBB_MIGRATION("fbbMigration"),
  VARIANT_CHANGE("variantChange"),
  B2B_FIELD_CHANGE("b2bFieldChange"),
  BUYABLE_SCHEDULE_CHANGE("buyableScheduleChange"),
  DISCOVERABLE_SCHEDULE_CHANGE("discoverableScheduleChange"),
  SOURCE_SUSPENSION_FLOW("suspendProduct"),
  SOURCE_PRODUCT_ARCHIVAL("archiveProduct"),
  SOURCE_PRODUCT_REJECTION("rejectProduct");

  String name;

  ItemPickupPointChangeEventType(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public static ItemPickupPointChangeEventType findEnumByValue(String value) {
    for (ItemPickupPointChangeEventType enumValue : ItemPickupPointChangeEventType.values()) {
      if (enumValue.getName().equals(value)) {
        return enumValue;
      }
    }
    return null;
  }
}
