package com.gdn.mta.product.entity;

/**
 * Created by riteshkumar on 27/03/17.
 */
public enum EntityNotificationType {
  ORDER_NEW("orderNew", 0),
  ORDER_SENT("orderSent", 1),
  ORDER_CANCELED("orderCanceled", 2),
  PRODUCT_LIVE("productLive", 3),
  PRODUCT_REJECT_BY_TEAM_QC("productRejectByTeamQC", 4),
  PRODUCT_OUT_OF_STOCK("productOutOfStock", 5),
  PRODUCT_AUTO_REJECT("productAutoReject",6);

  private String name;
  private int bytePosition;

  EntityNotificationType(String name, int bytePosition) {
    this.name = name;
    this.bytePosition = bytePosition;
  }

  public String getName() {
    return name;
  }

  public int getBytePosition() {
    return bytePosition;
  }
}
