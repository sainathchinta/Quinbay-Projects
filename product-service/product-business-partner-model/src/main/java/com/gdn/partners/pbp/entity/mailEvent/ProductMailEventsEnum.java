package com.gdn.partners.pbp.entity.mailEvent;

public enum ProductMailEventsEnum {
  APPROVED("product-mail-approved"),
  POST_LIVE_REVIEW_APPROVED("post-live-product-mail-approved"),
  REJECTED("product-mail-rejected"),
  POST_LIVE_REVIEW_REJECTED("post-live-product-mail-rejected"),
  CATEGORY_CHANGE("product-mail-categoryChange"),
  SENT_FOR_CORRECTION("product-mail-sendForCorrection"),
  AUTO_ARCHIVED("item-skus-auto-archived"),
  SUSPENDED("product-mail-suspension"),
  RE_ACTIVATED("product-mail-re_activated");

  private final String notificationType;

  ProductMailEventsEnum(String notificationType){
    this.notificationType = notificationType;
  }

  public String getNotificationType(){
    return notificationType;
  }
}
