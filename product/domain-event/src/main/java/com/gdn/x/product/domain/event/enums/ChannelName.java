package com.gdn.x.product.domain.event.enums;

public enum ChannelName {
  DEFAULT("Default"),

  ANDROID("Android"),

  DESKTOP_WEB("Web Channel"),

  TABLET_WEB("Tablet Web"),

  MOBILE_WEB("Mobile Web"),

  IOS("iOS"),

  WINDOWS_APPS("Windows Apps");

  private String description;

  private ChannelName(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }

}
