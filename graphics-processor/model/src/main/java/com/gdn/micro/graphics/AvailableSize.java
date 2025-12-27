package com.gdn.micro.graphics;

/**
 * Created by Yudhi K. Surtan on 11/29/2015.
 */
public enum AvailableSize {
  FULL("full"),
  MEDIUM("medium"),
  THUMBNAIL("thumbnail"),
  ALL("ALL");

  private String name;

  private AvailableSize(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }
}
