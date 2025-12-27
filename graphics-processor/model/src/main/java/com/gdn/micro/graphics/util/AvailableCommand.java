package com.gdn.micro.graphics.util;

/**
 * Created by Yudhi K. Surtan on 11/28/2015.
 */
public enum AvailableCommand {
  CONVERT("convert"),
  IDENTIFY("identify");

  private String name;

  private AvailableCommand(String name) {
    this.name = name;
  }

  public String getName() {
    return this.name;
  }

  @Override
  public String toString() {
    return this.getName();
  }
}
