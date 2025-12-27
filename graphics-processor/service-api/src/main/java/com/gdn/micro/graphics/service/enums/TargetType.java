package com.gdn.micro.graphics.service.enums;

public enum  TargetType {
  BPG("bpg"),
  JPG("jpg"),
  WEBP("webp");

  private final String name;

  private TargetType(String s) {
    name = s;
  }

  public boolean equalsName(String otherName) {
    return (otherName == null) ? false : name.equals(otherName);
  }

  @Override
  public String toString() {
    return this.name;
  }
}
