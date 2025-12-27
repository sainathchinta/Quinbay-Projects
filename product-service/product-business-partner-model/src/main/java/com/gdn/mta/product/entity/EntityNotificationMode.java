package com.gdn.mta.product.entity;

/**
 * Created by riteshkumar on 27/03/17.
 */
public enum EntityNotificationMode {

  SMS("sms", 1),
  EMAIL("email", 2);
  private String name;
  private int value;

  EntityNotificationMode(String name, int value) {
    this.name = name;
    this.value = value;
  }

  public String getName() {
    return name;
  }

  public int getValue() {
    return value;
  }
}
