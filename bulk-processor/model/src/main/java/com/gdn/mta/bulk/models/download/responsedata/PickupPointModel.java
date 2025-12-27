package com.gdn.mta.bulk.models.download.responsedata;

import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by keshashah on 07/11/16.
 */
public class PickupPointModel {
  private String name;
  private String code;
  private boolean fbbActivated;

  public PickupPointModel(String name, String code) {
    this.name = name;
    this.code = code;
  }
  public PickupPointModel(String name, String code, boolean fbbActivated) {
    this.name = name;
    this.code = code;
    this.fbbActivated = fbbActivated;
  }

  public String getName() {
    return name;
  }

  public String getCode() {
    return code;
  }


  public boolean isFbbActivated() {
    return fbbActivated;
  }

  public void setFbbActivated(boolean fbbActivated) {
    this.fbbActivated = fbbActivated;
  }



  @Override
  public String toString() {
    return new ToStringBuilder(this).append("name", name).append("code", code).append("fbbActivated", fbbActivated).toString();
  }
}
