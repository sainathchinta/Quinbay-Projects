package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Level1Level2ListInventoryResponseRestWeb extends BaseResponse implements Serializable {
  public static class Builder implements GdnBaseBuilder<Level1Level2ListInventoryResponseRestWeb> {
    private boolean success;
    private List<Level1InventoryResponseRestWeb> level1Inventory;
    private List<Level2InventoryResponseRestWeb> level2Inventory;

    public Builder() {}

    @Override
    public Level1Level2ListInventoryResponseRestWeb build() {
      return new Level1Level2ListInventoryResponseRestWeb(this);
    }

    public Level1Level2ListInventoryResponseRestWeb.Builder setLevel1Inventory(
        List<Level1InventoryResponseRestWeb> level1Inventory) {
      this.level1Inventory = level1Inventory;
      return this;
    }

    public Level1Level2ListInventoryResponseRestWeb.Builder setLevel2Inventory(
        List<Level2InventoryResponseRestWeb> level2Inventory) {
      this.level2Inventory = level2Inventory;
      return this;
    }

    public Level1Level2ListInventoryResponseRestWeb.Builder setSuccess(boolean success) {
      this.success = success;
      return this;
    }

    @Override
    public String toString() {
      return String.format(
          "Builder [success=%s, level1Inventory=%s, level2Inventory=%s, toString()=%s]",
          new Object[] {Boolean.valueOf(this.success), this.level1Inventory, this.level2Inventory,
              super.toString()});
    }
  }

  private static final long serialVersionUID = 1L;
  private boolean success;
  private List<Level1InventoryResponseRestWeb> level1Inventory;

  private List<Level2InventoryResponseRestWeb> level2Inventory;

  public Level1Level2ListInventoryResponseRestWeb() {}

  public Level1Level2ListInventoryResponseRestWeb(
      Level1Level2ListInventoryResponseRestWeb.Builder builder) {
    this.success = builder.success;
    this.level1Inventory = builder.level1Inventory;
    this.level2Inventory = builder.level2Inventory;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }


  public List<Level1InventoryResponseRestWeb> getLevel1Inventory() {
    return this.level1Inventory;
  }

  public List<Level2InventoryResponseRestWeb> getLevel2Inventory() {
    return this.level2Inventory;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isSuccess() {
    return this.success;
  }

  public void setLevel1Inventory(List<Level1InventoryResponseRestWeb> level1Inventory) {
    this.level1Inventory = level1Inventory;
  }

  public void setLevel2Inventory(List<Level2InventoryResponseRestWeb> level2Inventory) {
    this.level2Inventory = level2Inventory;
  }

  public void setSuccess(boolean success) {
    this.success = success;
  }

  @Override
  public String toString() {
    return String
        .format(
            "Level1Level2ListInventoryResponseRestWeb [success=%s, level1Inventory=%s, level2Inventory=%s, toString()=%s]",
            new Object[] {Boolean.valueOf(this.success), this.level1Inventory,
                this.level2Inventory, super.toString()});
  }
}
