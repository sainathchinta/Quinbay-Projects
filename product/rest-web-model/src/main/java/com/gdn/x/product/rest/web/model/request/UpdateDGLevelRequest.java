package com.gdn.x.product.rest.web.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateDGLevelRequest {

  private String itemSku;

  private int dangerousLevel;

  public UpdateDGLevelRequest() {}

  public UpdateDGLevelRequest(String itemSku, int dangerousLevel) {
    this.itemSku = itemSku;
    this.dangerousLevel = dangerousLevel;
  }

  public int getDangerousLevel() {
    return dangerousLevel;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setDangerousLevel(int dangerousLevel) {
    this.dangerousLevel = dangerousLevel;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("UpdateDGLevelRequest [itemSku=").append(itemSku).append(", dangerousLevel=")
        .append(dangerousLevel).append("]");
    return builder.toString();
  }

}
