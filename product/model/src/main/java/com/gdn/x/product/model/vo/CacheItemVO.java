package com.gdn.x.product.model.vo;

import java.io.Serializable;


public class CacheItemVO implements Serializable{

  private static final long serialVersionUID = 1L;

  private String itemCode;

  public CacheItemVO(){
  }

  public CacheItemVO(String itemCode) {
    this.itemCode = itemCode;
  }

  public String getItemCode() {
    return itemCode;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }
}
