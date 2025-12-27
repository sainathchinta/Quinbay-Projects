package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;

public class ItemSkuListRequest implements Serializable {
  private static final long serialVersionUID = 7141802466870893512L;
  List<String> itemSkuList;
  
  public List<String> getItemSkuList() {
    return itemSkuList;
  }
  public void setItemSkuList(List<String> itemSkuList) {
    this.itemSkuList = itemSkuList;
  }
}
