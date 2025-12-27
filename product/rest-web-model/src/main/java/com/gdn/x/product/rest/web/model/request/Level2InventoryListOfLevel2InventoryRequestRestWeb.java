package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;
import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Level2InventoryListOfLevel2InventoryRequestRestWeb implements Serializable {

  private static final long serialVersionUID = 1L;
  private List<Level2InventoryRequestRestWeb> listOfLevel2InventoryRequest;

  public Level2InventoryListOfLevel2InventoryRequestRestWeb() {
    // Do nothing
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public List<Level2InventoryRequestRestWeb> getListOfLevel2InventoryRequest() {
    return listOfLevel2InventoryRequest;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setListOfLevel2InventoryRequest(
      List<Level2InventoryRequestRestWeb> listOfLevel2InventoryRequest) {
    this.listOfLevel2InventoryRequest = listOfLevel2InventoryRequest;
  }

  @Override
  public String toString() {
    return String
        .format(
            "Level2InventoryListOfLevel2InventoryRequestRestWeb [listOfLevel2InventoryRequest=%s, toString()=%s]",
            listOfLevel2InventoryRequest, super.toString());
  }

}
