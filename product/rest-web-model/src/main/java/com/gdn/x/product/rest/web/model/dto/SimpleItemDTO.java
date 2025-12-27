package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;

/**
 * Created by govind on 14/09/2017 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleItemDTO implements Serializable{


  private static final long serialVersionUID = 355944134096957062L;

  private String itemsSkus;
  private String pristineId;
  private boolean isDiscoverable;
  private boolean isSynchronized;


  public String getItemsSkus() {
    return itemsSkus;
  }

  public void setItemsSkus(String itemsSkus) {
    this.itemsSkus = itemsSkus;
  }

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public boolean isDiscoverable() {
    return isDiscoverable;
  }

  public void setDiscoverable(boolean discoverable) {
    isDiscoverable = discoverable;
  }

  public boolean isSynchronized() {
    return isSynchronized;
  }

  public void setSynchronized(boolean aSynchronized) {
    isSynchronized = aSynchronized;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("itemsSkus", itemsSkus)
        .append("pristineId", pristineId)
        .append("isDiscoverable", isDiscoverable)
        .append("isSynchronized", isSynchronized).toString();
  }
}
