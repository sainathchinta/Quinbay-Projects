package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3SummaryMinifiedRequest implements Serializable{
  private static final long serialVersionUID = -8420680463143871764L;

  /**
   * @deprecated {@link use inventoryFilter instead}
   */
  @Deprecated
  private Integer stock;
  private Boolean display;
  private Boolean buyable;
  private Boolean archived;
  private ProductLevel3InventoryCriteria inventoryFilter;
  
  public ProductLevel3SummaryMinifiedRequest(){}

  public Integer getStock() {
    return stock;
  }

  public void setStock(Integer stock) {
    this.stock = stock;
  }

  public Boolean getDisplay() {
    return display;
  }

  public void setDisplay(Boolean display) {
    this.display = display;
  }

  public Boolean getBuyable() {
    return buyable;
  }

  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public Boolean getArchived() {
    return archived;
  }

  public void setArchived(Boolean archived) {
    this.archived = archived;
  }

  public ProductLevel3InventoryCriteria getInventoryFilter() {
    return inventoryFilter;
  }

  public void setInventoryFilter(ProductLevel3InventoryCriteria inventoryFilter) {
    this.inventoryFilter = inventoryFilter;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductLevel3SummaryMinifiedRequest{");
    sb.append("stock=").append(stock);
    sb.append(", display=").append(display);
    sb.append(", buyable=").append(buyable);
    sb.append(", archived=").append(archived);
    sb.append(", inventoryFilter=").append(inventoryFilter);
    sb.append('}');
    return sb.toString();
  }
}
