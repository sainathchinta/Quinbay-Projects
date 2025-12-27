package com.gdn.mta.product.entity;

import java.io.Serializable;

public class ProductLevel3Dashboard implements Serializable {

  private static final long serialVersionUID = -257532545150304607L;
  private Long totalReady;
  private Long totalUnbuyable;
  private Long totalUndisplay;
  private Long totalProduct;

  public ProductLevel3Dashboard() {
    // do nothing
  }

  public ProductLevel3Dashboard(Long totalReady, Long totalUnbuyable, Long totalUndisplay) {
    super();
    this.totalReady = totalReady;
    this.totalUnbuyable = totalUnbuyable;
    this.totalUndisplay = totalUndisplay;
  }


  public Long getTotalReady() {
    return totalReady;
  }

  public void setTotalReady(Long totalReady) {
    this.totalReady = totalReady;
  }

  public Long getTotalUnbuyable() {
    return totalUnbuyable;
  }

  public void setTotalUnbuyable(Long totalUnbuyable) {
    this.totalUnbuyable = totalUnbuyable;
  }

  public Long getTotalUndisplay() {
    return totalUndisplay;
  }

  public void setTotalUndisplay(Long totalUndisplay) {
    this.totalUndisplay = totalUndisplay;
  }

  public Long getTotalProduct() {
    return totalProduct;
  }

  public void setTotalProduct(Long totalProduct) {
    this.totalProduct = totalProduct;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3Dashboard [totalReady=").append(totalReady)
        .append(", totalUnbuyable=").append(totalUnbuyable).append(", totalUndisplay=")
        .append(totalUndisplay).append(", totalProduct=").append(totalProduct)
        .append(", getTotalReady()=").append(getTotalReady()).append(", getTotalUnbuyable()=")
        .append(getTotalUnbuyable()).append(", getTotalUndisplay()=").append(getTotalUndisplay())
        .append(", getTotalProduct()=").append(getTotalProduct()).append("]");
    return builder.toString();
  }

}
