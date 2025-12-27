package com.gda.mta.product.dto;

import com.gdn.common.web.base.BaseResponse;

public class ProductLevel3DashboardResponse extends BaseResponse {

  private static final long serialVersionUID = -5074636278032032241L;
  private Long totalReady;
  private Long totalUnbuyable;
  private Long totalUndisplay;
  private Long totalProduct;

  public ProductLevel3DashboardResponse() {
    // do nothing
  }

  public ProductLevel3DashboardResponse(Long totalReady, Long totalUnbuyable, Long totalUndisplay) {
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
    return String
        .format(
            "ProductLevel3DashboardResponse [totalReady=%s, totalUnbuyable=%s, totalUndisplay=%s, totalProduct=%s, getTotalReady()=%s, getTotalUnbuyable()=%s, getTotalUndisplay()=%s, getTotalProduct()=%s]",
            totalReady, totalUnbuyable, totalUndisplay, totalProduct, getTotalReady(),
            getTotalUnbuyable(), getTotalUndisplay(),getTotalProduct());
  }

}
