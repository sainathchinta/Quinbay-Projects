package com.gda.mta.product.dto;

import com.gdn.common.web.base.BaseResponse;

public class ProductLevel3StockInfoWebSiteResponse extends BaseResponse {
  private static final long serialVersionUID = -204158163463331235L;

  private String webMerchantCode;
  private String webItemSku;
  private int availableStock;
  private int originalStock;
  private int reservedStock;
  private int reservedStockOrderPending;
  private int reservedStockInFulfillment;

  public String getWebMerchantCode() {
    return webMerchantCode;
  }

  public void setWebMerchantCode(String webMerchantCode) {
    this.webMerchantCode = webMerchantCode;
  }

  public String getWebItemSku() {
    return webItemSku;
  }

  public void setWebItemSku(String webItemSku) {
    this.webItemSku = webItemSku;
  }

  public int getAvailableStock() {
    return availableStock;
  }

  public void setAvailableStock(int availableStock) {
    this.availableStock = availableStock;
  }

  public int getOriginalStock() {
    return originalStock;
  }

  public void setOriginalStock(int originalStock) {
    this.originalStock = originalStock;
  }

  public int getReservedStock() {
    return reservedStock;
  }

  public void setReservedStock(int reservedStock) {
    this.reservedStock = reservedStock;
  }

  public int getReservedStockOrderPending() {
    return reservedStockOrderPending;
  }

  public void setReservedStockOrderPending(int reservedStockOrderPending) {
    this.reservedStockOrderPending = reservedStockOrderPending;
  }

  public int getReservedStockInFulfillment() {
    return reservedStockInFulfillment;
  }

  public void setReservedStockInFulfillment(int reservedStockInFulfillment) {
    this.reservedStockInFulfillment = reservedStockInFulfillment;
  }

  @Override
  public String toString() {
    return "ProductLevel3StockInfoWebSiteResponse [webMerchantCode=" + webMerchantCode
        + ", webItemSku=" + webItemSku + ", availableStock=" + availableStock + ", originalStock="
        + originalStock + ", reservedStock=" + reservedStock + ", reservedStockOrderPending="
        + reservedStockOrderPending + ", reservedStockInFulfillment=" + reservedStockInFulfillment
        + "]";
  }
}
