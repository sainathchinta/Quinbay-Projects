package com.gda.mta.product.dto;

import com.gdn.common.web.base.BaseResponse;

public class ProductLevel3StockInfoWarehouseResponse extends BaseResponse {
  private static final long serialVersionUID = -1573827967951917886L;

  private String warehouseMerchantCode;
  private String warehouseItemSku;
  private int availableStock;
  private int originalStock;
  private int reservedStock;
  private int reservedStockOrderPending;
  private int reservedStockOrderOnlineInFulfillment;
  private int reservedStockOrderOfflineInFulfillment;

  public String getWarehouseMerchantCode() {
    return warehouseMerchantCode;
  }

  public void setWarehouseMerchantCode(String warehouseMerchantCode) {
    this.warehouseMerchantCode = warehouseMerchantCode;
  }

  public String getWarehouseItemSku() {
    return warehouseItemSku;
  }

  public void setWarehouseItemSku(String warehouseItemSku) {
    this.warehouseItemSku = warehouseItemSku;
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

  public int getReservedStockOrderOnlineInFulfillment() {
    return reservedStockOrderOnlineInFulfillment;
  }

  public void setReservedStockOrderOnlineInFulfillment(int reservedStockOrderOnlineInFulfillment) {
    this.reservedStockOrderOnlineInFulfillment = reservedStockOrderOnlineInFulfillment;
  }

  public int getReservedStockOrderOfflineInFulfillment() {
    return reservedStockOrderOfflineInFulfillment;
  }

  public void setReservedStockOrderOfflineInFulfillment(
      int reservedStockOrderOfflineInFulfillment) {
    this.reservedStockOrderOfflineInFulfillment = reservedStockOrderOfflineInFulfillment;
  }

  @Override
  public String toString() {
    return "ProductLevel3StockInfoWarehouseResponse [warehouseMerchantCode=" + warehouseMerchantCode
        + ", warehouseItemSku=" + warehouseItemSku + ", availableStock=" + availableStock
        + ", originalStock=" + originalStock + ", reservedStock=" + reservedStock
        + ", reservedStockOrderPending=" + reservedStockOrderPending
        + ", reservedStockOrderOnlineInFulfillment=" + reservedStockOrderOnlineInFulfillment
        + ", reservedStockOrderOfflineInFulfillment=" + reservedStockOrderOfflineInFulfillment
        + "]";
  }
}
