package com.gdn.x.productcategorybase.csv;

import org.supercsv.cellprocessor.constraint.NotNull;
import org.supercsv.cellprocessor.ift.CellProcessor;

public class ProductItemCsv {

  public static final String[] INPUT_HEADER =
      new String[] {"storeId", "productId", "id", "generatedItemName", "skuCode", "upcCode"};
  public static final CellProcessor[] INPUT_PROCESSORS =
      new CellProcessor[] {new NotNull(), new NotNull(), new NotNull(), new NotNull(), new NotNull(), new NotNull()};

  private String productId;
  private String generatedItemName;
  private String upcCode;
  private String skuCode;
  private String id;
  private String storeId;
  private byte[] hash;

  public ProductItemCsv() {
    // nothing to do here
  }


  public String getGeneratedItemName() {
    return this.generatedItemName;
  }


  public byte[] getHash() {
    return this.hash;
  }


  public String getId() {
    return this.id;
  }


  public String getProductId() {
    return this.productId;
  }


  public String getSkuCode() {
    return this.skuCode;
  }


  public String getStoreId() {
    return this.storeId;
  }


  public String getUpcCode() {
    return this.upcCode;
  }


  public void setGeneratedItemName(String generatedItemName) {
    this.generatedItemName = generatedItemName;
  }


  public void setHash(byte[] hash) {
    this.hash = hash;
  }


  public void setId(String id) {
    this.id = id;
  }


  public void setProductId(String productId) {
    this.productId = productId;
  }


  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }


  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }


  public void setUpcCode(String upcCode) {
    this.upcCode = upcCode;
  }


  @Override
  public String toString() {
    return String.format("ProductItem [upcCode=%s, skuCode=%s, generatedItemName=%s]", this.upcCode, this.skuCode,
        this.generatedItemName);
  }

}
