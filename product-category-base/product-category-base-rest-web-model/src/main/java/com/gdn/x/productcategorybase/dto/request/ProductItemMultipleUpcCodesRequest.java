package com.gdn.x.productcategorybase.dto.request;

import java.util.List;

import com.gdn.x.productcategorybase.dto.BaseDTORequest;

public class ProductItemMultipleUpcCodesRequest extends BaseDTORequest {

  private static final long serialVersionUID = 4702279990925736382L;

  private List<String> upcCodes;

  private String skuCode;

  public ProductItemMultipleUpcCodesRequest() {}

  public String getSkuCode() {
    return this.skuCode;
  }

  public List<String> getUpcCodes() {
    return this.upcCodes;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }

  public void setUpcCodes(List<String> upcCodes) {
    this.upcCodes = upcCodes;
  }

  @Override
  public String toString() {
    return String.format("ProductItemMultipleUpcCodesRequest [upcCodes=%s, skuCode=%s, toString()=%s]", this.upcCodes,
        this.skuCode, super.toString());
  }
}
