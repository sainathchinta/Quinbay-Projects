package com.gdn.mta.bulk.models;

import java.io.Serializable;
import java.util.List;

public class BulkProcessPostItemImageV2Request implements
    Serializable {

  private static final long serialVersionUID = -1322335810783908094L;

  private String productItemCode;
  private List<BulkProcessProductImageV2Request> images;

  public BulkProcessPostItemImageV2Request() {
    super();
  }

  public BulkProcessPostItemImageV2Request(String productItemCode,
      List<BulkProcessProductImageV2Request> images) {
    super();
    this.productItemCode = productItemCode;
    this.images = images;
  }

  public String getProductItemCode() {
    return productItemCode;
  }

  public void setProductItemCode(String productItemCode) {
    this.productItemCode = productItemCode;
  }

  public List<BulkProcessProductImageV2Request> getImages() {
    return images;
  }

  public void setImages(List<BulkProcessProductImageV2Request> images) {
    this.images = images;
  }

}
