package com.gdn.mta.bulk.models;

import java.io.Serializable;
import java.util.List;

import com.gdn.x.mta.rest.web.request.BulkProcessMandatoryRequest;

public class BulkProcessPostImageV2Request extends BulkProcessMandatoryRequest implements
    Serializable {

  private static final long serialVersionUID = -1322335810783908094L;
  
  private String username;
  private String merchantCode;
  private String productCode;
  private String tmpDir;
  private List<BulkProcessProductImageV2Request> images;
  private List<BulkProcessPostItemImageV2Request> productItem;

  public BulkProcessPostImageV2Request() {
    super();
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public String getTmpDir() {
    return tmpDir;
  }

  public void setTmpDir(String tmpDir) {
    this.tmpDir = tmpDir;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public List<BulkProcessProductImageV2Request> getImages() {
    return images;
  }

  public void setImages(List<BulkProcessProductImageV2Request> images) {
    this.images = images;
  }

  public List<BulkProcessPostItemImageV2Request> getProductItem() {
    return productItem;
  }

  public void setProductItem(List<BulkProcessPostItemImageV2Request> productItem) {
    this.productItem = productItem;
  }
  
}
