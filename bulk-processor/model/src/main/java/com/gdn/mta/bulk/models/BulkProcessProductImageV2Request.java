package com.gdn.mta.bulk.models;

import java.io.Serializable;

public class BulkProcessProductImageV2Request implements Serializable {

  private static final long serialVersionUID = -7240943177831213216L;

  private boolean mainImage = false;
  private String imagePath;

  public BulkProcessProductImageV2Request() {
    super();
  }
  

  public BulkProcessProductImageV2Request(boolean mainImage, String imagePath) {
    super();
    this.mainImage = mainImage;
    this.imagePath = imagePath;
  }


  public String getImagePath() {
    return imagePath;
  }

  public void setImagePath(String imagePath) {
    this.imagePath = imagePath;
  }

  public boolean isMainImage() {
    return mainImage;
  }

  public void setMainImage(boolean mainImage) {
    this.mainImage = mainImage;
  }

}
