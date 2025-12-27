package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;


/**
 * Created by govind on 21/03/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineMasterDataItemDTO implements Serializable{


  public PristineMasterDataItemDTO() {
  }

  private static final long serialVersionUID = 1L;
  private String skuCode;
  private String upcCode;
  private boolean isActivated;
  private boolean isViewable;
  private int dangerousLevel;
  private List<MasterDataItemImageDTO> masterDataItemImages = new ArrayList<MasterDataItemImageDTO>();
  private String productCode;

  public String getSkuCode() {
    return skuCode;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }

  public String getUpcCode() {
    return upcCode;
  }

  public void setUpcCode(String upcCode) {
    this.upcCode = upcCode;
  }

  public boolean isActivated() {
    return isActivated;
  }

  public void setActivated(boolean activated) {
    isActivated = activated;
  }

  public boolean isViewable() {
    return isViewable;
  }

  public void setViewable(boolean viewable) {
    isViewable = viewable;
  }

  public int getDangerousLevel() {
    return dangerousLevel;
  }

  public void setDangerousLevel(int dangerousLevel) {
    this.dangerousLevel = dangerousLevel;
  }

  public List<MasterDataItemImageDTO> getMasterDataItemImages() {
    return masterDataItemImages;
  }

  public void setMasterDataItemImages(List<MasterDataItemImageDTO> masterDataItemImages) {
    this.masterDataItemImages = masterDataItemImages;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override public String toString() {
    return new ToStringBuilder(this).append("skuCode", skuCode).append("upcCode", upcCode)
        .append("isActivated", isActivated).append("isViewable", isViewable)
        .append("dangerousLevel", dangerousLevel)
        .append("masterDataItemImages", masterDataItemImages).append("productCode", productCode)
        .toString();
  }
}
