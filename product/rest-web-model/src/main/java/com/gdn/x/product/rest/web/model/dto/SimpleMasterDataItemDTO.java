package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * Created by govind on 16/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleMasterDataItemDTO implements Serializable{

  private static final long serialVersionUID = -5958007831610306069L;
  private String productCode;
  private int dangerousLevel;
  private List<MasterDataItemImageDTO> masterDataItemImages;
  private List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValues;

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public List<MasterDataItemImageDTO> getMasterDataItemImages() {
    return masterDataItemImages;
  }

  public void setMasterDataItemImages(List<MasterDataItemImageDTO> masterDataItemImages) {
    this.masterDataItemImages = masterDataItemImages;
  }

  public List<MasterDataItemAttributeValueDTO> getMasterDataItemAttributeValues() {
    return masterDataItemAttributeValues;
  }

  public void setMasterDataItemAttributeValues(
      List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValues) {
    this.masterDataItemAttributeValues = masterDataItemAttributeValues;
  }

  public int getDangerousLevel() {
    return dangerousLevel;
  }

  public void setDangerousLevel(int dangerousLevel) {
    this.dangerousLevel = dangerousLevel;
  }

  @Override
  public String toString() {
    return "SimpleMasterDataItemDTO{" + "productCode='" + productCode + '\'' + ", dangerousLevel="
        + dangerousLevel + ", masterDataItemImages=" + masterDataItemImages
        + ", masterDataItemAttributeValues=" + masterDataItemAttributeValues + '}';
  }
}
