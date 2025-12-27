package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * Created by govind on 16/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleAsyncMasterDataItemDTO implements Serializable{

  private static final long serialVersionUID = -335679551390740877L;
  private List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValues;
  private List<MasterDataItemImageDTO> masterDataItemImages;

  public List<MasterDataItemAttributeValueDTO> getMasterDataItemAttributeValues() {
    return masterDataItemAttributeValues;
  }

  public void setMasterDataItemAttributeValues(
      List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValues) {
    this.masterDataItemAttributeValues = masterDataItemAttributeValues;
  }

  public List<MasterDataItemImageDTO> getMasterDataItemImages() {
    return masterDataItemImages;
  }

  public void setMasterDataItemImages(List<MasterDataItemImageDTO> masterDataItemImages) {
    this.masterDataItemImages = masterDataItemImages;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("masterDataItemAttributeValues", masterDataItemAttributeValues)
        .append("masterDataItemImages", masterDataItemImages).toString();
  }
}
