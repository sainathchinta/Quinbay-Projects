package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataProductAttributeDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private boolean isOwnedByProductItem;
  private MasterDataAttributeDTO masterDataAttribute;
  private int sequence;
  private List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValues;

  public MasterDataProductAttributeDTO() {

  }

  public MasterDataProductAttributeDTO(boolean isOwnedByProductItem,
      MasterDataAttributeDTO masterDataAttribute, int sequence,
      List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValues) {
    this.isOwnedByProductItem = isOwnedByProductItem;
    this.masterDataAttribute = masterDataAttribute;
    this.sequence = sequence;
    this.masterDataProductAttributeValues = masterDataProductAttributeValues;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public MasterDataAttributeDTO getMasterDataAttribute() {
    return this.masterDataAttribute;
  }

  public List<MasterDataProductAttributeValueDTO> getMasterDataProductAttributeValues() {
    return this.masterDataProductAttributeValues;
  }

  public int getSequence() {
    return this.sequence;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isOwnedByProductItem() {
    return this.isOwnedByProductItem;
  }

  public void setMasterDataAttribute(MasterDataAttributeDTO masterDataAttribute) {
    this.masterDataAttribute = masterDataAttribute;
  }

  public void setMasterDataProductAttributeValues(
      List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValues) {
    this.masterDataProductAttributeValues = masterDataProductAttributeValues;
  }

  public void setOwnedByProductItem(boolean isOwnedByProductItem) {
    this.isOwnedByProductItem = isOwnedByProductItem;
  }

  public void setSequence(int sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String.format(
        "MasterDataProductAttributeDTO [isOwnedByProductItem=%s, masterDataAttribute=%s, sequence=%s, masterDataProductAttributeValues=%s, toString()=%s]",
        isOwnedByProductItem, masterDataAttribute, sequence, masterDataProductAttributeValues,
        super.toString());
  }

}
