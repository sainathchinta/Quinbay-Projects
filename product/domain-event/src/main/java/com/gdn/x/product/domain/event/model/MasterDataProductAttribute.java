package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataProductAttribute implements Serializable {

  private static final long serialVersionUID = 6208972733864449845L;

  private boolean isOwnedByProductItem;

  private MasterDataAttribute masterDataAttribute;

  private int sequence;

  private List<MasterDataProductAttributeValue> masterDataProductAttributeValues;

  public MasterDataProductAttribute() {

  }

  public MasterDataProductAttribute(boolean isOwnedByProductItem,
      MasterDataAttribute masterDataAttribute, int sequence,
      List<MasterDataProductAttributeValue> masterDataProductAttributeValues) {
    super();
    this.isOwnedByProductItem = isOwnedByProductItem;
    this.masterDataAttribute = masterDataAttribute;
    this.sequence = sequence;
    this.masterDataProductAttributeValues = masterDataProductAttributeValues;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public MasterDataAttribute getMasterDataAttribute() {
    return this.masterDataAttribute;
  }

  public List<MasterDataProductAttributeValue> getMasterDataProductAttributeValues() {
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

  public void setMasterDataAttribute(MasterDataAttribute masterDataAttribute) {
    this.masterDataAttribute = masterDataAttribute;
  }

  public void setMasterDataProductAttributeValues(
      List<MasterDataProductAttributeValue> masterDataProductAttributeValues) {
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
    return String
        .format(
            "MasterDataProductAttribute [isOwnedByProductItem=%s, masterDataAttribute=%s, sequence=%s, masterDataProductAttributeValues=%s, toString()=%s]",
            this.isOwnedByProductItem, this.masterDataAttribute, this.sequence,
            this.masterDataProductAttributeValues, super.toString());
  }

}
