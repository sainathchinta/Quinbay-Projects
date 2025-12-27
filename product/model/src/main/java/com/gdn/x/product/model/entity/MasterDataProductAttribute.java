package com.gdn.x.product.model.entity;

import java.util.List;



import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.DBRef;
import org.springframework.data.mongodb.core.mapping.Field;

public class MasterDataProductAttribute implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.IS_OWNED_BY_PRODUCT_ITEM)
  private boolean isOwnedByProductItem;

  @DBRef
  private MasterDataAttribute masterDataAttribute;

  @Field(value = ProductFieldNames.SEQUENCE)
  private Integer sequence;

  @Field(value = ProductFieldNames.MASTER_DATA_PRODUCT_ATTRIBUTE_VALUES)
  private List<MasterDataProductAttributeValue> masterDataProductAttributeValues;

  @Transient
  private boolean markForDelete;

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

  public Integer getSequence() {
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

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setMarkForDelete(boolean markForDelete){this.markForDelete = markForDelete;}

  public boolean getMarkForDelete() {
    return markForDelete;
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
