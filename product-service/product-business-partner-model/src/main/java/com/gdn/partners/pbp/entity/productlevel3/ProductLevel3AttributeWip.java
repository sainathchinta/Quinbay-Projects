package com.gdn.partners.pbp.entity.productlevel3;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import com.gdn.GdnBaseEntity;

@Entity
@Table(name = ProductLevel3AttributeWip.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    GdnBaseEntity.STORE_ID, ProductLevel3AttributeWip.COLUMN_PRODUCT_LEVEL3_WIP_ID,
    ProductLevel3AttributeWip.COLUMN_ATTRIBUTE_ID})})
public class ProductLevel3AttributeWip extends GdnBaseEntity {

  private static final long serialVersionUID = 1600385117394686924L;
  public static final String TABLE_NAME = "PRD_PRODUCT_BUSINESS_PARTNER_ATTRIBUTE";
  public static final String COLUMN_PRODUCT_LEVEL3_WIP_ID = "PRODUCT_BUSINESS_PARTNER_ID";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_VALUE = "VALUE";

  @ManyToOne
  @JoinColumn(name = ProductLevel3AttributeWip.COLUMN_PRODUCT_LEVEL3_WIP_ID)
  private ProductLevel3Wip productLevel3Wip;

  @Column(name = ProductLevel3AttributeWip.COLUMN_ATTRIBUTE_ID)
  private String attributeId;

  @Column(name = ProductLevel3AttributeWip.COLUMN_VALUE)
  private String value;

  public ProductLevel3AttributeWip() {}

  public ProductLevel3AttributeWip(ProductLevel3Wip productLevel3Wip, String attributeId, String value) {
    super();
    this.productLevel3Wip = productLevel3Wip;
    this.attributeId = attributeId;
    this.value = value;
  }

  public ProductLevel3Wip getProductLevel3Wip() {
    return productLevel3Wip;
  }

  public void setProductLevel3Wip(ProductLevel3Wip productLevel3Wip) {
    this.productLevel3Wip = productLevel3Wip;
  }

  public String getAttributeId() {
    return attributeId;
  }

  public void setAttributeId(String attributeId) {
    this.attributeId = attributeId;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.format("ProductLevel3AttributeWip [productLevel3Wip=%s, attributeId=%s, value=%s]", productLevel3Wip,
        attributeId, value);
  }

}
