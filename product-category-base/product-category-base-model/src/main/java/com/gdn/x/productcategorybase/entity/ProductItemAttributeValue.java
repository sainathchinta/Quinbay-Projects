package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = ProductItemAttributeValue.TABLE_NAME)
public class ProductItemAttributeValue extends GdnBaseEntity {

  private static final long serialVersionUID = -1024651438405592183L;
  public static final String TABLE_NAME = "PCC_PRODUCT_ITEM_ATTRIBUTE_VALUE";
  public static final String COLUMN_PRODUCT_ITEM_ID = "PRODUCT_ITEM_ID";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_VALUE = "VALUE";

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductItemAttributeValue.COLUMN_PRODUCT_ITEM_ID)
  private ProductItem productItem;

  @Column(name = ProductItemAttributeValue.COLUMN_PRODUCT_ITEM_ID, insertable = false, updatable = false)
  private String productItemId;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductItemAttributeValue.COLUMN_ATTRIBUTE_ID)
  private Attribute attribute;

  @Column(name = ProductItemAttributeValue.COLUMN_ATTRIBUTE_ID, insertable = false, updatable = false)
  private String attributeId;

  @Column(name = ProductItemAttributeValue.COLUMN_VALUE)
  private String value;

  public ProductItemAttributeValue(Attribute attribute, String value) {
    this.attribute = attribute;
    this.value = value;
  }

  @Override
  public String toString() {
    return String.format("ProductItemAttributeValue [attribute=%s, value=%s, toString()=%s]", this.attribute,
        this.value, super.toString());
  }
}
