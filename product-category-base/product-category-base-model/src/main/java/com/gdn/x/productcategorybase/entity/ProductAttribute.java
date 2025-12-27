package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = ProductAttribute.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID,
    ProductAttribute.COLUMN_PRODUCT_ID, ProductAttribute.COLUMN_ATTRIBUTE_ID, ProductAttribute.COLUMN_SEQUENCE})})
public class ProductAttribute extends GdnBaseEntity {

  private static final long serialVersionUID = 8692925353238834248L;

  public static final String TABLE_NAME = "PCC_PRODUCT_ATTRIBUTE";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_PRODUCT_ATTRIBUTE_NAME = "PRODUCT_ATTRIBUTE_NAME";
  public static final String COLUMN_IS_OWN_BY_PRODUCT_ITEM = "IS_OWN_BY_PRODUCT_ITEM";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";
  public static final String EXTRACTED_VALUE = "EXTRACTED_VALUE";

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductAttribute.COLUMN_ATTRIBUTE_ID)
  private Attribute attribute;

  @Column(name = ProductAttribute.COLUMN_ATTRIBUTE_ID, insertable = false, updatable = false)
  private String attributeId;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductAttribute.COLUMN_PRODUCT_ID)
  private Product product;

  @Column(name = ProductAttribute.COLUMN_PRODUCT_ID, insertable = false, updatable = false)
  private String productId;

  @Column(name = ProductAttribute.COLUMN_PRODUCT_ATTRIBUTE_NAME)
  private String productAttributeName;

  @Column(name = ProductAttribute.COLUMN_IS_OWN_BY_PRODUCT_ITEM)
  private boolean isOwnByProductItem;

  @Column(name = ProductAttribute.COLUMN_SEQUENCE)
  private Integer sequence;

  @Column(name = ProductAttribute.EXTRACTED_VALUE)
  private boolean extractedValue;

  @OneToMany(mappedBy = "productAttribute", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<ProductAttributeValue> productAttributeValues = new ArrayList<>();

  public ProductAttribute(Attribute attribute, Product product, String productAttributeName, boolean isOwnByProductItem,
      Integer sequence, String storeId) {
    this.attribute = attribute;
    this.product = product;
    this.productAttributeName = productAttributeName;
    this.isOwnByProductItem = isOwnByProductItem;
    this.sequence = sequence;
    this.setStoreId(storeId);
  }

  @Override
  public String toString() {
    return String.format(
        "ProductAttribute [productAttributeName=%s, isOwnByProductItem=%s, sequence=%s, productAttibuteValues=%s, extractedValue=%s, toString()=%s]",
        this.productAttributeName, this.isOwnByProductItem, this.sequence,
        this.productAttributeValues, this.extractedValue, super.toString());
  }

}
