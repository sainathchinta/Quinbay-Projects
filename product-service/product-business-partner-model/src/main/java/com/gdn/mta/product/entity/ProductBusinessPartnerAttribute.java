package com.gdn.mta.product.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.GdnBaseEntity;

@Entity
@Table(name = ProductBusinessPartnerAttribute.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    GdnBaseEntity.STORE_ID, ProductBusinessPartnerAttribute.COLUMN_PRODUCT_BUSINESS_PARTNER_ID,
    ProductBusinessPartnerAttribute.COLUMN_ATTRIBUTE_ID})})
public class ProductBusinessPartnerAttribute extends GdnBaseEntity {

  private static final long serialVersionUID = -8531995416325983241L;
  public static final String TABLE_NAME = "PRD_PRODUCT_BUSINESS_PARTNER_ATTRIBUTE";
  public static final String COLUMN_PRODUCT_BUSINESS_PARTNER_ID = "PRODUCT_BUSINESS_PARTNER_ID";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_VALUE = "VALUE";

  @ManyToOne
  @JoinColumn(name = COLUMN_PRODUCT_BUSINESS_PARTNER_ID)
  private ProductBusinessPartner productBusinessPartner;

  @Column(name = COLUMN_ATTRIBUTE_ID, nullable = false)
  private String attributeId;

  @Column(name = COLUMN_VALUE, nullable = false)
  private String value;

  public ProductBusinessPartnerAttribute() {}

  public ProductBusinessPartnerAttribute(ProductBusinessPartner productBusinessPartner, String attributeId,
      String value, String createdBy, Date createdDate, String storeId) {
    this.productBusinessPartner = productBusinessPartner;
    this.attributeId = attributeId;
    this.value = value;
    setCreatedBy(createdBy);
    setCreatedDate(createdDate);
    setStoreId(storeId);
  }

  public String getAttributeId() {
    return attributeId;
  }

  public ProductBusinessPartner getProductBusinessPartner() {
    return productBusinessPartner;
  }

  public String getValue() {
    return value;
  }

  public void setAttributeId(String attributeId) {
    this.attributeId = attributeId;
  }

  public void setProductBusinessPartner(ProductBusinessPartner productBusinessPartner) {
    this.productBusinessPartner = productBusinessPartner;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
      .append("id", this.getId())
      .append("attributeId", attributeId)
      .append("value", value)
      .toString();
  }
}
