package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@EqualsAndHashCode(callSuper=true)
@NoArgsConstructor
@Entity
@Table(name = AllowedAttributeValue.TABLE_NAME,
       uniqueConstraints = {@UniqueConstraint(columnNames = {AllowedAttributeValue.COLUMN_ALLOWED_ATTRIBUTE_CODE})})
public class AllowedAttributeValue extends GdnBaseEntity {

  private static final long serialVersionUID = -8746037726495946846L;
  public static final String TABLE_NAME = "PCC_ALLOWED_ATTRIBUTE_VALUE";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_ALLOWED_ATTRIBUTE_CODE = "ALLOWED_ATTRIBUTE_CODE";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";
  public static final String VALUE_TYPE = "VALUE_TYPE";

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = AllowedAttributeValue.COLUMN_ATTRIBUTE_ID)
  private Attribute attribute;

  @Column(name = AllowedAttributeValue.COLUMN_ATTRIBUTE_ID, insertable = false, updatable = false)
  private String attributeId;

  @Column(name = AllowedAttributeValue.COLUMN_ALLOWED_ATTRIBUTE_CODE)
  private String allowedAttributeCode;

  @Column(name = AllowedAttributeValue.VALUE_TYPE)
  private String valueType;

  @Column
  private String value;

  @Column(name = AllowedAttributeValue.COLUMN_SEQUENCE)
  private Integer sequence;

  public AllowedAttributeValue(Attribute attribute, String value, String storeId,
      Integer sequence) {
    this.attribute = attribute;
    this.value = value;
    this.sequence = sequence;
    this.setStoreId(storeId);
  }

  public AllowedAttributeValue(Attribute attribute, String value, String valueType, String storeId,
      Integer sequence) {
    this.attribute = attribute;
    this.value = value;
    this.valueType = valueType;
    this.sequence = sequence;
    this.setStoreId(storeId);
  }

  @Override
  public String toString() {
    return String.format(
        "AllowedAttributeValue [allowedAttributeCode=%s, value=%s, valueType=%s, sequence=%s, "
            + "toString()=%s]",
        this.allowedAttributeCode, this.value, this.valueType, this.sequence, super.toString());
  }

}
