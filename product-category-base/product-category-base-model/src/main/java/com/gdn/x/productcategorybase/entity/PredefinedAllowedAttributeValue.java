package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@Entity
@Table(name = PredefinedAllowedAttributeValue.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {PredefinedAllowedAttributeValue.COLUMN_PREDEFINED_ALLOWED_ATTRIBUTE_CODE})})
public class PredefinedAllowedAttributeValue extends GdnBaseEntity {

  private static final long serialVersionUID = -1435353917036838635L;
  public static final String TABLE_NAME = "PCC_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "PREDEFINED_ALLOWED_ATTRIBUTE_CODE";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = PredefinedAllowedAttributeValue.COLUMN_ATTRIBUTE_ID)
  private Attribute attribute;

  @Column(name = PredefinedAllowedAttributeValue.COLUMN_ATTRIBUTE_ID, insertable = false, updatable = false)
  private String attributeId;

  @Column(name = PredefinedAllowedAttributeValue.COLUMN_PREDEFINED_ALLOWED_ATTRIBUTE_CODE)
  private String predefinedAllowedAttributeCode;

  @Column
  private String value;

  @Column
  private String valueEn;

  @Column(name = PredefinedAllowedAttributeValue.COLUMN_SEQUENCE)
  private Integer sequence;

  public PredefinedAllowedAttributeValue(Attribute attribute, String value, String storeId, Integer sequence) {
    this.attribute = attribute;
    this.value = value;
    this.valueEn = valueEn;
    this.sequence = sequence;
    this.setStoreId(storeId);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("PredefinedAllowedAttributeValue{");
    sb.append(", predefinedAllowedAttributeCode='").append(predefinedAllowedAttributeCode)
        .append('\'');
    sb.append(", value='").append(value).append('\'');
    sb.append(", valueEn='").append(valueEn).append('\'');
    sb.append(", sequence=").append(sequence);
    sb.append('}');
    return sb.toString();
  }
}
