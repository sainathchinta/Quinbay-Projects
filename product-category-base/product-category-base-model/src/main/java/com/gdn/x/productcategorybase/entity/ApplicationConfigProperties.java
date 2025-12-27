package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Entity
@Table(name = ApplicationConfigProperties.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    GdnBaseEntity.STORE_ID, ApplicationConfigProperties.COLUMN_PROPERTY_NAME})})
public class ApplicationConfigProperties extends GdnBaseEntity {

  public static final String TABLE_NAME = "CONFIG_PROPERTIES";
  public static final String COLUMN_PROPERTY_NAME = "PROPERTY_NAME";
  public static final String COLUMN_VALUE = "VALUE";
  private static final long serialVersionUID = 1060961505106486465L;

  @Column(name = COLUMN_PROPERTY_NAME, nullable = false)
  private String propertyName;

  @Column(name = COLUMN_VALUE, nullable = false)
  private String value;

}
