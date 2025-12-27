package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import com.gdn.GdnBaseEntity;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Entity
@Table(name = ApplicationConfigProperties.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    GdnBaseEntity.STORE_ID, ApplicationConfigProperties.COLUMN_PROPERTY_NAME})})
public class ApplicationConfigProperties extends GdnBaseEntity {

  private static final long serialVersionUID = -8018269085922274133L;
  public static final String TABLE_NAME = "CONFIG_PROPERTIES";
  public static final String COLUMN_PROPERTY_NAME = "PROPERTY_NAME";
  public static final String COLUMN_VALUE = "VALUE";

  @Column(name = COLUMN_PROPERTY_NAME, nullable = false)
  private String propertyName;

  @Column(name = COLUMN_VALUE, nullable = false)
  private String value;

}
