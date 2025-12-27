package com.gdn.x.mta.distributiontask.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Builder
@Table(name = ConfigProperties.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    GdnBaseEntity.STORE_ID, ConfigProperties.COLUMN_PROPERTY_NAME})})
public class ConfigProperties extends GdnBaseEntity {

  private static final long serialVersionUID = -8018269085922274133L;
  public static final String TABLE_NAME = "CONFIG_PROPERTIES";
  public static final String COLUMN_PROPERTY_NAME = "PROPERTY_NAME";
  public static final String COLUMN_VALUE = "VALUE";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";

  @Column(name = COLUMN_PROPERTY_NAME, nullable = false)
  private String propertyName;

  @Column(name = COLUMN_VALUE, nullable = false)
  private String value;

  @Column(name = COLUMN_DESCRIPTION, nullable = false)
  private String description;
}
