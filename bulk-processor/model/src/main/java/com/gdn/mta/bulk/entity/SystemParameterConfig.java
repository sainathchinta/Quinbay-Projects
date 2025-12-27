package com.gdn.mta.bulk.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper=true)
@Entity
@Table(name = SystemParameterConfig.TABLE_NAME)
public class SystemParameterConfig extends GdnBaseEntity {
  public static final String TABLE_NAME = "SYSTEM_PARAMETER_CONFIG";

  private static final String VARIABLE_KEY = "VARIABLE";
  private static final String VALUE_KEY = "VALUE";
  private static final String DESCRIPTION_KEY = "DESCRIPTION";
  private static final long serialVersionUID = -4446712180767214612L;

  @Column(name = SystemParameterConfig.VARIABLE_KEY, nullable = false)
  private String variable;

  @Column(name = SystemParameterConfig.VALUE_KEY, nullable = false)
  private String value;

  @Column(name = SystemParameterConfig.DESCRIPTION_KEY)
  private String description;
}
