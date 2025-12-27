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
@Entity
@EqualsAndHashCode(callSuper=true)
@Table(name = SystemParameterConfigHistory.TABLE_NAME)
public class SystemParameterConfigHistory extends GdnBaseEntity {
  public static final String TABLE_NAME = "SYSTEM_PARAMETER_CONFIG_HISTORY";

  private static final String VARIABLE_KEY = "VARIABLE";
  private static final String OLD_VALUE_KEY = "OLD_VALUE";
  private static final String NEW_VALUE_KEY = "NEW_VALUE";
  private static final long serialVersionUID = -4446712180767214612L;

  @Column(name = SystemParameterConfigHistory.VARIABLE_KEY, nullable = false)
  private String variable;

  @Column(name = SystemParameterConfigHistory.OLD_VALUE_KEY)
  private String oldValue;

  @Column(name = SystemParameterConfigHistory.NEW_VALUE_KEY)
  private String newValue;
}
