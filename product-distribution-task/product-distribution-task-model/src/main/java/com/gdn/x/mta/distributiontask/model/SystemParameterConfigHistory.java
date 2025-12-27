package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = SystemParameterConfigHistory.TABLE_NAME)
public class SystemParameterConfigHistory extends GdnBaseEntity {

  private static final long serialVersionUID = -492427526844775817L;
  public static final String TABLE_NAME = "PDT_SYSTEM_PARAMETER_CONFIG_HISTORY";
  private static final String VARIABLE_KEY = "VARIABLE";
  private static final String OLD_VALUE_KEY = "OLD_VALUE";
  private static final String NEW_VALUE_KEY = "NEW_VALUE";

  @Column(name = SystemParameterConfigHistory.VARIABLE_KEY, nullable = false)
  private String variable;

  @Column(name = SystemParameterConfigHistory.OLD_VALUE_KEY)
  private String oldValue;

  @Column(name = SystemParameterConfigHistory.NEW_VALUE_KEY)
  private String newValue;
}
