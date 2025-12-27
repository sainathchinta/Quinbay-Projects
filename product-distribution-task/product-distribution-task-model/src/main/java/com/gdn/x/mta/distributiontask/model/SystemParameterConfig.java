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
@Table(name = SystemParameterConfig.TABLE_NAME)
public class SystemParameterConfig extends GdnBaseEntity {

  private static final long serialVersionUID = 7133255688927679507L;
  public static final String TABLE_NAME = "PDT_SYSTEM_PARAMETER_CONFIG";
  private static final String VARIABLE_KEY = "VARIABLE";
  private static final String VALUE_KEY = "VALUE";
  private static final String DESCRIPTION_KEY = "DESCRIPTION";

  @Column(name = SystemParameterConfig.VARIABLE_KEY, nullable = false)
  private String variable;

  @Column(name = SystemParameterConfig.VALUE_KEY, nullable = false)
  private String value;

  @Column(name = SystemParameterConfig.DESCRIPTION_KEY)
  private String description;
}
