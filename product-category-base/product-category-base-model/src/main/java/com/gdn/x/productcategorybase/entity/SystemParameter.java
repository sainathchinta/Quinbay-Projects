package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper=true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = SystemParameter.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, SystemParameter.COLUMN_VARIABLE})})
public class SystemParameter extends GdnBaseEntity {

  private static final long serialVersionUID = -1603456237060089589L;
  public static final String TABLE_NAME = "PCC_SYSTEM_PARAMETER";
  public static final String COLUMN_VARIABLE = "VARIABLE";
  public static final String COLUMN_VALUE = "VALUE";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";

  @Column(name = COLUMN_VARIABLE, nullable = false)
  private String variable;

  @Column(name = COLUMN_VALUE, nullable = false)
  private String value;

  @Column(name = COLUMN_DESCRIPTION, nullable = false)
  private String description;
}
