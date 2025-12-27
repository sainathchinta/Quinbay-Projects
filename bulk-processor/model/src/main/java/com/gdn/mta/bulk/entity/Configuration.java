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
@Table(name = Configuration.TABLE_NAME)
public class Configuration extends GdnBaseEntity {
  public static final String TABLE_NAME = "BLP_DELETION_CONFIGURATION";

  private static final String COLUMN_SERVICE_KEY = "SERVICE_KEY";
  private static final String COLUMN_FOLDER_NAME = "FOLDER_NAME";
  private static final String COLUMN_IS_DISABLED = "IS_DISABLED";
  private static final String COLUMN_AGE_OF_DELETION = "AGE_OF_DELETION";
  private static final long serialVersionUID = 6828038076031911191L;

  @Column(name = Configuration.COLUMN_FOLDER_NAME)
  private String folderName;

  @Column(name = Configuration.COLUMN_IS_DISABLED, nullable = false)
  private boolean isDisabled;

  @Column(name = Configuration.COLUMN_AGE_OF_DELETION, nullable = false)
  private int ageOfDeletion;

  @Column(name = Configuration.COLUMN_SERVICE_KEY, nullable = false)
  private String serviceKey;
}
