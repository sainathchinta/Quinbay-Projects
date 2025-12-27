package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = ProductMigration.TABLE_NAME)
public class ProductMigration extends GdnBaseEntity {
  private static final long serialVersionUID = -743305312879823999L;
  public static final String TABLE_NAME = "PDT_PRODUCT_MIGRATION";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_MIGRATION_TYPE = "MIGRATION_TYPE";

  @Column(name = COLUMN_PRODUCT_CODE)
  private String productCode;

  @Column(name = COLUMN_STATUS)
  private String status;

  @Column(name = COLUMN_MIGRATION_TYPE)
  private String migrationType;
}
