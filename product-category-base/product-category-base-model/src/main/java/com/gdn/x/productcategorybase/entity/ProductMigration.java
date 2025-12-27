package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Builder
@Table(name = ProductMigration.TABLE_NAME)
public class ProductMigration extends GdnBaseEntity {
  private static final long serialVersionUID = -743305312879823999L;
  public static final String TABLE_NAME = "PCC_PRODUCT_MIGRATION";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String MIGRATION_TYPE = "MIGRATION_TYPE";
  public static final String MIGRATION_PAYLOAD = "MIGRATION_PAYLOAD";
  public static final String ERROR_MESSAGE = "ERROR_MESSAGE";


  @Column(name = COLUMN_PRODUCT_CODE)
  private String productCode;

  @Column(name = COLUMN_STATUS)
  private String status;

  @Column(name = MIGRATION_TYPE)
  private String migrationType;

  @Column(name = MIGRATION_PAYLOAD)
  private String migrationPayload;

  @Column(name = ERROR_MESSAGE)
  private String errorMessage;

}
