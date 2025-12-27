package com.gdn.mta.product.entity;

import com.gdn.GdnBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;


@Data
@Entity
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Table(name = ProductFbbMigration.TABLE_NAME)
public class ProductFbbMigration extends GdnBaseEntity {
  private static final long serialVersionUID = -1936647414940145863L;
  public static final String TABLE_NAME = "PRD_PRODUCT_FBB_MIGRATION";
  public static final String IDENTIFIER = "IDENTIFIER";
  public static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  public static final String PRODUCT_STATE = "PRODUCT_STATE";
  public static final String STATUS = "STATUS";
  public static final String MIGRATION_TYPE = "MIGRATION_TYPE";

  @Column(name = IDENTIFIER)
  private String identifier;

  @Column(name = PICKUP_POINT_CODE)
  private String pickupPointCode;

  @Column(name = PRODUCT_STATE)
  private String productState;

  @Column(name = STATUS)
  private String status;

  @Column(name = MIGRATION_TYPE)
  private String migrationType;
}
