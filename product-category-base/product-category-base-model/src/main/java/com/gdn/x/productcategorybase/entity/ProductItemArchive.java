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
@Builder
@Entity
@Table(name = ProductItemArchive.TABLE_NAME)
public class ProductItemArchive extends GdnBaseEntity{
  public static final String TABLE_NAME = "PCC_PRODUCT_ITEM_ARCHIVE";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_GENERATED_ITEM_NAME = "GENERATED_ITEM_NAME";
  public static final String COLUMN_HASH = "ITEM_HASH";
  public static final String COLUMN_UPC_CODE = "UPC_CODE";
  public static final String COLUMN_SKU_CODE = "SKU_CODE";
  public static final String COLUMN_ACTIVATED = "IS_ACTIVATED";
  public static final String COLUMN_VIEWABLE = "IS_VIEWABLE";
  public static final String COLUMN_DANGEROUS_GOODS_LEVEL = "DANGEROUS_GOODS_LEVEL";
  public static final String COLUMN_INTERNAL_UPDATE = "INTERNAL_UPDATE";
  public static final String COLUMN_CONTENT_CHANGED = "IS_CONTENT_CHANGED";
  public static final String COLUMN_SOURCE_ITEM_CODE = "SOURCE_ITEM_CODE";
  public static final String COLUMN_VAT_APPLICABLE = "VAT_APPLICABLE";

  @Column(name = COLUMN_PRODUCT_ID)
  private String productId;

  @Column(name = COLUMN_GENERATED_ITEM_NAME)
  private String generatedItemName;

  @Column(name = COLUMN_UPC_CODE)
  private String upcCode;

  @Column(name = COLUMN_SKU_CODE)
  private String skuCode;

  @Column(name = COLUMN_ACTIVATED)
  private boolean activated;

  @Column(name = COLUMN_VIEWABLE)
  private boolean viewable;

  @Column(name = COLUMN_INTERNAL_UPDATE)
  private boolean internalUpdate;

  @Column(name = COLUMN_HASH)
  private byte[] hash;

  @Column(name = COLUMN_SOURCE_ITEM_CODE)
  private String sourceItemCode;

  @Column(name = COLUMN_CONTENT_CHANGED)
  private boolean contentChanged;

  @Column(name = COLUMN_DANGEROUS_GOODS_LEVEL)
  private Integer dangerousGoodsLevel;

  @Column(name = COLUMN_VAT_APPLICABLE)
  private Boolean vatApplicable;
}
