package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.io.Serial;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Builder
@Table(name = ProductItemUomInfo.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {ProductItemUomInfo.COLUMN_SKU_CODE})})
@ToString(exclude = "productItem")
public class ProductItemUomInfo extends GdnBaseEntity {

  @Serial
  private static final long serialVersionUID = -8499624429436264633L;

  public static final String TABLE_NAME = "PCC_PRODUCT_ITEM_UOM_INFO";
  public static final String COLUMN_SKU_CODE = "SKU_CODE";
  public static final String COLUMN_PRODUCT_ITEM_ID = "PRODUCT_ITEM_ID";
  public static final String COLUMN_SELLER_CODE = "SELLER_CODE";
  public static final String COLUMN_ORIGIN = "ORIGIN";
  public static final String COLUMN_UOM = "UOM";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_EXPIRY = "EXPIRY";

  @OneToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductItemUomInfo.COLUMN_PRODUCT_ITEM_ID)
  private ProductItem productItem;

  @Column(name = ProductItemImage.COLUMN_PRODUCT_ID, insertable = false, updatable = false)
  private String productItemId;

  @Column(name = ProductItemUomInfo.COLUMN_SKU_CODE)
  private String skuCode;

  @Column(name = ProductItemUomInfo.COLUMN_PRODUCT_CODE)
  private String productCode;

  @Column(name = ProductItemUomInfo.COLUMN_SELLER_CODE)
  private String sellerCode;

  @Enumerated(EnumType.STRING)
  @Column(name = ProductItemUomInfo.COLUMN_ORIGIN)
  private Origin origin;

  @Column(name = ProductItemUomInfo.COLUMN_UOM)
  private String uom;

  @Column(name = ProductItemUomInfo.COLUMN_EXPIRY)
  private boolean expiry;

}