package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import com.gdn.GdnBaseEntity;
import com.gdn.mta.product.enums.ProductSyncStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author anand
 * @since Sep 2019
 */
@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = ProductItemSyncStatus.TABLE_NAME, uniqueConstraints = {
  @UniqueConstraint(columnNames = {
    GdnBaseEntity.STORE_ID, ProductItemSyncStatus.BUSINESS_PARTNER, ProductItemSyncStatus.GDN_ITEM_SKU
  })
})
public class ProductItemSyncStatus extends GdnBaseEntity {

  public static final String TABLE_NAME = "prd_product_item_sync_status";

  public static final String BUSINESS_PARTNER = "business_partner_code";

  public static final String LINKED_BUSINESS_PARTNER = "linked_business_partner_code";

  public static final String GDN_ITEM_SKU = "gdn_item_sku";

  public static final String SYNC_STATUS = "sync_status";

  public static final String PROCESS_ID = "process_id";

  public static final String ATTEMPT = "attempt";

  private static final long serialVersionUID = 2069820725141618592L;

  @Column(name = ProductItemSyncStatus.BUSINESS_PARTNER, nullable = false)
  private String businessPartnerCode;

  @Column(name = ProductItemSyncStatus.LINKED_BUSINESS_PARTNER, nullable = false)
  private String linkedBusinessPartnerCode;

  @Column(name = ProductItemSyncStatus.GDN_ITEM_SKU, nullable = false)
  private String gdnItemSku;

  @Column(name = ProductItemSyncStatus.PROCESS_ID, nullable = false)
  private String processId;

  @Enumerated(EnumType.STRING)
  @Column(name = ProductItemSyncStatus.SYNC_STATUS, nullable = false)
  private ProductSyncStatus productSyncStatus;

  @Column(name = ProductItemSyncStatus.ATTEMPT, nullable = false)
  private int attempt;

  public ProductItemSyncStatus withStatus(ProductSyncStatus productSyncStatus) {
    this.setProductSyncStatus(productSyncStatus);
    return this;
  }

}
