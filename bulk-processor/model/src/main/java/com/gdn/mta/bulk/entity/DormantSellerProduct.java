package com.gdn.mta.bulk.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
@Entity
@EqualsAndHashCode(callSuper=true)
@Table(name = DormantSellerProduct.TABLE_NAME)
public class DormantSellerProduct extends GdnBaseEntity {

  public static final String TABLE_NAME = "BLP_DORMANTSELLER_PRODUCT";

  private static final String COLUMN_ITEM_SKU = "ITEM_SKU";
  private static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String COLUMN_STATUS = "STATUS";
  private static final String COLUMN_PRODUCT_STATUS = "PRODUCT_STATUS";
  private static final String COLUMN_DORMANT_SELLER_EVENT_ID = "DORMANT_SELLER_EVENT_ID";
  private static final String COLUMN_PROCESS_TYPE = "PROCESS_TYPE";

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = COLUMN_STATUS)
  private String status;

  @Column(name = COLUMN_ITEM_SKU)
  private String itemSku;

  @Column(name = COLUMN_PRODUCT_STATUS)
  private String productStatus;

  @Column(name = COLUMN_DORMANT_SELLER_EVENT_ID)
  private String dormantSellerEventId;

  @Column(name = COLUMN_PROCESS_TYPE)
  private String processType;
}
