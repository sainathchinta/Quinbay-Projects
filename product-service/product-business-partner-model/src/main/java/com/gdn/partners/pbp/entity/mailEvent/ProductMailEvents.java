package com.gdn.partners.pbp.entity.mailEvent;

import lombok.*;

import jakarta.persistence.*;

import com.gdn.GdnBaseEntity;

@Entity
@Table(name = ProductMailEvents.TABLE_NAME)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductMailEvents extends GdnBaseEntity {
  public static final String TABLE_NAME = "PRD_PRODUCT_MAIL_EVENTS";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_NOTES = "NOTES";
  public static final String COLUMN_EVENT = "MAIL_EVENT";
  public static final String COLUMN_PRODUCT_SKU = "PRODUCT_SKU";

  @Column(name = ProductMailEvents.COLUMN_BUSINESS_PARTNER_CODE , nullable = false)
  private String businessPartnerCode;

  @Column(name = ProductMailEvents.COLUMN_PRODUCT_CODE ,nullable = false)
  private String productCode;

  @Enumerated(EnumType.STRING)
  @Column(name = ProductMailEvents.COLUMN_EVENT, nullable = false)
  private ProductMailEventsEnum events;

  @Column(name = ProductMailEvents.COLUMN_NOTES ,nullable = false)
  private String notes;

  @Column(name = ProductMailEvents.COLUMN_PRODUCT_SKU ,nullable = false)
  private String productSku;
}
