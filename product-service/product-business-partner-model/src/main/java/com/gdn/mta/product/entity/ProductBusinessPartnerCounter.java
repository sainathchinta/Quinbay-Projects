package com.gdn.mta.product.entity;

import com.gdn.GdnBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

@Data
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = ProductBusinessPartnerCounter.TABLE_NAME, uniqueConstraints = {
  @UniqueConstraint(columnNames = {ProductBusinessPartnerCounter.COLUMN_BUSINESS_PARTNER_CODE})})
public class ProductBusinessPartnerCounter extends GdnBaseEntity {
  public static final String TABLE_NAME = "PRD_BUSINESS_PARTNER_COUNTER";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "business_partner_code";
  private static final long serialVersionUID = -3968690320238814852L;
  private static final String COLUMN_APPEALED_PRODUCT_COUNT = "appealed_product_count";

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE, nullable = false)
  private String businessPartnerCode;

  @Column(name = COLUMN_APPEALED_PRODUCT_COUNT)
  private int appealedProductCount;
}
