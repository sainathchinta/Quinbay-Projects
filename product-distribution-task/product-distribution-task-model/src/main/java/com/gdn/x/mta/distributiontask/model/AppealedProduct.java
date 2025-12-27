package com.gdn.x.mta.distributiontask.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = AppealedProduct.TABLE_NAME, uniqueConstraints = {
  @UniqueConstraint(columnNames = {AppealedProduct.COLUMN_PRODUCT_CODE})})
public class AppealedProduct extends GdnBaseEntity {

  private static final long serialVersionUID = -2222382112406616778L;
  public static final String TABLE_NAME = "PDT_APPEALED_PRODUCTS";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_APPEAL_PRODUCT_NOTES = "APPEALED_PRODUCT_NOTES";

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE, nullable = false)
  private String businessPartnerCode;

  @Column(name = COLUMN_APPEAL_PRODUCT_NOTES)
  private String appealedProductNotes;

}
