package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import javax.persistence.Column;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = ProductEmails.TABLE_NAME)
public class ProductEmails extends GdnBaseEntity {
  public static final String TABLE_NAME = "PDT_PRODUCT_EMAILS";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_NOTES = "NOTES";
  public static final String COLUMN_PRODUCT_EMAIL_TYPE = "PRODUCT_EMAIL_TYPE";
  public static final String COLUMN_PRODUCT_SKU = "PRODUCT_SKU";
  public static final String COLUMN_PRODUCT_NAME = "PRODUCT_NAME";
  public static final String COLUMN_STATUS= "STATUS";

  @Column(name = ProductEmails.COLUMN_BUSINESS_PARTNER_CODE, nullable = false)
  private String businessPartnerCode;

  @Column(name = ProductEmails.COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = ProductEmails.COLUMN_NOTES, nullable = false)
  private String notes;

  @Column(name = ProductEmails.COLUMN_PRODUCT_SKU, nullable = false)
  private String productSku;

  @Column(name = ProductEmails.COLUMN_PRODUCT_NAME, nullable = false)
  private String productName;

  @Column(name = ProductEmails.COLUMN_STATUS, nullable = false)
  private String status;

  @Column(name = ProductEmails.COLUMN_PRODUCT_EMAIL_TYPE, nullable = false)
  private String productEmailType;

}
