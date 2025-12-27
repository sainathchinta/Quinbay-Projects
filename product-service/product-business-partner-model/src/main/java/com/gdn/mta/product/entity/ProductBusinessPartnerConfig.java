package com.gdn.mta.product.entity;

import java.util.Date;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.GdnBaseEntity;

/**
 * Created by Vishal on 17/05/18.
 */
@Entity
@Table(name = ProductBusinessPartnerConfig.TABLE_NAME)
public class ProductBusinessPartnerConfig extends GdnBaseEntity {

  private static final long serialVersionUID = 3089920307307295228L;
  public static final String TABLE_NAME = "PRD_PRODUCT_BUSINESS_PARTNER_CONFIG";

  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_PRODUCT_TO_ACTIVATE_NOTIFY_MAIL_DATE =
      "PRODUCT_TO_ACTIVATE_NOTIFY_MAIL_DATE";

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE, nullable = false)
  private String bpCode;

  @Column (name = COLUMN_PRODUCT_TO_ACTIVATE_NOTIFY_MAIL_DATE, nullable = false)
  @Temporal(TemporalType.TIMESTAMP)
  private Date productToActivateNotifyMailDate;

  public ProductBusinessPartnerConfig(String bpCode,
      Date productToActivateNotifyMailDate) {
    this.bpCode = bpCode;
    this.productToActivateNotifyMailDate = productToActivateNotifyMailDate;
  }

  public ProductBusinessPartnerConfig() {}

  public String getBpCode() {
    return bpCode;
  }

  public void setBpCode(String bpCode) {
    this.bpCode = bpCode;
  }

  public Date getProductToActivateNotifyMailDate() {
    return productToActivateNotifyMailDate;
  }

  public void setProductToActivateNotifyMailDate(Date productToActivateNotifyMailDate) {
    this.productToActivateNotifyMailDate = productToActivateNotifyMailDate;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("bpCode", bpCode)
        .append("productToActivateNotifyMailDate", productToActivateNotifyMailDate).toString();
  }
}
