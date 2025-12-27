package com.gda.mta.product.dto;

import java.util.Date;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by Vishal on 18/05/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBusinessPartnerConfigRequest extends BaseRequest{

  private static final long serialVersionUID = 2195845276012940854L;

  private String bpCode;
  private Date productToActivateNotifyMailDate;

  public ProductBusinessPartnerConfigRequest() {
  }

  public ProductBusinessPartnerConfigRequest(String bpCode, Date productToActivateNotifyMailDate) {
    this.bpCode = bpCode;
    this.productToActivateNotifyMailDate = productToActivateNotifyMailDate;
  }

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
