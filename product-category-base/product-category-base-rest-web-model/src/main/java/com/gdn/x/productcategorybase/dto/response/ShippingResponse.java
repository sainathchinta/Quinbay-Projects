package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ShippingResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 1L;

  private boolean deliveredByMerchant;
  private boolean specialHandling;
  private boolean directFlight;
  private Boolean ageLimit;
  private boolean sizeChartRequired;

  public ShippingResponse() {
    // nothing to do here
  }

  public boolean isDeliveredByMerchant() {
    return this.deliveredByMerchant;
  }

  public boolean isDirectFlight() {
    return this.directFlight;
  }

  public boolean isSpecialHandling() {
    return this.specialHandling;
  }

  public void setDeliveredByMerchant(boolean deliveredByMerchant) {
    this.deliveredByMerchant = deliveredByMerchant;
  }

  public void setDirectFlight(boolean directFlight) {
    this.directFlight = directFlight;
  }

  public void setSpecialHandling(boolean specialHandling) {
    this.specialHandling = specialHandling;
  }

  public Boolean getAgeLimit() {
    return ageLimit;
  }

  public void setAgeLimit(Boolean ageLimit) {
    this.ageLimit = ageLimit;
  }

  public boolean isSizeChartRequired() {
    return sizeChartRequired;
  }

  public void setSizeChartRequired(boolean sizeChartRequired) {
    this.sizeChartRequired = sizeChartRequired;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ShippingResponse{");
    sb.append("deliveredByMerchant=").append(deliveredByMerchant);
    sb.append(", specialHandling=").append(specialHandling);
    sb.append(", directFlight=").append(directFlight);
    sb.append(", ageLimit=").append(ageLimit);
    sb.append(", sizeChartRequired=").append(sizeChartRequired);
    sb.append('}');
    return sb.toString();
  }
}
