package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ShippingRequest implements Serializable {

  private static final long serialVersionUID = -928413577407453434L;

  private boolean deliveredByMerchant;
  private boolean specialHandling;
  private boolean directFlight;
  private Boolean ageLimit;
  private boolean sizeChartRequired;

  public ShippingRequest() {
    // nothing to do here
  }

  public ShippingRequest(boolean deliveredByMerchant, boolean specialHandling, boolean directFlight) {
    super();
    this.deliveredByMerchant = deliveredByMerchant;
    this.specialHandling = specialHandling;
    this.directFlight = directFlight;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (this.getClass() != obj.getClass()) {
      return false;
    }
    ShippingRequest other = (ShippingRequest) obj;
    if (this.deliveredByMerchant != other.deliveredByMerchant) {
      return false;
    }
    if (this.directFlight != other.directFlight) {
      return false;
    }
    if (this.specialHandling != other.specialHandling) {
      return false;
    }
    if (this.ageLimit != other.ageLimit) {
      return false;
    }
    return true;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = (prime * result) + (this.deliveredByMerchant ? 1231 : 1237);
    result = (prime * result) + (this.directFlight ? 1231 : 1237);
    result = (prime * result) + (this.specialHandling ? 1231 : 1237);
    result = (prime * result) + (Objects.nonNull(this.ageLimit) && this.ageLimit ? 1231 : 1237);
    return result;
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
    return String.format("ShippingRequest [deliveredByMerchant=%s, specialHandling=%s, directFlight=%s, ageLimit=%s, sizeChartRequired=%s]",
        this.deliveredByMerchant, this.specialHandling, this.directFlight, this.ageLimit, this.sizeChartRequired);
  }

}
