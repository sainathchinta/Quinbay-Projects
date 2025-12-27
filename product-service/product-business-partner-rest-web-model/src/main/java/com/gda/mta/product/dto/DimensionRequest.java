package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.util.Collections;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DimensionRequest implements MasterDataUpdateRequest{
  @Serial
  private static final long serialVersionUID = -58043492117174799L;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Integer productShippingType;
  @Override
  public String getDescription() {
    return null;
  }

  @Override
  public void setDescription(String description) {

  }

  @Override
  public String getUrl() {
    return null;
  }

  @Override
  public void setUrl(String url) {

  }

  @Override
  public String getProductCode() {
    return null;
  }

  @Override
  public String getProductName() {
    return null;
  }

  public void setProductType(Integer productType) {
    this.productShippingType = productType;
  }

  @Override
  public Integer getProductType() {
    return this.productShippingType;
  }

  @Override
  public Set<L3InfoUpdateChangeType> getMasterDataEditChangeTypes() {
    return Collections.emptySet();
  }

  @Override
  public Double getShippingWeight() {
    return null;
  }

  @Override
  public void setShippingWeight(Double shippingWeight) {

  }


  @Override
  public boolean isOff2OnChannelActive() {
    return false;
  }

  @Override
  public boolean isPureInstoreProduct() {
    return false;
  }

  @Override
  protected Object clone() throws CloneNotSupportedException {
    return super.clone();
  }
}
