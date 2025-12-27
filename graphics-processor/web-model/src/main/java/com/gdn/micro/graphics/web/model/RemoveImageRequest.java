package com.gdn.micro.graphics.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class RemoveImageRequest extends BaseRequest {

  private static final long serialVersionUID = 7011627327387778546L;
  private String value;
  private String randomSeed;

  /**
   * this value property class must be assign with encrypted value of "clientId"+ "," +"imagePath"
   * with coma as delimiter. assign random seed value every time you create this request
   */
  public RemoveImageRequest() {
    // nothing to do here
  }

  public String getRandomSeed() {
    return randomSeed;
  }

  public String getValue() {
    return value;
  }

  public void setRandomSeed(String randomSeed) {
    this.randomSeed = randomSeed;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("RemoveImageRequest [value=").append(value).append(", randomSeed=")
    .append(randomSeed).append("]");
    return builder.toString();
  }

}
