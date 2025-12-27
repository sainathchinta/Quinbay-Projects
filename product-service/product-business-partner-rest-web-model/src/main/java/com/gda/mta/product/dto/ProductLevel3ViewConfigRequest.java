package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ProductLevel3ViewConfigRequest extends BaseRequest {

  private static final long serialVersionUID = -2614781582594208223L;
  private String channelId;
  private Boolean display;
  private Boolean buyable;
  private Boolean b2bDisplay;
  private Boolean b2bBuyable;

  public ProductLevel3ViewConfigRequest(String channelId, Boolean display, Boolean buyable) {
    super();
    this.channelId = channelId;
    this.display = display;
    this.buyable = buyable;
  }

  public String getChannelId() {
    return channelId;
  }
}
