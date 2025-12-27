package com.gdn.x.product.rest.web.model.request;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class PriceRequest extends BaseRequest {

  private static final long serialVersionUID = 1L;

  private String currency;
  private double offerPrice;
  private double listPrice;
  private String channel = ChannelName.DEFAULT.toString();
  private List<DiscountPriceDTO> discountPrice;
  private Boolean wholesalePriceActivated;

  public PriceRequest(String currency, double offerPrice, double listPrice, String channel,
      List<DiscountPriceDTO> discountPrice) {
    super();
    this.currency = currency;
    this.offerPrice = offerPrice;
    this.listPrice = listPrice;
    this.channel = channel;
    this.discountPrice = discountPrice;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }
}
