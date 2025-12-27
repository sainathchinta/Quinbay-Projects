package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductPriceAndWholesaleRequest implements Serializable {

  private static final long serialVersionUID = 4790247626554469555L;

  private String currency;
  private double offerPrice;
  private double listPrice;
  private String channel;
  private Long version;
  private List<DiscountPriceRequest> discountPrice;
  private Boolean wholesalePriceActivated;
  private List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests;
  }
