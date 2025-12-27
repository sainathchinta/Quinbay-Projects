package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 09/12/2018 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductItemBusinessPartnerWebRequest {

  private String productItemId;
  private String productHashCode;
  private Integer productType;
  private String merchantSku;
  private String gdnProductItemSku;
  private Double price;
  private Double salePrice;
  private Date saleStartDate;
  private Date saleEndDate;
  private Integer stock;
  private boolean markDefaultAddress;
  private Integer minimumStock;
  private String pickupPointId;
  private boolean display;
  private boolean buyable;
  private boolean installation;
  private String itemCode;
  private List<ProductItemWholesalePriceWebRequest> productItemWholesalePriceRequests = new ArrayList<>();
  private Boolean wholesalePriceActivated;
}
