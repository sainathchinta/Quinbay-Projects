package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

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
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBusinessPartnerWebRequest {

  private String businessPartnerCode;
  private String productId;
  private String gdnProductSku;
  private boolean activated = false;
  private String productName;
  private String categoryName;
  private String categoryCode;
  private String brand;
  private List<ProductItemBusinessPartnerWebRequest> productItemBusinessPartners = new ArrayList<>();
  private List<ProductBusinessPartnerAttributeWebRequest> productBusinessPartnerAttributes = new ArrayList<>();
  private List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequests = new ArrayList<>();
}
