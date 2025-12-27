package com.gdn.partners.pcu.external.model.request;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 12/12/2018 AD.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ProductBusinessPartnerServiceRequest extends BaseDTOServiceRequest{

  private String businessPartnerCode;
  private String productId;
  private String gdnProductSku;
  private boolean activated = false;
  private String productName;
  private String categoryName;
  private String categoryCode;
  private String brand;
  private List<ProductItemBusinessPartnerServiceRequest> productItemBusinessPartners = new ArrayList<>();
  private List<ProductBusinessPartnerAttributeServiceRequest> productBusinessPartnerAttributes = new ArrayList<>();
}
