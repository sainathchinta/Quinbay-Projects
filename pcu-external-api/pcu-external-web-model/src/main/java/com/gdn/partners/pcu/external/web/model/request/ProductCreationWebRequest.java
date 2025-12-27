package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 13/12/2018 AD.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCreationWebRequest extends ProductWebRequest {

  private String businessPartnerId;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String gdnProductSku;
  private String categoryName;
  private String oldProductCode;
  private String oldProductRejectionNote;
  private boolean forReview;
  private boolean b2cActivated = true;
  private boolean b2bActivated;
  private boolean bundleProduct;
  private List<ProductItemCreationWebRequest> productItemRequests = new ArrayList<>();
  private List<ProductBusinessPartnerAttributeWebRequest> productBusinessPartnerAttributes = new ArrayList<>();
  private List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequests = new ArrayList<>();
  private PreOrderWebRequest preOrder;
  private List<ImageRequest> commonImages = new ArrayList<>();
  private String sizeChartCode;
  private boolean externalUser;
  private VideoAddEditRequest videoAddEditRequest;
  private Boolean videoUpdated;
  private Map<String, String> distributionInfoRequest;
}
