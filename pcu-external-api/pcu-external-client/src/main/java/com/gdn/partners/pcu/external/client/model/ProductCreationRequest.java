package com.gdn.partners.pcu.external.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemLogisticsRequest;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCreationRequest extends ProductRequest {
  private static final long serialVersionUID = -4081319999907146849L;
  private String businessPartnerId;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String gdnProductSku;
  private String categoryName;
  private String oldProductCode;
  private String oldProductRejectionNote;
  private boolean forReview;
  private ProductCreationType productCreationType;
  private boolean containsUrlImage;
  private boolean imagesUpdated;
  private PreOrderRequest preOrder;
  private boolean freeSample;
  private boolean off2OnChannelActive;
  private boolean online;
  private int prioritySeller;
  private boolean b2cActivated;
  private boolean b2bActivated;
  private boolean bundleProduct;
  private String sizeChartCode;
  private List<Image> commonImages = new ArrayList();
  private List<ProductItemLogisticsRequest> productItemLogisticsRequests = new ArrayList();
  private List<ProductItemCreationRequest> productItemRequests = new ArrayList();
  private List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributes = new ArrayList();
  private boolean externalUser;
  private Map<String, String> distributionInfoRequest;
}
