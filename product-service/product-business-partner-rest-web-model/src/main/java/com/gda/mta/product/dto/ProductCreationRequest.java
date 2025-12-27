package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by Vishal on 22/06/18.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCreationRequest extends ProductRequest {
  private static final long serialVersionUID = -7236352811854366398L;
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
  private List<Image> commonImages = new ArrayList<>();
  private List<ProductItemLogisticsRequest> productItemLogisticsRequests = new ArrayList<>();
  private List<ProductItemCreationRequest> productItemRequests = new ArrayList<>();
  private List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributes =
      new ArrayList<>();
  private boolean externalUser;
  private Map<String, String> distributionInfoRequest;

  public ProductCreationRequest(String businessPartnerId, String businessPartnerCode, String businessPartnerName,
      String gdnProductSku, String categoryName, List<ProductItemCreationRequest> productItemRequests,
      List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributes, String oldProductCode,
      String oldProductRejectionNote) {
    this.businessPartnerId = businessPartnerId;
    this.businessPartnerCode = businessPartnerCode;
    this.businessPartnerName = businessPartnerName;
    this.gdnProductSku = gdnProductSku;
    this.categoryName = categoryName;
    this.productItemRequests = productItemRequests;
    this.productBusinessPartnerAttributes = productBusinessPartnerAttributes;
    this.oldProductCode = oldProductCode;
    this.oldProductRejectionNote = oldProductRejectionNote;
  }

  @Override
  public String toString() {
    final StringBuffer sb = new StringBuffer("ProductCreationRequest{");
    sb.append("productRequest='").append(super.toString()).append('\'');
    sb.append(",productCategories='").append(super.getProductCategories().toString()).append('\'');
    sb.append(",productAttributes='").append(super.getProductAttributes().toString()).append('\'');
    sb.append(",businessPartnerId='").append(businessPartnerId).append('\'');
    sb.append(", businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", businessPartnerName='").append(businessPartnerName).append('\'');
    sb.append(", gdnProductSku='").append(gdnProductSku).append('\'');
    sb.append(", categoryName='").append(categoryName).append('\'');
    sb.append(", oldProductCode='").append(oldProductCode).append('\'');
    sb.append(", oldProductRejectionNote='").append(oldProductRejectionNote).append('\'');
    sb.append(", forReview=").append(forReview);
    sb.append(", productCreationType=").append(productCreationType);
    sb.append(", containsUrlImage=").append(containsUrlImage);
    sb.append(", imagesUpdated=").append(imagesUpdated);
    sb.append(", productItemRequests=").append(productItemRequests);
    sb.append(", productBusinessPartnerAttributes=").append(productBusinessPartnerAttributes);
    sb.append(", preOrder=").append(preOrder);
    sb.append(", freeSample=").append(freeSample);
    sb.append(", off2OnChannelActive=").append(off2OnChannelActive);
    sb.append(", online=").append(online);
    sb.append(", bundleProduct").append(bundleProduct);
    sb.append(", sizeChartCode").append(sizeChartCode);
    sb.append(", externalUser").append(externalUser);
    sb.append(", distributionInfoRequest").append(distributionInfoRequest);
    sb.append('}');
    return sb.toString();
  }
}
