package com.gdn.partners.pcu.external.web.model.request;

import java.util.List;

import org.springframework.util.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 13/12/2018 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductWebRequest {

  private String id;
  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private byte[] description;
  private byte[] longDescription;
  private String brand;
  private String brandCode;
  private String brandApprovalStatus;
  private String uniqueSellingPoint;
  private String uom;
  private List<ProductCategoryWebRequest> productCategories;
  private List<ProductAttributeWebRequest> productAttributes;
  private List<ProductItemWebRequest> productItems;
  private boolean activated;
  private boolean viewable;
  private String productStory;
  private String specificationDetail;
  private String url;
  private List<ImageRequest> images;
  private boolean promoSKU;
  private boolean imagesUpdated;
  private boolean freeSample;
  private boolean off2OnChannelActive;
  private boolean online;
}
