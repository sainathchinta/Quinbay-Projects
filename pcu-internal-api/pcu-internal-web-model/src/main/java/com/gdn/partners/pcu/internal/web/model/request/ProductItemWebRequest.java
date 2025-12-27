package com.gdn.partners.pcu.internal.web.model.request;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 20/12/2018 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ProductItemWebRequest extends BaseWebRequest{

  private String id;
  private String generatedItemName;
  private String upcCode;
  private String skuCode;
  private boolean activated;
  private boolean viewable;
  private boolean contentChanged;
  private byte[] hash;
  List<ProductItemAttributeValueWebRequest> productItemAttributeValues;
  private List<ImageRequest> images;
  private Integer dangerousGoodsLevel;
  private Map<String, String> attributesMap;
  private boolean onlyVatChanged;
  private Boolean vatApplicable;
}
