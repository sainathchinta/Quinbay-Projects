package com.gdn.mta.bulk.dto.product;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import com.gdn.mta.bulk.dto.WarningMessage;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductV2Request extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -312485438560348241L;

  private String requestId;
  private String username;
  private String bpCode;
  private String storeId;

  private String productCode;
  private String name;
  private String brand;
  private String url;
  private String categoryCode;

  private int productType;
  private String pickupPointCode;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;

  private byte[] description;
  private byte[] uniqueSellingPoint;
  private byte[] productStory;

  private Map<String, String> productNonDefiningAttributes;
  private Map<String, List<String>> productDefiningAttributes;
  private List<ProductItemV2Request> productItems;

  private List<String> images;

  private String familyColorCode;

  private List<WarningMessage> warningMessage;
}
