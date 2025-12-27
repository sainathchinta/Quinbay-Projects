package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductMasterDataUpdateRequest extends BaseDTORequest {
  private String productSku;
  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private byte[] description;
  private String url;
  private List<ProductImageEditRequest> commonImages = new ArrayList();
  private Boolean reviewPending;
  private String businessPartnerCode;
  private String categoryCode;
  private VideoAddEditRequest videoAddEditRequest;
  private String sizeChartCode;
  private boolean videoDelete;
}
