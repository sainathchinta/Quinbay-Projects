package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.response.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductMasterDataUpdateRequest extends BaseDTORequest implements Serializable {
  @Serial
  private static final long serialVersionUID = 7035804051737759231L;
  private String productCode;
  private String productSku;
  private String name;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private byte[] description;
  private String url;
  private List<ProductImageEditRequest> commonImages = new ArrayList<>();
  private boolean postLive;
  private boolean reviewPending;
  private String businessPartnerCode;
  private String categoryCode;
  private PCBAddEditVideoRequest videoAddEditRequest;
  private String sizeChartCode;
  private boolean videoDelete;
}