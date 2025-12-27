package com.gda.mta.product.dto;

import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class ItemImageUpdateRequestPCB {
  private String productCode;
  private String businessPartnerCode;
  private boolean needCorrection;
  private List<ProductItemImageRequest> updatedProductItemImageRequestList;
  private List<ProductItemImageRequest> newProductItemImageRequestList;
  private List<LocationPathAndCommonImage> locationPathAndCommonImages;
  private String productSku;
}
