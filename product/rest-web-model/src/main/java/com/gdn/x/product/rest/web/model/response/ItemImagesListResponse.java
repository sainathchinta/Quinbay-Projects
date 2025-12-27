package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemImagesListResponse extends BaseResponse {

  private static final long serialVersionUID = -2044829721833197299L;
  private String itemSku;
  private List<ItemImagesResponse> imagesResponseList;
  private List<ProductAttributeDetailDTO> itemAttributeDetails;

  public ItemImagesListResponse(String itemSku, List<ItemImagesResponse> imagesResponseList) {
    this.itemSku = itemSku;
    this.imagesResponseList = imagesResponseList;
  }
}