package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import java.util.List;

/**
 * Created by govind on 14/09/2017 AD.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleProductsAndItemsResponse extends BaseResponse{

  private static final long serialVersionUID = -255944134096957062L;
  private SimpleProductDTO simpleProductDTO;
  private List<SimpleItemDTO> simpleItemDTOList ;

  public SimpleProductDTO getSimpleProductDTO() {
    return simpleProductDTO;
  }

  public void setSimpleProductDTO(SimpleProductDTO simpleProductDTO) {
    this.simpleProductDTO = simpleProductDTO;
  }

  public List<SimpleItemDTO> getSimpleItemDTOList() {
    return simpleItemDTOList;
  }

  public void setSimpleItemDTOList(List<SimpleItemDTO> simpleItemDTOList) {
    this.simpleItemDTOList = simpleItemDTOList;
  }

  @Override
  public String toString() {
    return String
        .format(
            " SimpleProductsAndItemsResponse [simpleProductDTO=%s, simpleItemDTOList=%s, toString()=%s]",
            this.simpleProductDTO, this.simpleItemDTOList, super.toString());
  }
}
