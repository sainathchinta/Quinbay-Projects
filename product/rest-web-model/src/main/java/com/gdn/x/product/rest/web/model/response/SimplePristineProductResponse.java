package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleProductDTO;

import java.util.Set;

/**
 * Created by govind on 20/09/2017 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimplePristineProductResponse extends BaseResponse {

  private static final long serialVersionUID = -255944134096957062L;
  private String pristineId;
  private Set<SimpleProductDTO> simpleProductDTOList;

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public Set<SimpleProductDTO> getSimpleProductDTOList() {
    return simpleProductDTOList;
  }

  public void setSimpleProductDTOList(Set<SimpleProductDTO> simpleProductDTOList) {
    this.simpleProductDTOList = simpleProductDTOList;
  }

  @Override
  public String toString() {
    return String
        .format(
            "SimplePristineProductResponse [pristineId=%s, simpleProductDTOList=%s, toString()=%s]",
            this.pristineId, this.simpleProductDTOList, super.toString());
  }



}
