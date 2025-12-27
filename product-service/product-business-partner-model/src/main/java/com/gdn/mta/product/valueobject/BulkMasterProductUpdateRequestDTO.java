package com.gdn.mta.product.valueobject;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkMasterProductUpdateRequestDTO {

  private List<SimpleMasterProductUpdateRequestDTO> simpleMasterProductUpdateRequestDTOS = new ArrayList<>();

  public List<SimpleMasterProductUpdateRequestDTO> getSimpleMasterProductUpdateRequestDTOS() {
    return simpleMasterProductUpdateRequestDTOS;
  }

  public void setSimpleMasterProductUpdateRequestDTOS(List<SimpleMasterProductUpdateRequestDTO> simpleMasterProductUpdateRequestDTOS) {
    this.simpleMasterProductUpdateRequestDTOS = simpleMasterProductUpdateRequestDTOS;
  }

  public BulkMasterProductUpdateRequestDTO(List<SimpleMasterProductUpdateRequestDTO> simpleMasterProductUpdateRequestDTOS) {
    this.simpleMasterProductUpdateRequestDTOS = simpleMasterProductUpdateRequestDTOS;
  }
}
