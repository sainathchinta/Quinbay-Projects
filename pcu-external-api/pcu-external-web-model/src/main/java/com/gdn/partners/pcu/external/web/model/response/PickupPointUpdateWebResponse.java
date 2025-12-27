package com.gdn.partners.pcu.external.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointUpdateWebResponse {

  private List<PickupPointUpdateItemWebResponse> variantsErrorList = new ArrayList<>();
  private Long l3version;

  public PickupPointUpdateWebResponse(List<PickupPointUpdateItemWebResponse> variantsErrorList) {
    this.variantsErrorList = variantsErrorList;
  }
}
