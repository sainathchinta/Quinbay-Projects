package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.dto.PickupPointDetailsDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class EanUpcPickupPointCodeRequest {
  private String eanUpcCode;
  private List<PickupPointDetailsDTO> pickupPointDetails;
}
